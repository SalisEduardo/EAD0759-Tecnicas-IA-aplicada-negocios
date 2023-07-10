library(tidyverse)
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(fpc)
library(readxl)
library(lubridate)
library(dbscan)
library(NbClust)
library(dplyr)
library(corrplot)
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(stringr)
library(tidyr)
library(GGally)
library(caret)


# Data Prep -------------------------------------------------------------------------------

path_FIA <- "dados/FIAS_BRASIL_v3.xlsx"

FIAs <- readxl::read_excel(path_FIA)

Encoding(FIAs$nome) <- "latin-1"

FIAs <- FIAs |> 
  dplyr::mutate(data =as.Date(data)) |>
  dplyr::mutate(inicio =as.Date(inicio)) |>
  dplyr::mutate(idade_fundo =  difftime(data, inicio, units = "days")/365)  |>
  dplyr::mutate(nome = gsub("A\U000fffc7\U000fffd5ES","ACOES",nome))




  
FIAs_quarters <- FIAs |>
  group_by(quarter = quarter(data, with_year = TRUE)) |>
  filter(data == max(data)) |>
  ungroup()




start_date <- FIAs$data |> min()
end_date <- FIAs$data |> max()

bova11 <- quantmod::getSymbols("BOVA11.SA",from = start_date, to = end_date,auto.assign = FALSE)

bova11_Ad <- bova11 |> 
  quantmod::Ad()

bova11_returns <- Return.calculate(bova11_Ad,method ='discrete') |> 
  na.omit()  |> 
  as.data.frame() |>
  dplyr::rename("ret_01d" = BOVA11.SA.Adjusted) |>
  dplyr::mutate('nome' = "BOVA11")

bova11_returns$data <- as.Date(rownames(bova11_returns) ) 
rownames(bova11_returns) <- NULL


bova11_returns <- bova11_returns[,c("data","nome",'ret_01d')]

bova11_returns$ret_01d <- bova11_returns$ret_01d * 100 


get_cummulative_returns <- function(dailyRets){
  cum_rets <- cumprod(1 + dailyRets) - 1
  return(cum_rets)
}



FIAS_bova_returns <- FIAs |>
  dplyr::select(data,nome,ret_01d) |>
  rbind(bova11_returns) |>
  dplyr::arrange(data) |>
  dplyr::group_by(nome) |>
  dplyr::mutate(ret_01d = ret_01d/100 ) |>
  dplyr::mutate(cummulativeReturn = get_cummulative_returns(ret_01d)) |> 
  dplyr::ungroup() |>
  dplyr::mutate("is_benchmark" =  ifelse(nome=='BOVA11',"Benchmark","Fundo"))

# EDA-------------------------------------------------------------------------------



## Performance Funds x ibov

ano_inicio_fias <- FIAs |>
  dplyr::mutate(ano_inicio = year(inicio))|>
  dplyr::select(nome,ano_inicio) |>
  dplyr::distinct() 

ano_inicio_fias |>
  ggplot(aes(x=ano_inicio)) +
  geom_bar()

ano_inicio_fias |>
  count(ano_inicio) |>
  dplyr::mutate(cumsum(n)) |>
  view()


plot_cummulative_rets <- FIAS_bova_returns |>
  ggplot(aes(x=data,y=cummulativeReturn,color=is_benchmark,alpha=is_benchmark)) + 
  geom_line() +
  scale_color_manual(values = c("Bechmark" = "blue", "Fundo" = "lightgreen")) +
  scale_alpha_manual(values = c("Bechmark" = 1, "Fundo" = 0.5)) +
  theme(legend.position = "none")
plot_cummulative_rets


### KPIs
wide_daily_rets <- FIAS_bova_returns |>
  pivot_wider(id_cols = data,names_from = nome,values_from = ret_01d)


daily_rets_time_series <- xts(wide_daily_rets[,-1],order.by = wide_daily_rets$data) 


annRets <- daily_rets_time_series |>
  PerformanceAnalytics::table.AnnualizedReturns() |> 
  t()


col_bova11 <- which(colnames(daily_rets_time_series) == "BOVA11")

corr_with_BOVA11 <- daily_rets_time_series[,-col_bova11] |> 
  table.Correlation(Rb=daily_rets_time_series$BOVA11) 

ddRatios <- daily_rets_time_series |>
  PerformanceAnalytics::table.DrawdownsRatio() |>
  t() 

downSideRisk <- daily_rets_time_series |>
  PerformanceAnalytics::table.DownsideRisk() |>
  t() 

distribs <- daily_rets_time_series |>
  PerformanceAnalytics::table.Distributions() |>
  t() 


kpis_funds_bova <- cbind(annRets,ddRatios,downSideRisk,distribs) |>
  as.data.frame()

kpis_funds_bova$nome <-rownames(kpis_funds_bova)
rownames(kpis_funds_bova) <- NULL

kpis_funds_bova <- cbind(kpis_funds_bova['nome'], kpis_funds_bova[,-dim(kpis_funds_bova)[2]]) 


fundos_inicio <- FIAs[,c('nome',"inicio")] |> 
  distinct() |>
  dplyr::arrange(nome) |>
  dplyr::mutate()

last_PL <- FIAs |>
  dplyr::group_by(nome) |>
  arrange(desc(data)) |>
  slice(1) |>
  select(nome, PL) 

idade_fundo <- FIAs |>
  dplyr::group_by(nome) |>
  arrange(desc(data)) |>
  slice(1) |>
  mutate(idade_atual = (as.numeric(data  -inicio))/365) |>
  dplyr::select(nome,idade_atual) |>
  distinct()

kpis_funds_bova_infos <- kpis_funds_bova |> 
  dplyr::left_join(fundos_inicio,by='nome') |>
  dplyr::left_join(last_PL,by='nome') |>
  dplyr::left_join(idade_fundo,by='nome')


kpis_funds_bova_infos |>
  dplyr::mutate("is_benchmark" =  ifelse(nome=='BOVA11',"Benchmark","Fundo")) |>
  ggplot(aes(y=`Annualized Return`,x=`Annualized Std Dev`)) +
  geom_point(aes(color=is_benchmark)) 
  

kpis_funds_bova_infos |>
  dplyr::mutate("is_benchmark" =  ifelse(nome=='BOVA11',"Benchmark","Fundo")) |>
  ggplot(aes(y=`Annualized Return`,x=`Annualized Std Dev`)) +
  geom_point(aes(size=`Annualized Sharpe (Rf=0%)`,color=PL))  +
  geom_vline(xintercept =  median(kpis_funds_bova_infos[,"Annualized Std Dev"]),size = 1.5, linetype = "dashed",color = "darkblue")+
  
  geom_hline(yintercept =  median(kpis_funds_bova_infos[,"Annualized Return"]),size = 1.5, linetype = "dashed",color = "darkblue")


kpis_funds_bova_infos |>
  dplyr::mutate("is_benchmark" =  ifelse(nome=='BOVA11',"Benchmark","Fundo")) |>
  ggplot(aes(y=`Annualized Return`,x=`Annualized Std Dev`)) +
  geom_point(aes(size=`Annualized Sharpe (Rf=0%)`,color=PL))  +
  geom_vline(xintercept =  median(kpis_funds_bova_infos[,"Annualized Std Dev"]),size = 1.5, linetype = "dashed",color = "darkblue")+
  geom_hline(yintercept =  median(kpis_funds_bova_infos[,"Annualized Return"]),size = 1.5, linetype = "dashed",color = "darkblue")



kpis_funds_bova_infos |>
  dplyr::mutate("is_benchmark" =  ifelse(nome=='BOVA11',"Benchmark","Fundo")) |>
  ggplot(aes(y=`Annualized Return`,x=`Annualized Std Dev`)) +
  geom_point(aes(color=is_benchmark,size=`Annualized Sharpe (Rf=0%)`))  +
  geom_vline(xintercept =  median(kpis_funds_bova_infos[,"Annualized Std Dev"]),size = 1.5, linetype = "dashed",color = "darkblue")+
  geom_hline(yintercept =  median(kpis_funds_bova_infos[,"Annualized Return"]),size = 1.5, linetype = "dashed",color = "darkblue")


### Histogrmans of KPIs

numeric_vars <- c("Annualized Return", "Annualized Std Dev",  "Annualized Sharpe (Rf=0%)","Maximum Drawdown",
                  "Historical VaR (95%)", "Historical ES (95%)" , "Modified VaR (95%)" , 
                  "Modified ES (95%)" , "daily  Std Dev","Skewness","Kurtosis",
                  "Excess kurtosis")

### Summary stats of kpis

selectd_kpis <- kpis_funds_bova_infos[,numeric_vars] |>drop_na()

summary_df <- data.frame(
  #Column = colnames(selectd_kpis),
  Mean = colMeans(selectd_kpis),
  Median = sapply(selectd_kpis, median),
  Max = sapply(selectd_kpis, max),
  Min = sapply(selectd_kpis, min),
  Q1 = sapply(selectd_kpis, quantile, probs = 0.25),
  Q3 = sapply(selectd_kpis, quantile, probs = 0.75)
)

# Print the summary dataframe
summary_df

# Create a basic histogram for each numeric variable
histograms <- lapply(numeric_vars, function(var) {
  ggplot(kpis_funds_bova_infos, aes(x = !!sym(var))) +
    geom_histogram( fill = "skyblue", color = "black") +
    ggtitle(var) +
    theme_minimal()
})


perf_ibov <- kpis_funds_bova_infos |> 
  dplyr::filter(nome == 'BOVA11')

overperf_ibov <- kpis_funds_bova_infos |>
  dplyr::filter(`Annualized Return` > perf_ibov[1,'Annualized Return']) 
 
lowerVol_ibov <- kpis_funds_bova_infos |>
  dplyr::filter(`Annualized Std Dev` < perf_ibov[1,'Annualized Std Dev']) 

lowerVaR_ibov <- kpis_funds_bova_infos |>
  dplyr::filter(`Historical VaR (95%)` < perf_ibov[1,'Historical VaR (95%)']) 

lowerMDD <- kpis_funds_bova_infos |>
  dplyr::filter(`Maximum Drawdown` < perf_ibov[1,'Maximum Drawdown']) 

better_sharp <- kpis_funds_bova_infos |>
  dplyr::filter(`Annualized Sharpe (Rf=0%)` > perf_ibov[1,'Annualized Sharpe (Rf=0%)']) 

n_funds <- FIAs$nome |> unique() |> length()




tab_bova11_comparison <- data.frame(
  Status = c("Better Returns","Lower Vol","Better Sharp","Lower VaR","Lower MDD"),
  Number_of_Funds = c(dim(overperf_ibov)[1] , dim(lowerVol_ibov)[1], 
                      dim(better_sharp)[1], dim(lowerVaR_ibov)[1], dim(lowerMDD)[1])
  )


tab_bova11_comparison$Percentage_of_funds <-   round(tab_bova11_comparison$Number_of_Funds/n_funds,4)

tab_bova11_comparison





# Feature selection -------------------------------------------------------------------------------


FIAs_quarters |>
  dplyr::select(-c(data,nome,inicio,Cota,ret_01d,quarter)) |>
  dplyr::mutate_all(as.numeric) |>
  cor() |>
  corrplot::corrplot(type = 'upper', method = "color",diag=FALSE, tl.col = "black", title = "Cross Section Correlation of the Features")
  


cutoff_correlated <- FIAs_quarters |>
  dplyr::select(-c(data,nome,inicio,Cota,ret_01d)) |>
  dplyr::mutate_all(as.numeric) |>
  cor() |>
  findCorrelation( cutoff = 0.7) 

columns_features <- FIAs_quarters |>
  dplyr::select(-c(data,nome,inicio,Cota,ret_01d,quarter))  |>
  dplyr::select(-cutoff_correlated) |>
  colnames()



FIAs_quarters$idade_fundo = as.numeric(FIAs_quarters$idade_fundo)


# Modeling - Cluster on a specific date-------------------------------------------------------------------------------

#selected_date <- max(FIAs_quarters$data)
selected_date <- "2022-12-30"

cut_date_FIAs_quarters <- FIAs_quarters  |>
  dplyr::filter(data == selected_date) 

cut_date_FIAs_quarters_features <- cut_date_FIAs_quarters  |>
  dplyr::select(all_of(columns_features)) 

pca_cut_date_FIAs_quarters <- cut_date_FIAs_quarters_features|>
  prcomp(scale=TRUE)
  

# Get the variance explained by each principal component
variance_explained <- pca_cut_date_FIAs_quarters$sdev^2 / sum(pca_cut_date_FIAs_quarters$sdev^2)

# Create a data frame for the plot
variance_df <- data.frame(
  Principal_Component = paste0("PC", 1:length(variance_explained)),
  Variance_Explained = variance_explained
)

# Plot the explained variance
ggplot(variance_df, aes(x = Principal_Component, y = Variance_Explained)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Principal Component", y = "Variance Explained") +
  ggtitle("Explained Variance by Principal Component") +
  theme_minimal()

variance_df$Cummulative_Variance_Explained <- cumsum(variance_df$Variance_Explained)

variance_df

# Extract the two principal components
pc1 <- pca_cut_date_FIAs_quarters$x[, 1]
pc2 <- pca_cut_date_FIAs_quarters$x[, 2]
pc3 <- pca_cut_date_FIAs_quarters$x[, 3]
pc4 <- pca_cut_date_FIAs_quarters$x[, 4]

# Create a new dataframe with the principal components and target variable
df_pca_two_dim <- data.frame(PC1 = pc1, PC2 = pc2)

# Plot the dataframe using the principal components
ggplot(df_pca_two_dim, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(x = "Principal Component 1", y = "Principal Component 2") +
  ggtitle("Data Plot in Principal Component Space") +
  theme_minimal()

# Estimating the optimal number of clusters 

df_pca_cluster <- data.frame(PC1 = pc1, PC2 = pc2,PC3 = pc3,PC4 = pc4)

# k = 4 or 6   wss euclidian

fviz_nbclust(df_pca_cluster, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

# k = 2   silhouette euclidian

fviz_nbclust(df_pca_cluster, kmeans, method = "silhouette") 


#  k = 7   wss pearson

fviz_nbclust(df_pca_cluster, kmeans, method = "wss",diss = get_dist(df_pca_cluster, method = "pearson")) +
  geom_vline(xintercept = 7, linetype = 2)

#  k = 2 silhouette pearson

fviz_nbclust(df_pca_cluster, kmeans, method = "silhouette",diss = get_dist(df_pca_cluster, method = "pearson")) 

selected_k  = 3 
funds_kmeans <- kmeans(df_pca_cluster, selected_k, iter.max = 10, nstart = 1)

print(funds_kmeans)

aggregate(cut_date_FIAs_quarters_features, by=list(cluster=funds_kmeans$cluster), mean)


## Performance ex - post 


funds_by_cluster<- data.frame(
  nome = cut_date_FIAs_quarters$nome,
  cluster = funds_kmeans$cluster
)


nomes_fias1 <- funds_by_cluster |>
  dplyr::filter(cluster == 1) |>
  dplyr::pull(nome) |>
  unique()

nomes_fias2 <- funds_by_cluster |>
  dplyr::filter(cluster == 2) |>
  dplyr::pull(nome) |>
  unique()

nomes_fias3 <- funds_by_cluster |>
  dplyr::filter(cluster == 3) |>
  dplyr::pull(nome) |>
  unique()


paste("Tamanho do cluster 1:",length(nomes_fias1), "fundos") 
paste("Tamanho do cluster 2:",length(nomes_fias2), "fundos") 
paste("Tamanho do cluster 3:",length(nomes_fias3), "fundos") 


cluster1_returns_expost <- daily_rets_time_series['2023-01-01/',nomes_fias1]
cluster2_returns_expost <- daily_rets_time_series['2023-01-01/',nomes_fias2]
cluster3_returns_expost <- daily_rets_time_series['2023-01-01/',nomes_fias3]

cluster1_returns_expost_mean <-  cluster1_returns_expost |> 
  as.matrix() |>
  apply(1, mean) |>
  xts(order.by = index(cluster1_returns_expost)) |>
  setNames("Cluster1")

cluster2_returns_expost_mean <-  cluster2_returns_expost |> 
  as.matrix() |>
  apply(1, mean) |>
  xts(order.by = index(cluster2_returns_expost)) |>
  setNames("Cluster2")

cluster3_returns_expost_mean <-  cluster3_returns_expost |> 
  as.matrix() |>
  apply(1, mean) |>
  xts(order.by = index(cluster3_returns_expost)) |>
  setNames("Cluster3")

all_cluster_means <- do.call(merge.xts, 
                             list(cluster1_returns_expost_mean,
                                  cluster2_returns_expost_mean,cluster3_returns_expost_mean,
                                  daily_rets_time_series['2023-01-01/','BOVA11']))  |> na.omit()

all_cluster_means_cumrets <- get_cummulative_returns(all_cluster_means)

all_cluster_means_cumrets |>
  data.frame() |>
  rownames_to_column(var = "Index") |>
  gather(key = "Group", value = "CumReturn", -Index) |>
  dplyr::mutate(Index = as.Date(Index)) |>
  ggplot(aes(x = Index, y = CumReturn, color = Group, group = Group)) +
  geom_line() +
  labs(x = 'Data', y = "Retorno acumulado")

all_cluster_means |> 
  cor() |>
  corrplot::corrplot(type = 'lower', method = "color",diag=FALSE, tl.col = "black")




