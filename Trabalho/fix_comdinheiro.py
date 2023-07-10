import pandas as pd
import numpy as np
from unidecode import unidecode
import hashlib
import os


diretorio_fundos = [file for file in os.listdir('dados/comdinheiro_fundos') if file.endswith(".xlsx")]

dfs_fundos = [pd.read_excel("dados/comdinheiro_fundos/" + f) for f in diretorio_fundos]

fias_brasil = pd.concat(dfs_fundos)
fias_brasil.dropna(inplace=True)

fias_brasil['data'] = pd.to_datetime(fias_brasil['data'],format='%d/%m/%Y') 
fias_brasil['inicio'] = pd.to_datetime(fias_brasil['inicio'],format='%d/%m/%Y') 
for col in fias_brasil.iloc[:,3:].columns:
    fias_brasil[col]=fias_brasil[col].astype("str").str.replace(',', '.').astype("float")

#fias_brasil.to_excel("dados/FIAS_BRASIL.xlsx",index=False)

fias_brasil['nome'] =  fias_brasil['nome'].apply(lambda x: unidecode(x))
fias_brasil['nome'] = fias_brasil['nome'].apply(lambda x: hashlib.sha256(x.encode()).hexdigest()) 
fias_brasil.to_excel("dados/FIAS_BRASIL_v3.xlsx",index=False)
