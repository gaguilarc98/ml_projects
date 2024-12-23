# -*- coding: utf-8 -*-
"""
Created on Mon Dec 25 18:35:18 2023

@author: 98acg
"""

import pandas as pd
import numpy as np
#%%____DUMMY VARIABLE
train  = pd.read_csv("http://bit.ly/kaggletrain")
print(train.head())
train['Sex_male'] = train.Sex.map({'female':0, 'male':1})
print(train.head())
#%%#A more flexible approach
print(pd.get_dummies(train.Sex)) #This uses one-hot encoding, that is one variable for each category
    
#%%READING FILES IN LOTES
mes = ["ene","feb","mar","abr"]
year = ['2023']
my = [m+y for m in mes for y in year]

pdFull = pd.DataFrame()
for i in range(0,len(my)):
    print(i)
    lot = pd.read_csv("C:/Files/Data/lotes/lot_"+my[i]+".csv", sep=";")
    lot[["CTACLIENTE","OPERACION"]] = lot.ID.str.split('-',expand=True)
    lot['CTACLIENTE'] = pd.to_numeric(lot['CTACLIENTE'], errors='coerce')#To coerce null values
    lot['OPERACION'] = pd.to_numeric(lot['OPERACION'], errors='coerce')#To coerce null values. Can only be done for Series not DataFrames
    #lot["ESTADO"] = lot.where(lot.CTACONT.isin(["131","135"]), "VIGENTE")
    pdFull = pd.concat([pdFull, lot], axis=0)
    
pd_grouped = pdFull.groupby(['CTACLIENTE','OPERACION'])
pdFull.reset_index(drop=True,inplace =True)
pdFull['MinSaldo'] = pd_grouped['SALDO'].transform('min')
pdFull['FMinSaldo'] = pdFull['FECHA'].loc[pd_grouped['SALDO'].transform('idxmin')].values
#%%