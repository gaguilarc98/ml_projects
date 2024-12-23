# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
#%%
"""
Created on Fri Aug 23 08:37:50 2019

@author: ahmed
"""
%reset -f
import xlrd
import pandas as pd
import matplotlib.pyplot as plt
plt.style.use('seaborn-whitegrid')
import numpy as np
#%%
df = pd.read_csv('C:/!bso/Cargamensual_infocred/BSO_202303_Datos_Adicionales.txt', delimiter='|')

#df.to_csv(r'D:/!bso/BSO202207_ab_v2.txt', header=None, index=None, sep='|', mode='a')

#df.Rectificacion.dtypes
#%%
df2 = df.copy()

df2['flag'] = (df2['Rectificacion'].str.len() >= 200)

df3 = df2[df2.flag]

df2.loc[df2['flag'] == True , 'Rectificacion'] = ''
df2['flag'] = (df2['Rectificacion'].str.len() >= 200)

df4 = df2[df2.flag]
df2 = df2.drop('flag', axis = 1)
#%%
df2 = df.copy()
df2['Rectificacion'] = 'rectificacion'
df2['DIRECCION'] = 'direccion'
df2['DiasMora'] = df2['DiasMora'].fillna(0)
df2['DiasMora'] = df2['DiasMora'].astype(int)
pos = [129415, 129416, 386282, 386283]
df2.drop(df2.index[pos], inplace=True)
df2.to_csv(r'C:/!bso/Cargamensual_infocred/utf/BSO202303_utf8.txt', header=True, index=None, 
sep='|', mode='w', encoding='utf-8')
df2.to_csv(r'C:/!bso/Cargamensual_infocred/ansi/BSO202303_ansi.txt', header=True, index=None, 
sep='|', mode='w', encoding='ansi')

