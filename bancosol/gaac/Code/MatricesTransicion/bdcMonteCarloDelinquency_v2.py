# -*- coding: utf-8 -*-
"""
Created on Mon Aug  1 11:11:29 2022

@author: aeid
"""

#%%

import datetime
import sys
from timeit import repeat
from tracemalloc import start
import pandas as pd
import numpy as np
import glob
import seaborn as sns
import matplotlib.pyplot as plt
import os
print(sys.version, sys.platform, sys.executable)
pd.options.display.float_format = '{:,.3f}'.format


# bdc = pd.read_csv('D:/OneDrive - Banco Solidario S.A/bdc/BaseCarteraJul2022.txt', 
#                   delimiter='|', encoding='latin-1')

# bdc = pd.read_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCarteraAgo2022.txt', 
#                    delimiter='|', encoding='latin-1')

bdc = pd.read_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCarteraMar2023.txt', delimiter='|', encoding='latin-1')
#%%
# Preparing dataset
bdcp = bdc[['CTACLIENTE', 'OPERACION', 'CALIFICACION', 'TIPO_CREDITO',
             'SALDO', 'PREVCONST', 'MODULO', 'ESTADO',
            'DIASMORA', 'MONEDA', 'FVEN_PROXPAGO']].set_index('OPERACION', drop=True)
bdcp = bdcp.loc[bdcp['ESTADO'] != 'CASTIGADA']
bdcp = bdcp.loc[bdcp['MODULO'] != 131]
bdcp = bdcp.loc[bdcp['FVEN_PROXPAGO'] != '/  /']
bdcp['saldous'] = np.where(bdcp['MONEDA'] == 0, bdcp['SALDO']/6.86, bdcp['SALDO'])
bdcp['previus'] = np.where(bdcp['MONEDA'] == 0, bdcp['PREVCONST']/6.86, bdcp['PREVCONST'])
bdcp['proxpago'] = pd.to_datetime(bdcp['FVEN_PROXPAGO'], dayfirst=True)
print(bdcp.saldous.sum())
print(bdcp.head())
print(bdcp.dtypes)
#%%
# Slicing current delinquents
dfdel = bdcp.loc[bdcp['DIASMORA'] > 0]
prevToday = dfdel['previus'].sum()
delToday = dfdel['saldous'].sum()

# %%
# Storing provision and delinquency from new delinquents
newDel = bdcp.loc[bdcp['DIASMORA'] == 0 ]
newDel['mpay'] = newDel['proxpago'].dt.month
newDel['myear'] = newDel['proxpago'].dt.year
newDel = newDel.loc[(newDel['mpay'] == 9) & (newDel['myear'] == 2022)]
# #%%
# # Raw Monte Carlo
# pdList = []
# propList = [0.007, 0.017, 0.03]
# closeList = ['2022-09-30', '2022-10-31', '2022-11-30']

# for l in range(len(closeList)) :
#     for k in range(len(propList)):
#         for i in range(50):
#             print(i)
#             prop = propList[k]
#             dateClose = pd.to_datetime(closeList[l])
#             dfs = newDel.sample(n = round(prop * len(newDel)))
#             dfs['diasMoraPot'] = dateClose - dfs['proxpago']
#             dfs['diasMoraPot'] = dfs['diasMoraPot'].dt.days
#             dfs['tipoCred'] = dfs['TIPO_CREDITO'].str[0]
#             dfs['tipoCred2'] = dfs['TIPO_CREDITO'].str[:2]
#             dfs['califPot'] = np.where(dfs['diasMoraPot'] <= 5, 'A', 
#                             np.where((dfs['diasMoraPot'] > 5) & (dfs['diasMoraPot'] <= 30), 'B', 
#                             np.where((dfs['diasMoraPot'] > 30) & (dfs['diasMoraPot'] <= 55), 'C', 
#                             np.where((dfs['diasMoraPot'] > 55) & (dfs['diasMoraPot'] <= 75), 'D', 
#                             np.where((dfs['diasMoraPot'] > 75) & (dfs['diasMoraPot'] <= 90), 'E', 
#                             np.where(dfs['diasMoraPot'] > 90, 'F',
#                             np.where((dfs['TIPO_CREDITO'].str[0] == 'H') & (dfs['diasMoraPot'] <= 30), 'A', 
#                             np.where((dfs['TIPO_CREDITO'].str[0] == 'H') & (dfs['diasMoraPot'] > 30) & (dfs['diasMoraPot'] <= 90), 'B', 
#                             np.where((dfs['TIPO_CREDITO'].str[0] == 'H') & (dfs['diasMoraPot'] > 90) & (dfs['diasMoraPot'] <= 180), 'C', 
#                             np.where((dfs['TIPO_CREDITO'].str[0] == 'H') & (dfs['diasMoraPot'] > 180) & (dfs['diasMoraPot'] <= 270), 'D', 
#                             np.where((dfs['TIPO_CREDITO'].str[0] == 'H') & (dfs['diasMoraPot'] > 270) & (dfs['diasMoraPot'] <= 360), 'E', 
#                             np.where((dfs['TIPO_CREDITO'].str[0] == 'H') & (dfs['diasMoraPot'] > 360), 'F', 
#                             np.where((dfs['TIPO_CREDITO'].str[:2] == 'M9') & (dfs['diasMoraPot'] <= 20), 'A', 
#                             np.where((dfs['TIPO_CREDITO'].str[:2] == 'M9') & (dfs['diasMoraPot'] > 20) & (dfs['diasMoraPot'] <= 30), 'B', 
#                             np.where((dfs['TIPO_CREDITO'].str[:2] == 'M9') & (dfs['diasMoraPot'] > 30) & (dfs['diasMoraPot'] <= 55), 'C', 
#                             np.where((dfs['TIPO_CREDITO'].str[:2] == 'M9') & (dfs['diasMoraPot'] > 55) & (dfs['diasMoraPot'] <= 75), 'D', 
#                             np.where((dfs['TIPO_CREDITO'].str[:2] == 'M9') & (dfs['diasMoraPot'] > 75) & (dfs['diasMoraPot'] <= 90), 'E', 
#                             np.where((dfs['TIPO_CREDITO'].str[:2] == 'M9') & (dfs['diasMoraPot'] > 90), 'F',  
#                             'Other'))))))))))))))))))
#             dfs['maxCalif'] = dfs.groupby('CTACLIENTE')['califPot'].transform('max')
#             dfs['califPot'] = dfs['maxCalif']
#             dfs['severity'] = np.where((dfs['CALIFICACION'] == 'B') & (dfs['califPot'] == 'C') & (dfs['MONEDA'] == 0) , 8,
#                               np.where((dfs['CALIFICACION'] == 'B') & (dfs['califPot'] == 'D') & (dfs['MONEDA'] == 0) , 20,
#                               np.where((dfs['CALIFICACION'] == 'B') & (dfs['califPot'] == 'E') & (dfs['MONEDA'] == 0) , 32,
#                               np.where((dfs['CALIFICACION'] == 'B') & (dfs['califPot'] == 'F') & (dfs['MONEDA'] == 0) , 40,
#                               np.where((dfs['CALIFICACION'] == 'C') & (dfs['califPot'] == 'D') & (dfs['MONEDA'] == 0) , 2.5,
#                               np.where((dfs['CALIFICACION'] == 'C') & (dfs['califPot'] == 'E') & (dfs['MONEDA'] == 0) , 4,
#                               np.where((dfs['CALIFICACION'] == 'C') & (dfs['califPot'] == 'F') & (dfs['MONEDA'] == 0) , 5,
#                               np.where((dfs['CALIFICACION'] == 'D') & (dfs['califPot'] == 'E') & (dfs['MONEDA'] == 0) , 1.6,
#                               np.where((dfs['CALIFICACION'] == 'D') & (dfs['califPot'] == 'F') & (dfs['MONEDA'] == 0) , 2,
#                               np.where((dfs['CALIFICACION'] == 'E') & (dfs['califPot'] == 'F') & (dfs['MONEDA'] == 0) , 1.25,
#                               np.where((dfs['CALIFICACION'] == 'A') & (dfs['califPot'] == 'B') & (dfs['MONEDA'] != 0) , 2,
#                               np.where((dfs['CALIFICACION'] == 'A') & (dfs['califPot'] == 'C') & (dfs['MONEDA'] != 0) , 8,
#                               np.where((dfs['CALIFICACION'] == 'A') & (dfs['califPot'] == 'E') & (dfs['MONEDA'] != 0) , 20,
#                               np.where((dfs['CALIFICACION'] == 'A') & (dfs['califPot'] == 'F') & (dfs['MONEDA'] != 0) , 32,
#                               np.where((dfs['CALIFICACION'] == 'A') & (dfs['califPot'] == 'D') & (dfs['MONEDA'] != 0) , 40,
#                               np.where((dfs['CALIFICACION'] == 'B') & (dfs['califPot'] == 'C') & (dfs['MONEDA'] != 0) , 4,
#                               np.where((dfs['CALIFICACION'] == 'B') & (dfs['califPot'] == 'D') & (dfs['MONEDA'] != 0) , 10,
#                               np.where((dfs['CALIFICACION'] == 'B') & (dfs['califPot'] == 'E') & (dfs['MONEDA'] != 0) , 16,
#                               np.where((dfs['CALIFICACION'] == 'B') & (dfs['califPot'] == 'F') & (dfs['MONEDA'] != 0) , 16,
#                               np.where((dfs['CALIFICACION'] == 'C') & (dfs['califPot'] == 'D') & (dfs['MONEDA'] != 0) , 2.5,
#                               np.where((dfs['CALIFICACION'] == 'C') & (dfs['califPot'] == 'E') & (dfs['MONEDA'] != 0) , 4,
#                               np.where((dfs['CALIFICACION'] == 'C') & (dfs['califPot'] == 'F') & (dfs['MONEDA'] != 0) , 5,
#                               np.where((dfs['CALIFICACION'] == 'D') & (dfs['califPot'] == 'E') & (dfs['MONEDA'] != 0) , 1.6,
#                               np.where((dfs['CALIFICACION'] == 'D') & (dfs['califPot'] == 'F') & (dfs['MONEDA'] != 0) , 2,
#                               np.where((dfs['CALIFICACION'] == 'E') & (dfs['califPot'] == 'F') & (dfs['MONEDA'] != 0) , 1.25,
#                               1))))))))))))))))))))))))) 
#             dfs['newPrev'] = dfs['severity'] * dfs['previus']
#             dfs['newPrev'] = np.where((dfs['CALIFICACION'] == 'A') & (dfs['califPot'] == 'B') & (dfs['MONEDA'] == 0) , dfs['saldous']*0.025,
#                              np.where((dfs['CALIFICACION'] == 'A') & (dfs['califPot'] == 'C') & (dfs['MONEDA'] == 0) , dfs['saldous']*0.2,
#                              np.where((dfs['CALIFICACION'] == 'A') & (dfs['califPot'] == 'D') & (dfs['MONEDA'] == 0) , dfs['saldous']*0.5,
#                              np.where((dfs['CALIFICACION'] == 'A') & (dfs['califPot'] == 'E') & (dfs['MONEDA'] == 0) , dfs['saldous']*0.8,
#                              np.where((dfs['CALIFICACION'] == 'A') & (dfs['califPot'] == 'F') & (dfs['MONEDA'] == 0) , dfs['saldous']*1,
#                             dfs['newPrev'])))))

#             newMora = dfs.loc[dfs['diasMoraPot'] > 30]['saldous'].sum()
#             newPrev = dfs['newPrev'].sum()
#             store = pd.DataFrame(columns=('newMora', 'newPrev', 'prop', 'dateClose'), index=[0])
#             store['newMora'][0] = newMora
#             store['newPrev'][0] = newPrev
#             store['prop'][0] = prop
#             store['dateClose'][0] = closeList[l]
#             pdList.append(store)
#         #print(dfs.head())
#         #print(dfs.dtypes)
# #%%
# pd.options.display.float_format = '{:,.3f}'.format
# pd30 = pd.concat(pdList, axis = 0).reset_index()
# pd30['newPrev'] = pd30['newPrev'].astype('float')
# pd30['newMora'] = pd30['newMora'].astype('float')
# pd30['prop'] = pd30['prop'].astype('float')
# #%%
# gph = pd30[(pd30['prop'] == 0.03) & (pd30['dateClose'] == '2022-11-30')]
# gph.newPrev.plot.density(color='green')

# %%
# Monte Carlo based on transition matrices
# Transition parameters
# pd.options.display.float_format = '{:,.3f}'.format
# tmExp = pd.read_csv('D:/!bso/transMat/tmExpZ_12sens20.csv')
# draws = pd.DataFrame(np.random.rand(len(bdcp), 1), columns=['draw'], index=bdcp.index)
# ndDraw = pd.concat([bdcp, draws], axis = 1)
# ndDraw['draw'] = ndDraw['draw']*100
# print(tmExp)

# #%%
# # Monthly transition
# tlist = []
# for i in range(20): #máximo 1000
#     print(i)
#     ndDraw['califPot'] = np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] <= 99.87), 'A',
#                         np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > 99.87) & (ndDraw['draw'] <= 99.99), 'B',  
#                         np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > 99.99) & (ndDraw['draw'] <= 100), 'C',
#                         np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] <= 17.60), 'A',
#                         np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > 17.60) & (ndDraw['draw'] <= 27.24), 'B',  
#                         np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > 27.24) & (ndDraw['draw'] <= 81.43), 'C',
#                         np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > 81.43) & (ndDraw['draw'] <= 100), 'D',
#                         np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] <= 3.78), 'A',
#                         np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > 3.78) & (ndDraw['draw'] <= 4.12), 'B',  
#                         np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > 4.12) & (ndDraw['draw'] <= 8.93), 'C',
#                         np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > 8.93) & (ndDraw['draw'] <= 29.55), 'D',
#                         np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > 29.55) & (ndDraw['draw'] <= 100), 'E',
#                         np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] <= 5.81), 'A',
#                         np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > 5.81) & (ndDraw['draw'] <= 16.28), 'D',  
#                         np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > 16.28) & (ndDraw['draw'] <= 95.35), 'E',
#                         np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > 95.35) & (ndDraw['draw'] <= 100), 'F',
#                         np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] <= 3.65), 'A',
#                         np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > 3.65) & (ndDraw['draw'] <= 3.98), 'B',  
#                         np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > 3.98) & (ndDraw['draw'] <= 4.64), 'E',
#                         np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > 4.64) & (ndDraw['draw'] <= 100), 'F',
#                         np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] <= 0.10), 'A',
#                         np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > 0.10) & (ndDraw['draw'] <= 0.11), 'C',  
#                         np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > 0.11) & (ndDraw['draw'] <= 100), 'F',
#                         ndDraw['CALIFICACION'])))))))))))))))))))))))
#     ndDraw['maxCalif'] = ndDraw.groupby('CTACLIENTE')['califPot'].transform('max')
#     ndDraw['califPot'] = ndDraw['maxCalif']
#     ndDraw['severity'] = np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 8,
#                                 np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 20,
#                                 np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 32,
#                                 np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 40,
#                                 np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 2.5,
#                                 np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 4,
#                                 np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 5,
#                                 np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 1.6,
#                                 np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 2,
#                                 np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 1.25,
#                                 np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 2,
#                                 np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 8,
#                                 np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 20,
#                                 np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 32,
#                                 np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 40,
#                                 np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 4,
#                                 np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 10,
#                                 np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 16,
#                                 np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 16,
#                                 np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 2.5,
#                                 np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 4,
#                                 np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 5,
#                                 np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 1.6,
#                                 np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 2,
#                                 np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 1.25,
#                                 1))))))))))))))))))))))))) 
#     ndDraw['newPrev'] = ndDraw['severity'] * ndDraw['previus']
#     ndDraw['newPrev'] = np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.025,
#                                 np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.2,
#                                 np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.5,
#                                 np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.8,
#                                 np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*1,
#                                 ndDraw['newPrev'])))))
#     newPrev = ndDraw['newPrev'].sum()
#     print(newPrev)
#     #store = pd.DataFrame(columns=('newPrev'), index=[0])
#     #store['newPrev'][0] = newPrev
#     tlist.append(newPrev)                    

# %%
#%%
# Yearly transition
tmExpz2 = pd.read_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/prevMar23/tm23_avg12.csv')
plims = tmExpz2.loc[tmExpz2.index.repeat(len(bdcp))]
plims = plims.set_index(bdcp.index)

#%%
import time
start = time.time()
ytlist = []
dplist = []
for i in range(200):
    print(i)
    draws = pd.DataFrame(np.random.rand(len(bdcp), 1), columns=['draw'], index=bdcp.index)
    ndDraw = pd.concat([bdcp, draws, plims], axis = 1)
    ndDraw['draw'] = ndDraw['draw']*100
    ndDraw['califPot'] = np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] <= ndDraw['AA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AA']) & (ndDraw['draw'] <= ndDraw['AB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AB']) & (ndDraw['draw'] <= ndDraw['AC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AC']) & (ndDraw['draw'] <= ndDraw['AD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AD']) & (ndDraw['draw'] <= ndDraw['AE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AE']) & (ndDraw['draw'] <= ndDraw['AF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AF']) & (ndDraw['draw'] <= ndDraw['AS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] <= ndDraw['BA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BA']) & (ndDraw['draw'] <= ndDraw['BB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BB']) & (ndDraw['draw'] <= ndDraw['BC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BC']) & (ndDraw['draw'] <= ndDraw['BD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BD']) & (ndDraw['draw'] <= ndDraw['BE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BE']) & (ndDraw['draw'] <= ndDraw['BF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BF']) & (ndDraw['draw'] <= ndDraw['BS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] <= ndDraw['CA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CA']) & (ndDraw['draw'] <= ndDraw['CB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CB']) & (ndDraw['draw'] <= ndDraw['CC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CC']) & (ndDraw['draw'] <= ndDraw['CD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CD']) & (ndDraw['draw'] <= ndDraw['CE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CE']) & (ndDraw['draw'] <= ndDraw['CF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CF']) & (ndDraw['draw'] <= ndDraw['CS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] <= ndDraw['DA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DA']) & (ndDraw['draw'] <= ndDraw['DB']), 'B',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DB']) & (ndDraw['draw'] <= ndDraw['DC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DC']) & (ndDraw['draw'] <= ndDraw['DD']), 'D',  
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DD']) & (ndDraw['draw'] <= ndDraw['DE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DE']) & (ndDraw['draw'] <= ndDraw['DF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DF']) & (ndDraw['draw'] <= ndDraw['DS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] <= ndDraw['EA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EA']) & (ndDraw['draw'] <= ndDraw['EB']), 'B',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EB']) & (ndDraw['draw'] <= ndDraw['EC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EC']) & (ndDraw['draw'] <= ndDraw['ED']), 'D',  
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['ED']) & (ndDraw['draw'] <= ndDraw['EE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EE']) & (ndDraw['draw'] <= ndDraw['EF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EF']) & (ndDraw['draw'] <= ndDraw['ES']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['ES']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] <= ndDraw['FA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FA']) & (ndDraw['draw'] <= ndDraw['FB']), 'B', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FB']) & (ndDraw['draw'] <= ndDraw['FC']), 'C', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FC']) & (ndDraw['draw'] <= ndDraw['FD']), 'D', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FD']) & (ndDraw['draw'] <= ndDraw['FE']), 'E', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FE']) & (ndDraw['draw'] <= ndDraw['FF']), 'F',  
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FF']) & (ndDraw['draw'] <= ndDraw['FS']), 'S',   
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['draw'] <= ndDraw['SS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['draw'] > ndDraw['SS']) & (ndDraw['draw'] <= ndDraw['SZ']), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['draw'] <= ndDraw['ZZ']), 'Z', 
                        ndDraw['CALIFICACION'])))))))))))))))))))))))))))))))))))))))))))))))))))
    ndDraw['maxCalif'] = ndDraw.groupby('CTACLIENTE')['califPot'].transform('max')
    ndDraw['califPot'] = ndDraw['maxCalif']
    ndDraw['severity'] = np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 8,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 32,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.125,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 2.5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.4,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 1.6,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.0312,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 0.625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.025,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.2,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 0.8,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 8,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 32,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 10,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 16,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.125,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.25,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 2.5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.4,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 1.6,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.03125,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.0625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 0.625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.025,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.2,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 0.8,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                1))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    ndDraw['newPrev'] = ndDraw['severity'] * ndDraw['previus']
    ndDraw['newPrev'] = np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.025,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.2,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.5,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.8,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0,
                                ndDraw['newPrev'])))))))
    ndDraw['newPrev'] = np.where((ndDraw['califPot'] == 'F') | (ndDraw['califPot'] == 'S'), ndDraw['saldous']*1, ndDraw['newPrev'])
    ndDraw['difPrev'] = ndDraw['newPrev'] - ndDraw['previus']
    newPrevi = ndDraw['newPrev'].sum()
    exitPrevi = ndDraw[ndDraw['califPot'] == 'Z'].previus.sum()
    castPrevi = ndDraw[(ndDraw['califPot'] == 'S')].newPrev.sum()
    ffPrevi = ndDraw[(ndDraw['califPot'] == 'F') & (ndDraw['CALIFICACION'] == 'F')].newPrev.sum()
    fcastPrevi = ndDraw[(ndDraw['califPot'] == 'S') & (ndDraw['CALIFICACION'] == 'F')].newPrev.sum()
    fdetPrevi = ndDraw[(ndDraw['califPot'] == 'F') & (ndDraw['CALIFICACION'] != 'F')].difPrev.sum()
    newaPrevi = ndDraw[ndDraw['califPot'] == 'A'].newPrev.sum()
    oldaPrevi = ndDraw[ndDraw['CALIFICACION'] == 'A'].previus.sum()
    oldfPrevi = ndDraw[ndDraw['CALIFICACION'] == 'F'].previus.sum()
    detPrevi = ndDraw[(ndDraw['califPot'] == 'B') | (ndDraw['califPot'] == 'C') |
               (ndDraw['califPot'] == 'D')|(ndDraw['califPot'] == 'E')].difPrev.sum()
    difPrevi = ndDraw['difPrev'].sum()
    print(newPrevi)
    store = pd.DataFrame(columns=(['newPrev']), index=[0])
    store['newPrev'][0] = newPrevi
    store['exitPrev'] = exitPrevi
    store['castPrev'] = castPrevi
    store['fcastPrev'] = fcastPrevi
    store['fdetPrev'] = fdetPrevi
    store['detPrev'] = detPrevi
    store['newaPrev'] = newaPrevi
    store['oldaPrev'] = oldaPrevi
    store['oldfPrev'] = oldfPrevi
    store['diffPrev'] = difPrevi
    store['ffPrev'] = ffPrevi
    ytlist.append(store)  
    dfDraw = ndDraw[['CALIFICACION', 'califPot', 'previus', 'newPrev', 'difPrev']]
    dpAgg = dfDraw.groupby(['CALIFICACION','califPot']).agg(['sum','count'])
    dplist.append(dpAgg)
end = time.time()
exTime = end - start
print(exTime)

pd.options.display.float_format = '{:,.0f}'.format            
ytprev = pd.concat(ytlist, axis = 0)
dpprev = pd.concat(dplist, axis = 0)
#ytprev.newPrev.plot.density(color='green')
#ytprev.to_csv('D:/!bso/transMat/ytprev_sens12_W20.csv')
ytprev.to_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/ytprev_12avg.csv')
dpprev.to_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/dpprev_12avg.csv')

#%%
tmExpz2 = pd.read_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/tm22_avg12_Lag12.csv')
plims = tmExpz2.loc[tmExpz2.index.repeat(len(bdcp))]
plims = plims.set_index(bdcp.index)
import time
start = time.time()
ytlist = []
dplist = []
for i in range(200):
    print(i)
    draws = pd.DataFrame(np.random.rand(len(bdcp), 1), columns=['draw'], index=bdcp.index)
    ndDraw = pd.concat([bdcp, draws, plims], axis = 1)
    ndDraw['draw'] = ndDraw['draw']*100
    ndDraw['califPot'] = np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] <= ndDraw['AA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AA']) & (ndDraw['draw'] <= ndDraw['AB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AB']) & (ndDraw['draw'] <= ndDraw['AC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AC']) & (ndDraw['draw'] <= ndDraw['AD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AD']) & (ndDraw['draw'] <= ndDraw['AE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AE']) & (ndDraw['draw'] <= ndDraw['AF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AF']) & (ndDraw['draw'] <= ndDraw['AS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] <= ndDraw['BA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BA']) & (ndDraw['draw'] <= ndDraw['BB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BB']) & (ndDraw['draw'] <= ndDraw['BC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BC']) & (ndDraw['draw'] <= ndDraw['BD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BD']) & (ndDraw['draw'] <= ndDraw['BE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BE']) & (ndDraw['draw'] <= ndDraw['BF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BF']) & (ndDraw['draw'] <= ndDraw['BS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] <= ndDraw['CA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CA']) & (ndDraw['draw'] <= ndDraw['CB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CB']) & (ndDraw['draw'] <= ndDraw['CC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CC']) & (ndDraw['draw'] <= ndDraw['CD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CD']) & (ndDraw['draw'] <= ndDraw['CE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CE']) & (ndDraw['draw'] <= ndDraw['CF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CF']) & (ndDraw['draw'] <= ndDraw['CS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] <= ndDraw['DA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DA']) & (ndDraw['draw'] <= ndDraw['DB']), 'B',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DB']) & (ndDraw['draw'] <= ndDraw['DC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DC']) & (ndDraw['draw'] <= ndDraw['DD']), 'D',  
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DD']) & (ndDraw['draw'] <= ndDraw['DE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DE']) & (ndDraw['draw'] <= ndDraw['DF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DF']) & (ndDraw['draw'] <= ndDraw['DS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] <= ndDraw['EA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EA']) & (ndDraw['draw'] <= ndDraw['EB']), 'B',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EB']) & (ndDraw['draw'] <= ndDraw['EC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EC']) & (ndDraw['draw'] <= ndDraw['ED']), 'D',  
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['ED']) & (ndDraw['draw'] <= ndDraw['EE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EE']) & (ndDraw['draw'] <= ndDraw['EF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EF']) & (ndDraw['draw'] <= ndDraw['ES']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['ES']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] <= ndDraw['FA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FA']) & (ndDraw['draw'] <= ndDraw['FB']), 'B', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FB']) & (ndDraw['draw'] <= ndDraw['FC']), 'C', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FC']) & (ndDraw['draw'] <= ndDraw['FD']), 'D', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FD']) & (ndDraw['draw'] <= ndDraw['FE']), 'E', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FE']) & (ndDraw['draw'] <= ndDraw['FF']), 'F',  
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FF']) & (ndDraw['draw'] <= ndDraw['FS']), 'S',   
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['draw'] <= ndDraw['SS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['draw'] > ndDraw['SS']) & (ndDraw['draw'] <= ndDraw['SZ']), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['draw'] <= ndDraw['ZZ']), 'Z', 
                        ndDraw['CALIFICACION'])))))))))))))))))))))))))))))))))))))))))))))))))))
    ndDraw['maxCalif'] = ndDraw.groupby('CTACLIENTE')['califPot'].transform('max')
    ndDraw['califPot'] = ndDraw['maxCalif']
    ndDraw['severity'] = np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 8,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 32,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.125,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 2.5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.4,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 1.6,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.0312,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 0.625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.025,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.2,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 0.8,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 8,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 32,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 10,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 16,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.125,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.25,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 2.5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.4,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 1.6,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.03125,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.0625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 0.625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.025,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.2,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 0.8,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                1))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    ndDraw['newPrev'] = ndDraw['severity'] * ndDraw['previus']
    ndDraw['newPrev'] = np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.025,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.2,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.5,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.8,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0,
                                ndDraw['newPrev'])))))))
    ndDraw['newPrev'] = np.where((ndDraw['califPot'] == 'F') | (ndDraw['califPot'] == 'S'), ndDraw['saldous']*1, ndDraw['newPrev'])
    ndDraw['difPrev'] = ndDraw['newPrev'] - ndDraw['previus']
    newPrevi = ndDraw['newPrev'].sum()
    exitPrevi = ndDraw[ndDraw['califPot'] == 'Z'].previus.sum()
    castPrevi = ndDraw[(ndDraw['califPot'] == 'S')].newPrev.sum()
    ffPrevi = ndDraw[(ndDraw['califPot'] == 'F') & (ndDraw['CALIFICACION'] == 'F')].newPrev.sum()
    fcastPrevi = ndDraw[(ndDraw['califPot'] == 'S') & (ndDraw['CALIFICACION'] == 'F')].newPrev.sum()
    fdetPrevi = ndDraw[(ndDraw['califPot'] == 'F') & (ndDraw['CALIFICACION'] != 'F')].difPrev.sum()
    newaPrevi = ndDraw[ndDraw['califPot'] == 'A'].newPrev.sum()
    oldaPrevi = ndDraw[ndDraw['CALIFICACION'] == 'A'].previus.sum()
    oldfPrevi = ndDraw[ndDraw['CALIFICACION'] == 'F'].previus.sum()
    detPrevi = ndDraw[(ndDraw['califPot'] == 'B') | (ndDraw['califPot'] == 'C') |
               (ndDraw['califPot'] == 'D')|(ndDraw['califPot'] == 'E')].difPrev.sum()
    difPrevi = ndDraw['difPrev'].sum()
    print(newPrevi)
    store = pd.DataFrame(columns=(['newPrev']), index=[0])
    store['newPrev'][0] = newPrevi
    store['exitPrev'] = exitPrevi
    store['castPrev'] = castPrevi
    store['fcastPrev'] = fcastPrevi
    store['fdetPrev'] = fdetPrevi
    store['detPrev'] = detPrevi
    store['newaPrev'] = newaPrevi
    store['oldaPrev'] = oldaPrevi
    store['oldfPrev'] = oldfPrevi
    store['diffPrev'] = difPrevi
    store['ffPrev'] = ffPrevi
    ytlist.append(store)  
    dfDraw = ndDraw[['CALIFICACION', 'califPot', 'previus', 'newPrev', 'difPrev']]
    dpAgg = dfDraw.groupby(['CALIFICACION','califPot']).agg(['sum','count'])
    dplist.append(dpAgg)
end = time.time()
exTime = end - start
print(exTime)

pd.options.display.float_format = '{:,.0f}'.format            
ytprev = pd.concat(ytlist, axis = 0)
dpprev = pd.concat(dplist, axis = 0)
#ytprev.newPrev.plot.density(color='green')
#ytprev.to_csv('D:/!bso/transMat/ytprev_sens12_W20.csv')
ytprev.to_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/ytprev_avg12_Lag12.csv')
dpprev.to_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/dpprev_avg12_Lag12.csv')

#%%
tmExpz2 = pd.read_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/tm22_sensab.csv')
plims = tmExpz2.loc[tmExpz2.index.repeat(len(bdcp))]
plims = plims.set_index(bdcp.index)
import time
start = time.time()
ytlist = []
dplist = []
for i in range(200):
    print(i)
    draws = pd.DataFrame(np.random.rand(len(bdcp), 1), columns=['draw'], index=bdcp.index)
    ndDraw = pd.concat([bdcp, draws, plims], axis = 1)
    ndDraw['draw'] = ndDraw['draw']*100
    ndDraw['califPot'] = np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] <= ndDraw['AA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AA']) & (ndDraw['draw'] <= ndDraw['AB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AB']) & (ndDraw['draw'] <= ndDraw['AC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AC']) & (ndDraw['draw'] <= ndDraw['AD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AD']) & (ndDraw['draw'] <= ndDraw['AE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AE']) & (ndDraw['draw'] <= ndDraw['AF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AF']) & (ndDraw['draw'] <= ndDraw['AS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] <= ndDraw['BA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BA']) & (ndDraw['draw'] <= ndDraw['BB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BB']) & (ndDraw['draw'] <= ndDraw['BC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BC']) & (ndDraw['draw'] <= ndDraw['BD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BD']) & (ndDraw['draw'] <= ndDraw['BE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BE']) & (ndDraw['draw'] <= ndDraw['BF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BF']) & (ndDraw['draw'] <= ndDraw['BS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] <= ndDraw['CA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CA']) & (ndDraw['draw'] <= ndDraw['CB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CB']) & (ndDraw['draw'] <= ndDraw['CC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CC']) & (ndDraw['draw'] <= ndDraw['CD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CD']) & (ndDraw['draw'] <= ndDraw['CE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CE']) & (ndDraw['draw'] <= ndDraw['CF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CF']) & (ndDraw['draw'] <= ndDraw['CS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] <= ndDraw['DA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DA']) & (ndDraw['draw'] <= ndDraw['DB']), 'B',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DB']) & (ndDraw['draw'] <= ndDraw['DC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DC']) & (ndDraw['draw'] <= ndDraw['DD']), 'D',  
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DD']) & (ndDraw['draw'] <= ndDraw['DE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DE']) & (ndDraw['draw'] <= ndDraw['DF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DF']) & (ndDraw['draw'] <= ndDraw['DS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] <= ndDraw['EA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EA']) & (ndDraw['draw'] <= ndDraw['EB']), 'B',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EB']) & (ndDraw['draw'] <= ndDraw['EC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EC']) & (ndDraw['draw'] <= ndDraw['ED']), 'D',  
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['ED']) & (ndDraw['draw'] <= ndDraw['EE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EE']) & (ndDraw['draw'] <= ndDraw['EF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EF']) & (ndDraw['draw'] <= ndDraw['ES']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['ES']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] <= ndDraw['FA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FA']) & (ndDraw['draw'] <= ndDraw['FB']), 'B', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FB']) & (ndDraw['draw'] <= ndDraw['FC']), 'C', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FC']) & (ndDraw['draw'] <= ndDraw['FD']), 'D', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FD']) & (ndDraw['draw'] <= ndDraw['FE']), 'E', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FE']) & (ndDraw['draw'] <= ndDraw['FF']), 'F',  
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FF']) & (ndDraw['draw'] <= ndDraw['FS']), 'S',   
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['draw'] <= ndDraw['SS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['draw'] > ndDraw['SS']) & (ndDraw['draw'] <= ndDraw['SZ']), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['draw'] <= ndDraw['ZZ']), 'Z', 
                        ndDraw['CALIFICACION'])))))))))))))))))))))))))))))))))))))))))))))))))))
    ndDraw['maxCalif'] = ndDraw.groupby('CTACLIENTE')['califPot'].transform('max')
    ndDraw['califPot'] = ndDraw['maxCalif']
    ndDraw['severity'] = np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 8,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 32,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.125,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 2.5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.4,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 1.6,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.0312,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 0.625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.025,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.2,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 0.8,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 8,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 32,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 10,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 16,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.125,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.25,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 2.5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.4,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 1.6,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.03125,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.0625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 0.625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.025,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.2,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 0.8,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                1))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    ndDraw['newPrev'] = ndDraw['severity'] * ndDraw['previus']
    ndDraw['newPrev'] = np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.025,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.2,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.5,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.8,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0,
                                ndDraw['newPrev'])))))))
    ndDraw['newPrev'] = np.where((ndDraw['califPot'] == 'F') | (ndDraw['califPot'] == 'S'), ndDraw['saldous']*1, ndDraw['newPrev'])
    ndDraw['difPrev'] = ndDraw['newPrev'] - ndDraw['previus']
    newPrevi = ndDraw['newPrev'].sum()
    exitPrevi = ndDraw[ndDraw['califPot'] == 'Z'].previus.sum()
    castPrevi = ndDraw[(ndDraw['califPot'] == 'S')].newPrev.sum()
    ffPrevi = ndDraw[(ndDraw['califPot'] == 'F') & (ndDraw['CALIFICACION'] == 'F')].newPrev.sum()
    fcastPrevi = ndDraw[(ndDraw['califPot'] == 'S') & (ndDraw['CALIFICACION'] == 'F')].newPrev.sum()
    fdetPrevi = ndDraw[(ndDraw['califPot'] == 'F') & (ndDraw['CALIFICACION'] != 'F')].difPrev.sum()
    newaPrevi = ndDraw[ndDraw['califPot'] == 'A'].newPrev.sum()
    oldaPrevi = ndDraw[ndDraw['CALIFICACION'] == 'A'].previus.sum()
    oldfPrevi = ndDraw[ndDraw['CALIFICACION'] == 'F'].previus.sum()
    detPrevi = ndDraw[(ndDraw['califPot'] == 'B') | (ndDraw['califPot'] == 'C') |
               (ndDraw['califPot'] == 'D')|(ndDraw['califPot'] == 'E')].difPrev.sum()
    difPrevi = ndDraw['difPrev'].sum()
    print(newPrevi)
    store = pd.DataFrame(columns=(['newPrev']), index=[0])
    store['newPrev'][0] = newPrevi
    store['exitPrev'] = exitPrevi
    store['castPrev'] = castPrevi
    store['fcastPrev'] = fcastPrevi
    store['fdetPrev'] = fdetPrevi
    store['detPrev'] = detPrevi
    store['newaPrev'] = newaPrevi
    store['oldaPrev'] = oldaPrevi
    store['oldfPrev'] = oldfPrevi
    store['diffPrev'] = difPrevi
    store['ffPrev'] = ffPrevi
    ytlist.append(store)  
    dfDraw = ndDraw[['CALIFICACION', 'califPot', 'previus', 'newPrev', 'difPrev']]
    dpAgg = dfDraw.groupby(['CALIFICACION','califPot']).agg(['sum','count'])
    dplist.append(dpAgg)
end = time.time()
exTime = end - start
print(exTime)

pd.options.display.float_format = '{:,.0f}'.format            
ytprev = pd.concat(ytlist, axis = 0)
dpprev = pd.concat(dplist, axis = 0)
#ytprev.newPrev.plot.density(color='green')
#ytprev.to_csv('D:/!bso/transMat/ytprev_sens12_W20.csv')
ytprev.to_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/ytprev_12sensab.csv')
dpprev.to_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/dpprev_12sensab.csv')

#%%
tmExpz2 = pd.read_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/tm22_sensba.csv')
plims = tmExpz2.loc[tmExpz2.index.repeat(len(bdcp))]
plims = plims.set_index(bdcp.index)
import time
start = time.time()
ytlist = []
dplist = []
for i in range(200):
    print(i)
    draws = pd.DataFrame(np.random.rand(len(bdcp), 1), columns=['draw'], index=bdcp.index)
    ndDraw = pd.concat([bdcp, draws, plims], axis = 1)
    ndDraw['draw'] = ndDraw['draw']*100
    ndDraw['califPot'] = np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] <= ndDraw['AA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AA']) & (ndDraw['draw'] <= ndDraw['AB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AB']) & (ndDraw['draw'] <= ndDraw['AC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AC']) & (ndDraw['draw'] <= ndDraw['AD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AD']) & (ndDraw['draw'] <= ndDraw['AE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AE']) & (ndDraw['draw'] <= ndDraw['AF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AF']) & (ndDraw['draw'] <= ndDraw['AS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] <= ndDraw['BA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BA']) & (ndDraw['draw'] <= ndDraw['BB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BB']) & (ndDraw['draw'] <= ndDraw['BC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BC']) & (ndDraw['draw'] <= ndDraw['BD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BD']) & (ndDraw['draw'] <= ndDraw['BE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BE']) & (ndDraw['draw'] <= ndDraw['BF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BF']) & (ndDraw['draw'] <= ndDraw['BS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] <= ndDraw['CA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CA']) & (ndDraw['draw'] <= ndDraw['CB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CB']) & (ndDraw['draw'] <= ndDraw['CC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CC']) & (ndDraw['draw'] <= ndDraw['CD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CD']) & (ndDraw['draw'] <= ndDraw['CE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CE']) & (ndDraw['draw'] <= ndDraw['CF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CF']) & (ndDraw['draw'] <= ndDraw['CS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] <= ndDraw['DA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DA']) & (ndDraw['draw'] <= ndDraw['DB']), 'B',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DB']) & (ndDraw['draw'] <= ndDraw['DC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DC']) & (ndDraw['draw'] <= ndDraw['DD']), 'D',  
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DD']) & (ndDraw['draw'] <= ndDraw['DE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DE']) & (ndDraw['draw'] <= ndDraw['DF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DF']) & (ndDraw['draw'] <= ndDraw['DS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] <= ndDraw['EA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EA']) & (ndDraw['draw'] <= ndDraw['EB']), 'B',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EB']) & (ndDraw['draw'] <= ndDraw['EC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EC']) & (ndDraw['draw'] <= ndDraw['ED']), 'D',  
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['ED']) & (ndDraw['draw'] <= ndDraw['EE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EE']) & (ndDraw['draw'] <= ndDraw['EF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EF']) & (ndDraw['draw'] <= ndDraw['ES']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['ES']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] <= ndDraw['FA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FA']) & (ndDraw['draw'] <= ndDraw['FB']), 'B', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FB']) & (ndDraw['draw'] <= ndDraw['FC']), 'C', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FC']) & (ndDraw['draw'] <= ndDraw['FD']), 'D', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FD']) & (ndDraw['draw'] <= ndDraw['FE']), 'E', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FE']) & (ndDraw['draw'] <= ndDraw['FF']), 'F',  
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FF']) & (ndDraw['draw'] <= ndDraw['FS']), 'S',   
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['draw'] <= ndDraw['SS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['draw'] > ndDraw['SS']) & (ndDraw['draw'] <= ndDraw['SZ']), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['draw'] <= ndDraw['ZZ']), 'Z', 
                        ndDraw['CALIFICACION'])))))))))))))))))))))))))))))))))))))))))))))))))))
    ndDraw['maxCalif'] = ndDraw.groupby('CTACLIENTE')['califPot'].transform('max')
    ndDraw['califPot'] = ndDraw['maxCalif']
    ndDraw['severity'] = np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 8,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 32,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.125,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 2.5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.4,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 1.6,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.0312,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 0.625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.025,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.2,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 0.8,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 8,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 32,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 10,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 16,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.125,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.25,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 2.5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.4,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 1.6,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.03125,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.0625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 0.625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.025,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.2,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 0.8,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                1))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    ndDraw['newPrev'] = ndDraw['severity'] * ndDraw['previus']
    ndDraw['newPrev'] = np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.025,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.2,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.5,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.8,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0,
                                ndDraw['newPrev'])))))))
    ndDraw['newPrev'] = np.where((ndDraw['califPot'] == 'F') | (ndDraw['califPot'] == 'S'), ndDraw['saldous']*1, ndDraw['newPrev'])
    ndDraw['difPrev'] = ndDraw['newPrev'] - ndDraw['previus']
    newPrevi = ndDraw['newPrev'].sum()
    exitPrevi = ndDraw[ndDraw['califPot'] == 'Z'].previus.sum()
    castPrevi = ndDraw[(ndDraw['califPot'] == 'S')].newPrev.sum()
    ffPrevi = ndDraw[(ndDraw['califPot'] == 'F') & (ndDraw['CALIFICACION'] == 'F')].newPrev.sum()
    fcastPrevi = ndDraw[(ndDraw['califPot'] == 'S') & (ndDraw['CALIFICACION'] == 'F')].newPrev.sum()
    fdetPrevi = ndDraw[(ndDraw['califPot'] == 'F') & (ndDraw['CALIFICACION'] != 'F')].difPrev.sum()
    newaPrevi = ndDraw[ndDraw['califPot'] == 'A'].newPrev.sum()
    oldaPrevi = ndDraw[ndDraw['CALIFICACION'] == 'A'].previus.sum()
    oldfPrevi = ndDraw[ndDraw['CALIFICACION'] == 'F'].previus.sum()
    detPrevi = ndDraw[(ndDraw['califPot'] == 'B') | (ndDraw['califPot'] == 'C') |
               (ndDraw['califPot'] == 'D')|(ndDraw['califPot'] == 'E')].difPrev.sum()
    difPrevi = ndDraw['difPrev'].sum()
    print(newPrevi)
    store = pd.DataFrame(columns=(['newPrev']), index=[0])
    store['newPrev'][0] = newPrevi
    store['exitPrev'] = exitPrevi
    store['castPrev'] = castPrevi
    store['fcastPrev'] = fcastPrevi
    store['fdetPrev'] = fdetPrevi
    store['detPrev'] = detPrevi
    store['newaPrev'] = newaPrevi
    store['oldaPrev'] = oldaPrevi
    store['oldfPrev'] = oldfPrevi
    store['diffPrev'] = difPrevi
    store['ffPrev'] = ffPrevi
    ytlist.append(store)  
    dfDraw = ndDraw[['CALIFICACION', 'califPot', 'previus', 'newPrev', 'difPrev']]
    dpAgg = dfDraw.groupby(['CALIFICACION','califPot']).agg(['sum','count'])
    dplist.append(dpAgg)
end = time.time()
exTime = end - start
print(exTime)

pd.options.display.float_format = '{:,.0f}'.format            
ytprev = pd.concat(ytlist, axis = 0)
dpprev = pd.concat(dplist, axis = 0)
#ytprev.newPrev.plot.density(color='green')
#ytprev.to_csv('D:/!bso/transMat/ytprev_sens12_W20.csv')
ytprev.to_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/ytprev_12sensba.csv')
dpprev.to_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/dpprev_12sensba.csv')

#%%
tmExpz2 = pd.read_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/tm22_sensP0.01P0.99.csv')
plims = tmExpz2.loc[tmExpz2.index.repeat(len(bdcp))]
plims = plims.set_index(bdcp.index)
import time
start = time.time()
ytlist = []
dplist = []
for i in range(200):
    print(i)
    draws = pd.DataFrame(np.random.rand(len(bdcp), 1), columns=['draw'], index=bdcp.index)
    ndDraw = pd.concat([bdcp, draws, plims], axis = 1)
    ndDraw['draw'] = ndDraw['draw']*100
    ndDraw['califPot'] = np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] <= ndDraw['AA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AA']) & (ndDraw['draw'] <= ndDraw['AB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AB']) & (ndDraw['draw'] <= ndDraw['AC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AC']) & (ndDraw['draw'] <= ndDraw['AD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AD']) & (ndDraw['draw'] <= ndDraw['AE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AE']) & (ndDraw['draw'] <= ndDraw['AF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AF']) & (ndDraw['draw'] <= ndDraw['AS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] <= ndDraw['BA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BA']) & (ndDraw['draw'] <= ndDraw['BB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BB']) & (ndDraw['draw'] <= ndDraw['BC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BC']) & (ndDraw['draw'] <= ndDraw['BD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BD']) & (ndDraw['draw'] <= ndDraw['BE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BE']) & (ndDraw['draw'] <= ndDraw['BF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BF']) & (ndDraw['draw'] <= ndDraw['BS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] <= ndDraw['CA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CA']) & (ndDraw['draw'] <= ndDraw['CB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CB']) & (ndDraw['draw'] <= ndDraw['CC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CC']) & (ndDraw['draw'] <= ndDraw['CD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CD']) & (ndDraw['draw'] <= ndDraw['CE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CE']) & (ndDraw['draw'] <= ndDraw['CF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CF']) & (ndDraw['draw'] <= ndDraw['CS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] <= ndDraw['DA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DA']) & (ndDraw['draw'] <= ndDraw['DB']), 'B',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DB']) & (ndDraw['draw'] <= ndDraw['DC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DC']) & (ndDraw['draw'] <= ndDraw['DD']), 'D',  
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DD']) & (ndDraw['draw'] <= ndDraw['DE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DE']) & (ndDraw['draw'] <= ndDraw['DF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DF']) & (ndDraw['draw'] <= ndDraw['DS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] <= ndDraw['EA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EA']) & (ndDraw['draw'] <= ndDraw['EB']), 'B',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EB']) & (ndDraw['draw'] <= ndDraw['EC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EC']) & (ndDraw['draw'] <= ndDraw['ED']), 'D',  
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['ED']) & (ndDraw['draw'] <= ndDraw['EE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EE']) & (ndDraw['draw'] <= ndDraw['EF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EF']) & (ndDraw['draw'] <= ndDraw['ES']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['ES']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] <= ndDraw['FA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FA']) & (ndDraw['draw'] <= ndDraw['FB']), 'B', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FB']) & (ndDraw['draw'] <= ndDraw['FC']), 'C', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FC']) & (ndDraw['draw'] <= ndDraw['FD']), 'D', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FD']) & (ndDraw['draw'] <= ndDraw['FE']), 'E', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FE']) & (ndDraw['draw'] <= ndDraw['FF']), 'F',  
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FF']) & (ndDraw['draw'] <= ndDraw['FS']), 'S',   
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['draw'] <= ndDraw['SS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['draw'] > ndDraw['SS']) & (ndDraw['draw'] <= ndDraw['SZ']), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['draw'] <= ndDraw['ZZ']), 'Z', 
                        ndDraw['CALIFICACION'])))))))))))))))))))))))))))))))))))))))))))))))))))
    ndDraw['maxCalif'] = ndDraw.groupby('CTACLIENTE')['califPot'].transform('max')
    ndDraw['califPot'] = ndDraw['maxCalif']
    ndDraw['severity'] = np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 8,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 32,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.125,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 2.5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.4,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 1.6,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.0312,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 0.625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.025,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.2,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 0.8,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 8,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 32,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 10,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 16,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.125,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.25,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 2.5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.4,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 1.6,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.03125,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.0625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 0.625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.025,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.2,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 0.8,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                1))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    ndDraw['newPrev'] = ndDraw['severity'] * ndDraw['previus']
    ndDraw['newPrev'] = np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.025,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.2,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.5,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.8,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0,
                                ndDraw['newPrev'])))))))
    ndDraw['newPrev'] = np.where((ndDraw['califPot'] == 'F') | (ndDraw['califPot'] == 'S'), ndDraw['saldous']*1, ndDraw['newPrev'])
    ndDraw['difPrev'] = ndDraw['newPrev'] - ndDraw['previus']
    newPrevi = ndDraw['newPrev'].sum()
    exitPrevi = ndDraw[ndDraw['califPot'] == 'Z'].previus.sum()
    castPrevi = ndDraw[(ndDraw['califPot'] == 'S')].newPrev.sum()
    ffPrevi = ndDraw[(ndDraw['califPot'] == 'F') & (ndDraw['CALIFICACION'] == 'F')].newPrev.sum()
    fcastPrevi = ndDraw[(ndDraw['califPot'] == 'S') & (ndDraw['CALIFICACION'] == 'F')].newPrev.sum()
    fdetPrevi = ndDraw[(ndDraw['califPot'] == 'F') & (ndDraw['CALIFICACION'] != 'F')].difPrev.sum()
    newaPrevi = ndDraw[ndDraw['califPot'] == 'A'].newPrev.sum()
    oldaPrevi = ndDraw[ndDraw['CALIFICACION'] == 'A'].previus.sum()
    oldfPrevi = ndDraw[ndDraw['CALIFICACION'] == 'F'].previus.sum()
    detPrevi = ndDraw[(ndDraw['califPot'] == 'B') | (ndDraw['califPot'] == 'C') |
               (ndDraw['califPot'] == 'D')|(ndDraw['califPot'] == 'E')].difPrev.sum()
    difPrevi = ndDraw['difPrev'].sum()
    print(newPrevi)
    store = pd.DataFrame(columns=(['newPrev']), index=[0])
    store['newPrev'][0] = newPrevi
    store['exitPrev'] = exitPrevi
    store['castPrev'] = castPrevi
    store['fcastPrev'] = fcastPrevi
    store['fdetPrev'] = fdetPrevi
    store['detPrev'] = detPrevi
    store['newaPrev'] = newaPrevi
    store['oldaPrev'] = oldaPrevi
    store['oldfPrev'] = oldfPrevi
    store['diffPrev'] = difPrevi
    store['ffPrev'] = ffPrevi
    ytlist.append(store)  
    dfDraw = ndDraw[['CALIFICACION', 'califPot', 'previus', 'newPrev', 'difPrev']]
    dpAgg = dfDraw.groupby(['CALIFICACION','califPot']).agg(['sum','count'])
    dplist.append(dpAgg)
end = time.time()
exTime = end - start
print(exTime)

pd.options.display.float_format = '{:,.0f}'.format            
ytprev = pd.concat(ytlist, axis = 0)
dpprev = pd.concat(dplist, axis = 0)
#ytprev.newPrev.plot.density(color='green')
#ytprev.to_csv('D:/!bso/transMat/ytprev_sens12_W20.csv')
ytprev.to_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/ytprev_12sensP1P99.csv')
dpprev.to_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/dpprev_12sensP1P99.csv')

#%%
tmExpz2 = pd.read_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/tm22_sensP0.1P0.9.csv')
plims = tmExpz2.loc[tmExpz2.index.repeat(len(bdcp))]
plims = plims.set_index(bdcp.index)
import time
start = time.time()
ytlist = []
dplist = []
for i in range(200):
    print(i)
    draws = pd.DataFrame(np.random.rand(len(bdcp), 1), columns=['draw'], index=bdcp.index)
    ndDraw = pd.concat([bdcp, draws, plims], axis = 1)
    ndDraw['draw'] = ndDraw['draw']*100
    ndDraw['califPot'] = np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] <= ndDraw['AA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AA']) & (ndDraw['draw'] <= ndDraw['AB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AB']) & (ndDraw['draw'] <= ndDraw['AC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AC']) & (ndDraw['draw'] <= ndDraw['AD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AD']) & (ndDraw['draw'] <= ndDraw['AE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AE']) & (ndDraw['draw'] <= ndDraw['AF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AF']) & (ndDraw['draw'] <= ndDraw['AS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] <= ndDraw['BA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BA']) & (ndDraw['draw'] <= ndDraw['BB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BB']) & (ndDraw['draw'] <= ndDraw['BC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BC']) & (ndDraw['draw'] <= ndDraw['BD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BD']) & (ndDraw['draw'] <= ndDraw['BE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BE']) & (ndDraw['draw'] <= ndDraw['BF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BF']) & (ndDraw['draw'] <= ndDraw['BS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] <= ndDraw['CA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CA']) & (ndDraw['draw'] <= ndDraw['CB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CB']) & (ndDraw['draw'] <= ndDraw['CC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CC']) & (ndDraw['draw'] <= ndDraw['CD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CD']) & (ndDraw['draw'] <= ndDraw['CE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CE']) & (ndDraw['draw'] <= ndDraw['CF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CF']) & (ndDraw['draw'] <= ndDraw['CS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] <= ndDraw['DA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DA']) & (ndDraw['draw'] <= ndDraw['DB']), 'B',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DB']) & (ndDraw['draw'] <= ndDraw['DC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DC']) & (ndDraw['draw'] <= ndDraw['DD']), 'D',  
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DD']) & (ndDraw['draw'] <= ndDraw['DE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DE']) & (ndDraw['draw'] <= ndDraw['DF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DF']) & (ndDraw['draw'] <= ndDraw['DS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] <= ndDraw['EA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EA']) & (ndDraw['draw'] <= ndDraw['EB']), 'B',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EB']) & (ndDraw['draw'] <= ndDraw['EC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EC']) & (ndDraw['draw'] <= ndDraw['ED']), 'D',  
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['ED']) & (ndDraw['draw'] <= ndDraw['EE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EE']) & (ndDraw['draw'] <= ndDraw['EF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EF']) & (ndDraw['draw'] <= ndDraw['ES']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['ES']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] <= ndDraw['FA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FA']) & (ndDraw['draw'] <= ndDraw['FB']), 'B', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FB']) & (ndDraw['draw'] <= ndDraw['FC']), 'C', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FC']) & (ndDraw['draw'] <= ndDraw['FD']), 'D', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FD']) & (ndDraw['draw'] <= ndDraw['FE']), 'E', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FE']) & (ndDraw['draw'] <= ndDraw['FF']), 'F',  
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FF']) & (ndDraw['draw'] <= ndDraw['FS']), 'S',   
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['draw'] <= ndDraw['SS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['draw'] > ndDraw['SS']) & (ndDraw['draw'] <= ndDraw['SZ']), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['draw'] <= ndDraw['ZZ']), 'Z', 
                        ndDraw['CALIFICACION'])))))))))))))))))))))))))))))))))))))))))))))))))))
    ndDraw['maxCalif'] = ndDraw.groupby('CTACLIENTE')['califPot'].transform('max')
    ndDraw['califPot'] = ndDraw['maxCalif']
    ndDraw['severity'] = np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 8,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 32,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.125,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 2.5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.4,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 1.6,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.0312,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 0.625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.025,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.2,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 0.8,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 8,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 32,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 10,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 16,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.125,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.25,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 2.5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.4,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 1.6,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.03125,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.0625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 0.625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.025,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.2,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 0.8,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                1))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    ndDraw['newPrev'] = ndDraw['severity'] * ndDraw['previus']
    ndDraw['newPrev'] = np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.025,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.2,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.5,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.8,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0,
                                ndDraw['newPrev'])))))))
    ndDraw['newPrev'] = np.where((ndDraw['califPot'] == 'F') | (ndDraw['califPot'] == 'S'), ndDraw['saldous']*1, ndDraw['newPrev'])
    ndDraw['difPrev'] = ndDraw['newPrev'] - ndDraw['previus']
    newPrevi = ndDraw['newPrev'].sum()
    exitPrevi = ndDraw[ndDraw['califPot'] == 'Z'].previus.sum()
    castPrevi = ndDraw[(ndDraw['califPot'] == 'S')].newPrev.sum()
    ffPrevi = ndDraw[(ndDraw['califPot'] == 'F') & (ndDraw['CALIFICACION'] == 'F')].newPrev.sum()
    fcastPrevi = ndDraw[(ndDraw['califPot'] == 'S') & (ndDraw['CALIFICACION'] == 'F')].newPrev.sum()
    fdetPrevi = ndDraw[(ndDraw['califPot'] == 'F') & (ndDraw['CALIFICACION'] != 'F')].difPrev.sum()
    newaPrevi = ndDraw[ndDraw['califPot'] == 'A'].newPrev.sum()
    oldaPrevi = ndDraw[ndDraw['CALIFICACION'] == 'A'].previus.sum()
    oldfPrevi = ndDraw[ndDraw['CALIFICACION'] == 'F'].previus.sum()
    detPrevi = ndDraw[(ndDraw['califPot'] == 'B') | (ndDraw['califPot'] == 'C') |
               (ndDraw['califPot'] == 'D')|(ndDraw['califPot'] == 'E')].difPrev.sum()
    difPrevi = ndDraw['difPrev'].sum()
    print(newPrevi)
    store = pd.DataFrame(columns=(['newPrev']), index=[0])
    store['newPrev'][0] = newPrevi
    store['exitPrev'] = exitPrevi
    store['castPrev'] = castPrevi
    store['fcastPrev'] = fcastPrevi
    store['fdetPrev'] = fdetPrevi
    store['detPrev'] = detPrevi
    store['newaPrev'] = newaPrevi
    store['oldaPrev'] = oldaPrevi
    store['oldfPrev'] = oldfPrevi
    store['diffPrev'] = difPrevi
    store['ffPrev'] = ffPrevi
    ytlist.append(store)  
    dfDraw = ndDraw[['CALIFICACION', 'califPot', 'previus', 'newPrev', 'difPrev']]
    dpAgg = dfDraw.groupby(['CALIFICACION','califPot']).agg(['sum','count'])
    dplist.append(dpAgg)
end = time.time()
exTime = end - start
print(exTime)

pd.options.display.float_format = '{:,.0f}'.format            
ytprev = pd.concat(ytlist, axis = 0)
dpprev = pd.concat(dplist, axis = 0)
#ytprev.newPrev.plot.density(color='green')
#ytprev.to_csv('D:/!bso/transMat/ytprev_sens12_W20.csv')
ytprev.to_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/ytprev_12sensP10P90.csv')
dpprev.to_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/dpprev_12sensP10P90.csv')

#%%
tmExpz2 = pd.read_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/tm22_sensP0.25P0.75.csv')
plims = tmExpz2.loc[tmExpz2.index.repeat(len(bdcp))]
plims = plims.set_index(bdcp.index)
import time
start = time.time()
ytlist = []
dplist = []
for i in range(200):
    print(i)
    draws = pd.DataFrame(np.random.rand(len(bdcp), 1), columns=['draw'], index=bdcp.index)
    ndDraw = pd.concat([bdcp, draws, plims], axis = 1)
    ndDraw['draw'] = ndDraw['draw']*100
    ndDraw['califPot'] = np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] <= ndDraw['AA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AA']) & (ndDraw['draw'] <= ndDraw['AB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AB']) & (ndDraw['draw'] <= ndDraw['AC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AC']) & (ndDraw['draw'] <= ndDraw['AD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AD']) & (ndDraw['draw'] <= ndDraw['AE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AE']) & (ndDraw['draw'] <= ndDraw['AF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AF']) & (ndDraw['draw'] <= ndDraw['AS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['draw'] > ndDraw['AS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] <= ndDraw['BA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BA']) & (ndDraw['draw'] <= ndDraw['BB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BB']) & (ndDraw['draw'] <= ndDraw['BC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BC']) & (ndDraw['draw'] <= ndDraw['BD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BD']) & (ndDraw['draw'] <= ndDraw['BE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BE']) & (ndDraw['draw'] <= ndDraw['BF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BF']) & (ndDraw['draw'] <= ndDraw['BS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['draw'] > ndDraw['BS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] <= ndDraw['CA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CA']) & (ndDraw['draw'] <= ndDraw['CB']), 'B',  
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CB']) & (ndDraw['draw'] <= ndDraw['CC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CC']) & (ndDraw['draw'] <= ndDraw['CD']), 'D',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CD']) & (ndDraw['draw'] <= ndDraw['CE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CE']) & (ndDraw['draw'] <= ndDraw['CF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CF']) & (ndDraw['draw'] <= ndDraw['CS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['draw'] > ndDraw['CS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] <= ndDraw['DA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DA']) & (ndDraw['draw'] <= ndDraw['DB']), 'B',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DB']) & (ndDraw['draw'] <= ndDraw['DC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DC']) & (ndDraw['draw'] <= ndDraw['DD']), 'D',  
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DD']) & (ndDraw['draw'] <= ndDraw['DE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DE']) & (ndDraw['draw'] <= ndDraw['DF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DF']) & (ndDraw['draw'] <= ndDraw['DS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['draw'] > ndDraw['DS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] <= ndDraw['EA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EA']) & (ndDraw['draw'] <= ndDraw['EB']), 'B',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EB']) & (ndDraw['draw'] <= ndDraw['EC']), 'C',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EC']) & (ndDraw['draw'] <= ndDraw['ED']), 'D',  
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['ED']) & (ndDraw['draw'] <= ndDraw['EE']), 'E',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EE']) & (ndDraw['draw'] <= ndDraw['EF']), 'F',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['EF']) & (ndDraw['draw'] <= ndDraw['ES']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['draw'] > ndDraw['ES']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] <= ndDraw['FA']), 'A',
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FA']) & (ndDraw['draw'] <= ndDraw['FB']), 'B', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FB']) & (ndDraw['draw'] <= ndDraw['FC']), 'C', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FC']) & (ndDraw['draw'] <= ndDraw['FD']), 'D', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FD']) & (ndDraw['draw'] <= ndDraw['FE']), 'E', 
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FE']) & (ndDraw['draw'] <= ndDraw['FF']), 'F',  
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FF']) & (ndDraw['draw'] <= ndDraw['FS']), 'S',   
                        np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['draw'] > ndDraw['FS']) & (ndDraw['draw'] <= 100), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['draw'] <= ndDraw['SS']), 'S',
                        np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['draw'] > ndDraw['SS']) & (ndDraw['draw'] <= ndDraw['SZ']), 'Z',
                        np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['draw'] <= ndDraw['ZZ']), 'Z', 
                        ndDraw['CALIFICACION'])))))))))))))))))))))))))))))))))))))))))))))))))))
    ndDraw['maxCalif'] = ndDraw.groupby('CTACLIENTE')['califPot'].transform('max')
    ndDraw['califPot'] = ndDraw['maxCalif']
    ndDraw['severity'] = np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 8,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 32,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.125,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 2.5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.4,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 1.6,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.0312,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 0.625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , 0.025,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , 0.2,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , 0.8,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 8,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 32,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 40,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 10,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 16,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 20,
                                np.where((ndDraw['CALIFICACION'] == 'B') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.125,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.25,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 2.5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 4,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 5,
                                np.where((ndDraw['CALIFICACION'] == 'C') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.4,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 1.6,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 2,
                                np.where((ndDraw['CALIFICACION'] == 'D') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.03125,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.0625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 0.625,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1.25,
                                np.where((ndDraw['CALIFICACION'] == 'E') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'A') & (ndDraw['MONEDA'] != 0) , 0.025,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] != 0) , 0.05,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] != 0) , 0.2,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] != 0) , 0.5,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] != 0) , 0.8,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'F') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] != 0) , 1,
                                np.where((ndDraw['CALIFICACION'] == 'S') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                np.where((ndDraw['CALIFICACION'] == 'Z') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] != 0) , 0,
                                1))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    ndDraw['newPrev'] = ndDraw['severity'] * ndDraw['previus']
    ndDraw['newPrev'] = np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'B') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.025,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'C') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.2,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'D') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.5,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'E') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0.8,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'F') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'S') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*1,
                                np.where((ndDraw['CALIFICACION'] == 'A') & (ndDraw['califPot'] == 'Z') & (ndDraw['MONEDA'] == 0) , ndDraw['saldous']*0,
                                ndDraw['newPrev'])))))))
    ndDraw['newPrev'] = np.where((ndDraw['califPot'] == 'F') | (ndDraw['califPot'] == 'S'), ndDraw['saldous']*1, ndDraw['newPrev'])
    ndDraw['difPrev'] = ndDraw['newPrev'] - ndDraw['previus']
    newPrevi = ndDraw['newPrev'].sum()
    exitPrevi = ndDraw[ndDraw['califPot'] == 'Z'].previus.sum()
    castPrevi = ndDraw[(ndDraw['califPot'] == 'S')].newPrev.sum()
    ffPrevi = ndDraw[(ndDraw['califPot'] == 'F') & (ndDraw['CALIFICACION'] == 'F')].newPrev.sum()
    fcastPrevi = ndDraw[(ndDraw['califPot'] == 'S') & (ndDraw['CALIFICACION'] == 'F')].newPrev.sum()
    fdetPrevi = ndDraw[(ndDraw['califPot'] == 'F') & (ndDraw['CALIFICACION'] != 'F')].difPrev.sum()
    newaPrevi = ndDraw[ndDraw['califPot'] == 'A'].newPrev.sum()
    oldaPrevi = ndDraw[ndDraw['CALIFICACION'] == 'A'].previus.sum()
    oldfPrevi = ndDraw[ndDraw['CALIFICACION'] == 'F'].previus.sum()
    detPrevi = ndDraw[(ndDraw['califPot'] == 'B') | (ndDraw['califPot'] == 'C') |
               (ndDraw['califPot'] == 'D')|(ndDraw['califPot'] == 'E')].difPrev.sum()
    difPrevi = ndDraw['difPrev'].sum()
    print(newPrevi)
    store = pd.DataFrame(columns=(['newPrev']), index=[0])
    store['newPrev'][0] = newPrevi
    store['exitPrev'] = exitPrevi
    store['castPrev'] = castPrevi
    store['fcastPrev'] = fcastPrevi
    store['fdetPrev'] = fdetPrevi
    store['detPrev'] = detPrevi
    store['newaPrev'] = newaPrevi
    store['oldaPrev'] = oldaPrevi
    store['oldfPrev'] = oldfPrevi
    store['diffPrev'] = difPrevi
    store['ffPrev'] = ffPrevi
    ytlist.append(store)  
    dfDraw = ndDraw[['CALIFICACION', 'califPot', 'previus', 'newPrev', 'difPrev']]
    dpAgg = dfDraw.groupby(['CALIFICACION','califPot']).agg(['sum','count'])
    dplist.append(dpAgg)
end = time.time()
exTime = end - start
print(exTime)

pd.options.display.float_format = '{:,.0f}'.format            
ytprev = pd.concat(ytlist, axis = 0)
dpprev = pd.concat(dplist, axis = 0)
#ytprev.newPrev.plot.density(color='green')
#ytprev.to_csv('D:/!bso/transMat/ytprev_sens12_W20.csv')
ytprev.to_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/ytprev_12sensP25P75.csv')
dpprev.to_csv('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/prevTransMat/acumulados/dpprev_12sensP25P75.csv')
# %%
checkPrev = ndDraw[['saldous', 'CALIFICACION', 'previus', 'draw', 'califPot', 'severity', 'newPrev']]
ytprev.castPrev.plot.density(color='green')
# %%
pd.crosstab(checkPrev['CALIFICACION'], checkPrev['califPot'])
# %%
