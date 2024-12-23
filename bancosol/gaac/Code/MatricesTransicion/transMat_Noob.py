# -*- coding: utf-8 -*-
"""
Created on Tue Jul 18 10:41:36 2023

@author: gaguilarc
"""

#CARGA DE PAQUETES
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import time
#%%
#LECTURA EN LOOP Y CREACION DE MATRICES
    
mes = ['Ene','Feb','Mar','Abr','May','Jun','Jul','Ago','Sep','Oct','Nov','Dic']
my = [m+str(year) for year in range(2015,2016) for m in mes]
lag = 1
start = time.time()
for i in range(len(my)):
    # k = i + lag
    print(my[i])
    bdc = pd.read_csv("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera"+my[i]+".txt", sep='|')
    bdc.loc[~ bdc['MODULO'].isin([131,29]), :]
    bdc['SALDOUS'] = np.where(bdc['MONEDA']==0, bdc['SALDO']/6.86,bdc['SALDO'])
    bdc['MONTOUS'] = np.where(bdc['MONEDA']==0, bdc['MONTO']/6.86,bdc['MONTO'])
    bdc['PREVIUS'] = np.where(bdc['MONEDA']==0, bdc['MONTO']/6.86,bdc['MONTO'])
    bdc.loc[: ,['OPERACION','CTACLIENTE','MONTOUS','SALDOUS','PREVIUS','CALIFICACION']]
    
    bdc2 = pd.read_csv("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera"+my[k]+".txt", sep='|')
    bdc2.loc[~ bdc['MODULO'].isin([131,29]), :]
    bdc2['SALDOUS'] = np.where(bdc2['MONEDA']==0, bdc2['SALDO']/6.86,bdc2['SALDO'])
    bdc2['MONTOUS'] = np.where(bdc2['MONEDA']==0, bdc2['MONTO']/6.86,bdc2['MONTO'])
    bdc2['PREVIUS'] = np.where(bdc2['MONEDA']==0, bdc2['MONTO']/6.86,bdc2['MONTO'])
    bdc2.loc[: ,['OPERACION','CTACLIENTE','MONTOUS','SALDOUS','PREVIUS','CALIFICACION']]
end = time.time()
print(end-start)    
    