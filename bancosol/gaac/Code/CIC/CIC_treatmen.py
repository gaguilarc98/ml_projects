# -*- coding: utf-8 -*-
"""
Created on Wed Jun 21 13:34:25 2023

@author: gaguilarc
"""

#%%
####____PACKAGES____####
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
#%%
namesk = pd.read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet_name="CR-K").iloc[:,1:3]

cick = pd.read_csv("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/13_CIC/CIC 2023/CIRC ENVIO 2023.01 ENERO/CR20230131H.IBBSO",names=namesk.NAME)
