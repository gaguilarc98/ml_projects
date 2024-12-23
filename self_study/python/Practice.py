# -*- coding: utf-8 -*-
"""
Created on Mon Dec 25 17:43:59 2023

@author: 98acg
"""
#%%
####____LOADING PACKAGES____####
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
#%%____IMPORTING DATA
breast_names = pd.read_csv("C:/Files/Data/breastcancer/breast_names.csv", sep=";")

breast = pd.read_csv("C:/Files/Data/breastcancer/wdbc.data", names=breast_names.Variable_Name)
breast.info()
#%%____DATA EXPLORATION
breast.value_counts('Diagnosis')

#%%ROOM OCCUPANCY DATASET

room = pd.read_csv("C:/Files/Data/roomoccupancy/Occupancy_Estimation.csv")

room["TOD"] = room.