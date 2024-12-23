# -*- coding: utf-8 -*-
"""
Created on Sun Jul 16 17:15:02 2023

@author: 98acg
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
#%%
bdc = pd.read_csv("C:/Files/Data/test.txt", sep="|")

#bdc = bdc.loc[:,["CTACLIENTE","OPERACION","saldous","previus","CALIFICACION"]]

#It is possible to use dot notation to reference column names
#However it does not work if the name is a method name, i.e. if the column name
#is "shape" calling df.shape does not return the shape pandas Series, instead
#It returns the shape of the DataFrame

#HOWEVER when creating a new column always use bracket notation, not dot notation.

bdc["new"] = bdc.rand/bdc.number

#Attributes are named with dot notation with no parenthesis
#Methods are called after a dot with parenthesis (which could take arguments)

bdc.dtypes

bdc.describe(include=("object"))

bdc.rename(columns={"dates":"Fecha","number":"Numero","cat":"Categorical",
                    "rand":"NumeroAleatorio","nn":"NotANumber","new":"Fraccion"},inplace=True)

#The inplace arguments tells pandas to change names in current object.

#Or if you want to replace all names:
namecols = ["fecha","numero","categorical","aleatorio","notNumber","fraccion"]

bdc.columns = namecols

#Or renaming when reading the file
bdc = pd.read_csv("C:/Files/Data/test.txt",sep='|',names=namecols[0:5], header=0)

#Tip if you want to change the column names by a pattern remember that the columns
#Attribute is mutable and is only a string Series so any string method will work.
bdc.columns = bdc.columns.str.upper()

#%%
#Dropping columns
bdc['FRACCION'] = bdc.ALEATORIO/bdc.NUMERO
bdc.drop("FRACCION",axis = 1,inplace=True)
#Axis 0 is the row axis and 1 is the column axis
#It works for multiple columns as well
bdc["VAR1"]  = bdc.ALEATORIO/10000
bdc["VAR2"]  = bdc.NUMERO*1000
print(bdc.info())
bdc.drop(["VAR1","VAR2"],axis=1, inplace=True)
print(bdc.info())
#%%
#Sorting Series or DataFrames
Ser = bdc["ALEATORIO"]
print(Ser.info())
print(Ser.sort_values(ascending=False))
type(Ser)
type(bdc)

print(bdc.ALEATORIO.sort_values(ascending=False))
#Sorting a df by a Series values

bdc.sort_values("ALEATORIO")#However it does not keep changes in bdc
print(bdc)
bdc.sort_values("ALEATORIO",inplace=True)#This saves changes to bdc
print(bdc)
#Sorting by two columns
bdc.sort_values(["CATEGORICAL","ALEATORIO"],ascending=False,inplace=True)
print(bdc)
#%%
#Filter rows by a column value
#the logic says get a Series of logical vlaues (booleans) that tell if the row
#meets the conditions required to keep it in the df
boolList = []
for cat in bdc.CATEGORICAL:
    if cat=="1. Soñar":
        boolList.append(True)
    else:
        boolList.append(False)
serList = pd.Series(boolList)
print(serList.head())
print(bdc[serList]) #Normally a bracket is to select a column, but if you pass
#A boolean Series it is interpreted as row selection, but it has to be the same size as th df
print(bdc[bdc["CATEGORICAL"]=='1. Soñar'])

#Since the output of this object is a DF you can use all we have learned up to now
#Like dot/bracket notation for selection or methods for df like

print(bdc[bdc["CATEGORICAL"]=='1. Soñar'].FECHA.sort_values(ascending=False))

#%%
#loc notation
#Sometimes selecting rows and columns the way showed before causes trouble
#The "better" way to do this is with the .loc method.
#This method basically lets you select rows and columns by name
#With the understanding that rows have names that are their index
print(bdc.loc[bdc["CATEGORICAL"]=="1. Soñar","NUMERO"])
#%%
#Multiple criteria filter for DataFrames
print(bdc[(bdc['CATEGORICAL']=="1. Soñar") & (bdc["NUMERO"]>20)])
#We need amperstnad & or pipe | to tell and /or in pandas
#We also need parenthesis for each condition to tell order of evaluation
print(bdc[(bdc['CATEGORICAL'].isin(["1. Soñar","2. Jürgen"])) & (bdc["NUMERO"]>20)])

#The .isin() method for Series asks for a list to search the Series values in that list
