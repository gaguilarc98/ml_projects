# -*- coding: utf-8 -*-
"""
Created on Mon Jul 17 12:32:25 2023

@author: 98acg
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
#%%
#CREATING DATA FRAME FROM SCRATCH
sleep = pd.read_csv("C:/Files/Data/SleepHealth/Sleep_health_and_lifestyle_dataset.csv")
sleep.info()
sleep.describe(include=['object'])

#%%
#Using axis parameter correctly
drinks = pd.read_csv("http://bit.ly/drinksbycountry")
#Dropping a column
print(drinks.drop("country",axis=1).head())
#Dropping a row
print(drinks.drop(2,axis=0).head())

#When using a summary function like mean() it is doing so in the 0 axis by default
#that is moving along columns, but we can specify the axis to 1 to apply the funcion
#along the columns
print(drinks.mean(axis=0))
print(drinks.mean(axis=1).head())

#The axis actually have nicknames to be used as a parameter
print(drinks.mean(axis='index'))
print(drinks.mean(axis='columns').head(6))
#%%
#STRING METHODS
sleep.info()
#In pandas string methods start with .str
print(sleep.Gender.str.upper())
print(sleep.Occupation.str.contains('Engineer'))
#select all engineers with contatins
print(sleep[sleep.Occupation.str.contains('Engineer')])
#replace " " tih underscore "_"
print(sleep.Occupation.str.replace(' ','_'))
#Regular expressions
print(sleep.Occupation.str.replace('@ Engineer','Engineer'))

#%%
#CHANGE DATA TYPES OF PANDAS SERIES
print(drinks.dtypes)
#change beer_servings to float
drinks['beer_servings'] = drinks.beer_servings.astype(float)
print(drinks.dtypes)
#Change types when reading we use dtype argument in the read_csv and pass a dictionary
print('During reading\n')
drinks = pd.read_csv("http://bit.ly/drinksbycountry", dtype = {'beer_servings':'float'})
print(drinks.dtypes)
#%%
#GROUPBY method
print(sleep.head())
#Mean age by gender 
print(sleep.groupby('Gender')['Age'].mean())
print(sleep.groupby('Gender').Age.mean())
#Multiple aggregation functions at once
print(sleep.groupby('Gender').Age.agg(['count','min','max','mean']))
#No need to specify column
sleep.groupby('Gender').Age.mean().plot(kind='bar')
#%%
#EXPLORE PANDAS SERIES
dfList = []
flist = ['test','test2']
for i  in range(len(flist)):
    df = pd.read_csv("C:/Files/Data/txt/"+flist[i]+".txt",sep='|')
    dfList.append(df)
dfFull = pd.concat(dfList, axis=0).reset_index(drop=True)
print(dfFull)

print(dfFull.cat.value_counts())
#Turn the into percentages
print(dfFull.cat.value_counts(normalize=True))
#Show unique values
print(sleep.Occupation.unique())

#Cross tabulation
print(pd.crosstab(sleep.Occupation, sleep.Gender))
#Histograms
sleep.columns = sleep.columns.str.replace(' ','_')
sleep.Heart_Rate.plot(kind="hist")
sleep.Occupation.value_counts().plot(kind='bar')

#%%
#MISSING VALUES
#Detecting nulls
print(sleep.isna().astype(int).sum())
print(sleep.notnull().astype(int).sum())
print(sleep.isnull().astype(int).sum())
print(sleep.isnull().sum()) #Converts True to 1 and False to 0

sleep.dropna(axis=1, how='all') #Drops columns when all entries are missing values
sleep.dropna(axis=0, how='any') #Drops rows when any entry is missing
#Drop when missings are in some columns

sleep.iloc[1,1] = np.NaN
print(sleep.head())

print(sleep.dropna(subset=['Gender'], axis = 0, how='any')) #Drop rows where Gender is missing

print(sleep.Gender.value_counts(dropna=False))
sleep.Gender.fillna(value = 'Unknown',inplace=True) #to fill missing values
print(sleep.Gender.value_counts())

