# -*- coding: utf-8 -*-
"""
Created on Mon Jul 17 16:17:36 2023

@author: 98acg
"""

import pandas as pd
import numpy as np
#%%
drinks = pd.read_csv("http://bit.ly/drinksbycountry", dtype = {'beer_servings':'float'})
print(drinks.dtypes)

print(drinks.index)
print(drinks.columns)
#Index are not columns they are part of the dataframe structure
#As well as columns, so when given no names the columns are labelled by numbers
#%%
#INDEXES
#They exist because of identification, selection and alignment
#IDENTIFICATION
#Notice that when making a query the indices remain unchanged
print(drinks[drinks['continent']=='South America'])
#SELECTION
#when using loc we reference rows and columns by labels. Example Bolivia is row 20
print(drinks.loc[20,:])

#SETTING USEFUL INDEXES
print(drinks.set_index('country',inplace=True))
print(drinks.index)
#SETTING INDEX AS COLUMNS AGAIN
drinks.index.name = 'country'
drinks.reset_index(inplace=True)
print(drinks.head())
#%%
#SERIES INDEX
#A series also has indices it takes them fron the df
drinks = pd.read_csv("http://bit.ly/drinksbycountry", dtype = {'beer_servings':'float'})
print(drinks.country)
drinks.set_index('country',inplace=True)
print(drinks.continent)
#Value counts for exmaple returns a Series with its own index
print("Value counts\n")
print(drinks.continent.value_counts())
print(drinks.continent.value_counts()['South America'])
#Sorting index
print(drinks.continent.value_counts().sort_index())
#%%
#ALIGNMENT
sleep = pd.read_csv("C:/Files/Data/SleepHealth/Sleep_health_and_lifestyle_dataset.csv")

counts = sleep.Gender.value_counts()
counts.name = 'GenderCounts2'
sleep.set_index('Gender',inplace=True)
sleep['GenderCounts'] = counts
#We can bind information automatically by index
#In this case the value counts has gender as index and then we set gender as the index of the 
#original df. Then when creating a new column GenderCounts it binds values based on index.

#we can add a new column with the concat function as well
sleep.reset_index(drop=False,inplace=True)
newdf = pd.Series([1,2],name='NewColumns')
#but notice that the indices have to be unique
print(pd.concat([sleep,newdf],axis=1).head())
#%%
#SELECTION WITH LOC ILOC AND IX
print(sleep.loc[0,:])
print(sleep.loc[[0,1,2,], :])
#Loc understand shortened lists with : notation and is inclusive in both sides
print(sleep.loc[0:2, :])
#column selection with : notation works too
print(sleep.loc[:,['Gender','Age','Occupation']])
print(sleep.loc[: , 'Gender':'Stress Level'])

#Filtering
print(sleep.loc[sleep.Gender=='Female', 'Gender':'Stress Level'])

#ILOC, it selects by integer position.
print(sleep.iloc[0:2,1:6])
#Notice that hwne using iloc with : notation it is exclusive from the second argument

#Practice to avoid 1. Column selection
print(sleep[['Gender','Stress Level']])
#instead use loc to make explicit that you want all rows from the selected columns
print(sleep.loc[:, ['Gender','Stress Level']])

#Practice to avoid 2. Row selection
print(sleep[0:2])
#instead use
print(sleep.iloc[0:2,:])
print(sleep.loc[0:2,:])

print("IX METHOD\n")
#IX method for selection. It figures out whether you are referring to names or indices
#print(sleep.ix[0:2, 'Gender':'Stress Level'])
#.ix is deprecated

#%%
#INPLACE parameter in PANDAS
#When looking at the documentation inplace=False by default will not make changes
#to the dataframe. It is useful for making test and trials in our df.
#Setting inplace to True it affects the underlying dataframe.

#However, the things we achieve with inplace=True can also be done with assignment

#sleep.set_index('Person ID',axis=1, inplace=True) #this is virtually equal to
sleep = sleep.set_index('Person ID')
print(sleep.head())