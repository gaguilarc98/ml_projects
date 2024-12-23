# -*- coding: utf-8 -*-
"""
Created on Sun Dec 17 19:04:43 2023

@author: 98acg
"""
#%%
# PACKAGES
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
#%%
####____BASIC PLOTS WITH MATPLOTLIB____####
# * Visualization
# * Data structure
# * Control structures
##
df = pd.read_excel("C:/Files/Data/excel/Ventas2.xlsx")
#%%
plt.plot(df['Año'],df['Ventas Global'])
plt.show()

plt.scatter(df['Año'],df['Ventas Global'])
plt.show()
#%%
#HISTOGRAM
plt.clf()
plt.hist(df['Año'], bins=15) #By default its 10 bins
#CUSTOMIZE PLOTS
#Changes can be made in: sizes, colors, shapes, labels, axis and so on
plt.show()
plt.clf()
#Label your axis
plt.hist(df['Año'], bins=15) 
plt.xlabel('Year')
plt.ylabel('Count')
plt.title('Histogram of Videogame count along years')
plt.yticks([0,500,1000,1500,2000,2500,3000],
           ['0','0.5 M','1.0 M','1.5 M','2.0 M', '2.5 M', '3.0 M'])
plt.show()

#%%
#HISTOGRAM WITH LOG SCALE
plt.clf()
plt.hist(np.log(df['Ventas Global']), bins=15) #By default its 10 bins
plt.show()
#%%
#Size and Color
plt.clf()
plt.scatter(df['Año'], df['Ventas EU'], s=df['Ventas JP'])
plt.text(1995,10,'China')
plt.grid(True)
plt.show()
#%%
####____DICTIONARIES____####
country = ['Argentina','Bolivia','Brazil','Chile','Colombia','Ecuador','Paraguay','Peru','Uruguay','Venezuela']
land_area = [2780400,1098581,8514877,756102,1141748,256369,406752,1285216,176215,912050]
pop = [46621847,12186079,218689757,18549457,49336454,17483326,7439863,32440172,3416264,30518260]

#We cna use index to select a country and search for its corresponding population or land area
ind_bol = country.index('Bolivia')
print(land_area[ind_bol])
#However it is cumbersome, we could avoid using indexes
country_area = {'Argentina':2780400,
       'Bolivia':1098581,
       'Brazil':8514877,
       'Chile':756102,
       'Colombia':1141748,
       'Ecuador':256369,
       'Paraguay':406752,
       'Peru':1285216,
       'Uruguay':176215,
       'Venezuela':912055}
print(country_area.keys())
print(country_area['Bolivia'])
#The keys must be unique and immutable (cannot be change after they are created)
#Therefore lists cannot be keys
country_area['Venezuela'] = 912050 #This updates rather than overwrite the value for Venezuela
country_area['Cuba'] = 109884 #This creates Cuba
print(country_area['Cuba'])
del(country_area['Cuba'])
print(country_area.keys())
print('Cuba' in country_area)
#Lists are indexed by a range of numbers, while dictionaries are indexed with unique keys
#%%
####____PANDAS____####
#Usually data comes in tabular form
#Where each row is an observation and each observation has the same variables
#numpy ND arrays will not do the job as they only handle Series of the same type of variable
#pandas, built on the numpy package, is a high level data structure manipulation package.
#pandas DataFrames are the object that makes working with tabular data easier with pandas.
south_america = {'Country':country,
                 'LandArea':land_area,
                 'Population':pop}
south_df = pd.DataFrame(south_america)
#We can set the index manually
ind = ["AR","BO","BR","CH","CO","EC","PA","PE","UR","VE"]
south_df.index = ind
print(south_df.index)

#But usually we import data from a file.
sleep = pd.read_csv("C:/Files/Data/SleepHealth/Sleep_health_and_lifestyle_dataset.csv", index_col=0)

####____SELECTION____####
#we can use loc and iloc
print(sleep['Gender'])
print(type(sleep['Gender'])) #All columns are pandas Series individually. They are 1D labelled arrays
print(sleep[['Gender']])
print(type(sleep[['Gender']])) #Using double square brackets gives a DataFrame as a result

print(sleep[['Gender','Age']])
print(sleep[0:3])
#Using these squared brackets has limited functionality
#They exclude the upper limit as we saw in the first course,
#We would like to refer to the vlaues as array[row, col]

#With PANDAS we must use:
# * loc[]  label based
# * iloc[] integer-position based
print("Data for person ID 2, 3")
print(sleep.loc[[2,3]]) #To select records with Person ID 2 and 3
#To select columns
print(sleep.loc[[2,3],['Gender','Age']])
#%%
#To sum up
#Single squared brackets can be used for:
# * Column acces based on names:
print(sleep[['Gender','Age']])
# * Row access only through slicing:
print(sleep[1:4])
#loc is more versatile, but we need to use the labels.
#We can select rows, columns sepparately or at the same time

#iloc
print(sleep.iloc[[1,2,3],[0,1]])
print(sleep.iloc[[1,2],:])
#%%
####____COMPARISON OPERATORS____####
#comparing numpy arrays element-wise with a value is easy
print(sleep['Age']>29)
print(sleep.iloc[np.arange(1,10),[1]]>28)
####____BOOLEAN OPERATORS____####
#and
print(7<10 and 7>4)
#or
print(7<3 or 7>3)
#not
print(not 7<3)
#Numpy arrays are a little bit different
#The folllowing is an error
#print(sleep.iloc[[1,2],[1]]>28 and sleep.iloc[[1,2],[1]]<30)
#We need the numpy logical operators
# * np.logical_and()
# * np.logical_or()
# * np.logical_not()
print(np.logical_and(sleep.iloc[[1,2],[1]]>27, sleep.iloc[[1,2],[1]]<31))

####____IF ELIF ELSE____####
z = 5
if z%2==0:
    print("z is even")
else:
    print("z is odd")
#%%
####____FILTERING PANDAS DATAFRAMES____####
#We need a Series of booleans
#The following lines give the same output
print(sleep['Age']>50)
print(sleep.loc[:,'Age']>50)
print(sleep.iloc[:,1]>50)
adult = sleep.iloc[:,1]>50
print("List of adults")
print(sleep[adult])

print("List of nurse adults")
print(sleep[np.logical_and(sleep['Age']>50, sleep['Occupation']=="Nurse")])
#%%
####_____WHILE LOOP____####
'''
while condition:
    expression
    
for var in seq:
    expression
'''
country = ['Argentina','Bolivia','Brazil','Chile','Colombia','Ecuador','Paraguay','Peru','Uruguay','Venezuela']
land_area = [2780400,1098581,8514877,756102,1141748,256369,406752,1285216,176215,912050]
pop = [46621847,12186079,218689757,18549457,49336454,17483326,7439863,32440172,3416264,30518260]
la = land_area[0]
i = 0
while la > 1000000:
    print(country[i])
    i+=1
    la = land_area[i]

#for loop enumerating each element in a list
for index, la in enumerate(land_area):
    print("index "+ str(index)+": "+str(la))

#for loop for strings
for c in "Bolivia":
    print(c.capitalize())

#for loops for list of lists
house = [["hallway",11.25],["kitchen",18.0],["living room",20.0],["bedroom",10.75],["bathroom",9.50]]

for h in house:
    print("The "+str(h[0])+" is "+str(h[1])+" sqm")    
####____LOOPS FOR DICTIONARIES____####
#.items() method of dicitionaries creates pairs of keys and values
for c,d in country_area.items():
    print(str(c)+" has a land area of "+str(d)+" km2")
####____LOOPS FOR NUMPY ARRAYS____####
np_height = np.array([1.73,1.68,1.71,1.89,1.79])
np_weight = np.array([65.4,59.2,63.6,88.4,68.7])
bmi = np_weight/np_height**2
for val in bmi:
    print(val)
    
meas = np.array([np_height,np_weight])
for val in meas:
    print(val) #This prints  the whole height and weight array instead of each element

#for loop for single items in 2D np.arrays
for m in np.nditer(meas):
    print(m)
####____LOOPS FOR DATAFRAMES____####
for name in sleep:
    print(name) #This prints the name of each column

#for loop for each row in a dataframe
print("OLD NURSES LIST")
nurses = sleep[np.logical_and(sleep['Age']>58, sleep['Occupation']=="Nurse")]
print(nurses)

for lab, row in nurses.iterrows():
    #Creating Series on every iteration
    print(str(lab)+" has a sleep duration of "+str(row["Sleep Duration"]))
    nurses.loc[lab,"sleep_length"] = len(row["Sleep Disorder"])
    
#This is inefficient, the better option is apply

#%%
####_____CREATING A COLUMN USING APPLY____#####
sleep['OccupationLength']= sleep['Occupation'].apply(len)
print(sleep.info())
print(sleep.head())

#%%
####____RANDOM NUMBERS____####
#Inside numpy package there is a random package
np.random.rand()
 