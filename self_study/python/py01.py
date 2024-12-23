# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
#%%
a = np.array([1,2,3])
print(a.dtype,
      a.ndim,
      a.size,
      a.shape)
b = np.array([[1,2,3],[4,5,6]])
print(b.dtype,
      b.ndim,
      b.size,
      b.shape)
print(b)
#%%
c = np.array([[1,2,3],(4,5,6),[7,8,9]])
print(c.dtype)
d = np.array([(1,2,3),(4,5,6),(7,8,9)])
print(d.dtype,
      d.shape,
      d.size,
      d.ndim)
e = np.array([['a','b'],['c','d']])
print(e)
#shape shows an array with the number of observations on each dimension
#ndim shows the number of dimensions
#size shows the number of elements in an array

#np.zeros((nrow,ncol)) creates an array for zeros, where (nrow,ncol) is the shape
#np.ones works the same as np.zeros
#np.arange(start, end, step) works like seq from r
#np.linspace(start, end, size,endpoint=True) works like seq with length from r
#the argument endpoint by default makes the end point included in the interval.
#np.random is a module of the numpy package which has the function random(nrow,ncol)
#to generate an array of random numbers where ncol,nrow is its shape

#%%
#The np.random module
#This module has functions like np.random.normal(loc,scale,shape) where loc stands
#for mean, scale stands for standard deviation and shape could be a single number
#indicating the size or a tuple/sequence/array indicating the shape.
nn = np.random.normal(0,1,(2,2))
#Also it is posible to change the shape of an np.array by assignment. If a is an np.array
#a.shape= (nrow,ncol) will change the shape of so long as the shape does not
#interfere with the size.
g = np.arange(1,7)
#There are two ways to change the shape of g
g.shape=(2,3)
print(g)
g.reshape((3,2)) #where the tuple indicates the shape
print(g)
#But the second is smor flexible as it allows to change the shape as a function
g = np.arange(1,7).reshape((3,2),order='F')
#where ther order can take the values 'C'to fill all columns before the rows
#and 'F' to fill all rows before new columns
c=np.arange(1,10).reshape((3,3))
d=c.copy()
c*d
#This multiplies c times d element by element. To get matrix multiplication
#use the dot functions instead:
np.dot(c,d)
c.dot(d)
#%%
#Increment and decrement operators these make calculations in an array and store
#it in the same array
c+=1
c-=1
c*=2
#ufunc. A universal functionis one that operates on an array element-wise to generate
#a new array withe the same shape. NumPy has some of them
np.sin(c)
np.log10(c)
#%%
#Aggregate funcions
#Those perform an operation on a set of values to produce a single result
np.mean(c)
c.var()
d.std()
np.min(c)
print(c/d)
np.max(c/d)
#%%
#Indexing, slicing
#It is possible to access an element by its position in the array
#Note that the numbering start at zero
c[0]
#Also note that in an array putting a single number will call the whole row
c[0,0]
#while putting the enough indexes will retrieve the element
#There are also negative indexes
c[-1]
#The largest negative being the first and -1 being the last this ones is quite helpful
f=np.array([1,5,6,7,8])
f[-1]
#It is possible o access some elements by indexing their positions
print(f[[0,3,4]])
#Slicing
f[0:3] #we use double dots to initiate a sequence of index start:end:step
f[0:5:2]
f[5:0:-1]
#also if one of the arguments is blank the default is first:last:1
#Multi dimensional arrays
c[:,2] #gets third column
c[2]#gets third row
c[2,:]#gets third row
c[2,2]#gets element from third row and third column
c[(2,2)]#same as previus but unnecessary
c[[0,2],1:]
#%%
#iterating in an array. For that matter we use the for construct
for i in f:
    print(i) #to access each element of a vector

for row in c:
    print(row) #for each row in an array
    
for item in c.flat:
    print(item) #to access each element in an array
    
#To convert an array into a vector:
g = c.reshape(c.size)
print(g.ndim,
      g.size,
      g.dtype,
      g.shape)
#Note: slicing and assignment does not create a separate object
#in order to do so use the copy() function
h = c[[1,2],:]
h[0,0] = 1000
#np.apply_along_axis(func, direction, array) function 
np.apply_along_axis(min, 1, c)
#func can be an aggregate function or an ufunc. The second argument tells how the
#function should be applied 1 is rowwise and 0 is columnwise
#%%
fruits = ['banana','apple','banana','apple','orange','orange','watermelon']
weight = [20,40,15,45,30,25,70]
volume = [10,6,8,5,5,6,24]
df = pd.DataFrame({'frutas':fruits,'peso':weight,'volumen':volume})

df.agg(Nfruits=('frutas','nunique'))
df.groupby('frutas').agg(Average_weight = ('peso','mean'),
                         Max_weight = ('peso','max'),
                         N_frutas = ('peso','count'))