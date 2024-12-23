# -*- coding: utf-8 -*-
"""
Created on Wed Dec 13 21:39:22 2023

@author: 98acg
"""
#%%
####____INTRODUCTION TO PYTHON_____####
"""
Python is a general purpose language. It is open source and free.
There ara python packages almost for anything in any field, including data science
"""
####____VARIABLES AND TYPES____####
"""
Python is case-sensitive name. You can call a value by its variable name.
"""
height = 1.79
weight = 74.2
bmi = weight/height**2
print(bmi)
print(type(bmi))
#The most common python types are strings, booleans.
#Depending on the data type some operators may behave differently
print(4+5)
print('a'+'b')
#%%
####____LISTS IN PYTHON____####
#A list is a collection of objects. They can be of any type, even lists.
CompList = [[1,2,3],['a','b','c']]
print(CompList)
####____SUBSETTING LISTS____####
#Each element of the list has an associated index. They start at zero.
print(CompList[0])
print(CompList[-1])
print(CompList[-1][1])
####____SLICES OF LISTS____####
print("SLICES")
print(CompList[0][0:2]) #the last index is not included in the output
print(CompList[0][0:]) 
print(CompList[0][:2])
print(CompList[-2])
####____MANIPULATING LISTS____####
#Change elements, add or remove elements 
print("Manipulating Lists")
CompList[1] = [True,False,True]
print(CompList)
#Add elements
CompList = CompList + [["Hello","World"]]
#Delete elements
del(CompList[1][0])
print(CompList)
#Naming variables only references the allocation of memory
#The object does not contain the elements themselves
print("Watch changing values of a copy list")
y = CompList #This Copies the reference to the list and not the actual values themselves
y[0] = [["N","Y","M"]] #Updating changes the elements for both (all) references to the list
print(CompList)
#Compare to 
z = list(CompList)
z[1] = [[21,22,23]]
print(CompList)
w = CompList[:]
w[2] = [["Donald","Trump"]]
print(CompList)
 #%%
 ####____FUNCTION____####
 #It is a chunck of reusable code.
 #Examples are type(), len(), max(), min()
 print(round(1.68,1))
 print(round(1.68)) #The second option is an OPTIONAL ARGUMENT
 print("Sort function")
 print(sorted([4,2,4,5,1]))
 print(sorted([4,2,4,5,1], reverse=True))
 ####____METHODS____####
 #They are functions that belong to objects.
 print(CompList.index([False, True]))
 #Methods for astring
 string  = "Hello World"
 print(string)
 print(string.capitalize())
 print(string.replace("H","Y"))
 #Objects can have different methods depending of the type of variable.
 CompList.append(["New","Object"]) #Some methods change the object itself
 print(CompList)
 print(CompList.reverse())
 ####____PACKAGES____####
 #They are like a directory of python scripts.
 import numpy as np
 nparray = np.array([1,2,3])
 print(type(nparray))
 #Compared to
 from numpy import array
 npnewarray = array(["a","b","c"])
 print(npnewarray.shape)
 #%%
 ####____THE NUMPY PACKAGE____####
 #Needs for Data Science
 # * Mathematical operations over collections
 # * Speed
 height = [1.72, 1.74,1.65]
 weight = [61, 68, 60]
 #weight/height**2 #Python does not recognize this operation as being element-wise
 
 npheight = np.array(height)
 npweight = np.array(weight)
 #Numpy arrays can perform calculations over entire arrays
 
 print(npweight/npheight**2)
 
 bmi = npweight/npheight**2
 #Selection and subsetting
print(bmi[0:2])
print(bmi[bmi>22])
####____2D Numpy Arrays____####
 
np_2d = np.array([height,weight]) 
print(np_2d)
print(np_2d.shape)
#Subsetting
print(np_2d[0][2])
print(np_2d[0,2])#This syntax is more intuitive
print(np_2d[:,2])

####____BASIC STATISTICS____####
city = np.array([[1.72,60],[1.80,80],[1.68,65],[1.75,69]])
print(np.mean(city[:,0]))
print(np.mean(city[:,1]))

height  = np.round(np.random.normal(1.75,0.20,5000),2)
weight  = np.round(np.random.normal(70,10,5000),2)

npcity = np.column_stack((height,weight))

print("Height Mean: "+str(np.mean(npcity[:,0])))
print("Weight Median:"+str(np.median(npcity[:,1])))