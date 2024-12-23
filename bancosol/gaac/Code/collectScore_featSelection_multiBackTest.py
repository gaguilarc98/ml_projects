# %%
#%reset - f
from modelFunctions import *
import time
import smogn
from sklearn import preprocessing
from keras.callbacks import EarlyStopping
from keras.metrics import categorical_crossentropy
from keras.optimizers import Adam
from keras.layers.core import Dense
from keras.layers import Activation
from keras.models import Sequential
from keras import backend as K
import pickle
import keras
from matplotlib import pyplot
from numpy import argmax
from numpy import sqrt
from plot_metric.functions import BinaryClassification
from collections import Counter
from imblearn.over_sampling import SMOTE
import seaborn as sns
from sklearn.ensemble import RandomForestClassifier, VotingClassifier, IsolationForest
import sklearn
from sklearn import svm
from sklearn.tree import DecisionTreeClassifier, export_graphviz
from treeinterpreter import treeinterpreter as ti
from functools import reduce
import graphviz
from xgboost import plot_tree
from IPython.display import display, HTML
import shap
import tqdm
import xgboost as xgb
from sklearn.model_selection import StratifiedKFold
from sklearn.model_selection import RandomizedSearchCV, GridSearchCV
from datetime import datetime
import matplotlib.style as style
from matplotlib.pyplot import figure
from matplotlib import pyplot as plt
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import RepeatedKFold
from sklearn.model_selection import cross_val_score
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.datasets import make_regression
from numpy import std
from numpy import mean
from sklearn import tree
from sklearn.feature_selection import RFE
from sklearn.svm import SVR
from sklearn.ensemble import AdaBoostClassifier
from sklearn.calibration import calibration_curve
from sklearn.model_selection import cross_val_score, KFold
from sklearn.metrics import r2_score
from sklearn.metrics import mean_squared_error
from sklearn.metrics import precision_recall_curve
from sklearn.metrics import accuracy_score
from sklearn.metrics import confusion_matrix
from sklearn.metrics import roc_auc_score
from sklearn.metrics import roc_curve
from sklearn.metrics import precision_recall_fscore_support
from sklearn.metrics import classification_report
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
import pandas as pd
import numpy as np
#%%
import sys

