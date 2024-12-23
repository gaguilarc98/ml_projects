#%%
import sys
print(sys.version, sys.platform, sys.executable)

%reset -f
import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report
from sklearn.metrics import precision_recall_fscore_support
from sklearn.metrics import roc_curve
from sklearn.metrics import roc_auc_score
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score
from sklearn.metrics import precision_recall_curve
from sklearn.metrics import mean_squared_error
from sklearn.metrics import r2_score
from sklearn.model_selection import cross_val_score, KFold
from sklearn.calibration import calibration_curve
from sklearn.ensemble import AdaBoostClassifier
from sklearn.svm import SVR
from sklearn.feature_selection import RFE
from sklearn import tree
from numpy import mean
from numpy import std
from sklearn.datasets import make_regression
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import RepeatedKFold
from sklearn.preprocessing import StandardScaler
from matplotlib import pyplot as plt
from matplotlib.pyplot import figure
import matplotlib.style as style
from datetime import datetime
from sklearn.model_selection import RandomizedSearchCV, GridSearchCV
from sklearn.model_selection import StratifiedKFold
#from mlxtend.plotting import plot_decision_regions
import xgboost as xgb
import tqdm
import shap
from IPython.display import display, HTML
from xgboost import plot_tree
import graphviz
from functools import reduce
from treeinterpreter import treeinterpreter as ti
from sklearn.tree import DecisionTreeClassifier, export_graphviz
from sklearn import svm
import sklearn
from sklearn.ensemble import RandomForestClassifier, VotingClassifier, IsolationForest
import seaborn as sns
from imblearn.over_sampling import SMOTE
from collections import Counter
from plot_metric.functions import BinaryClassification
from numpy import sqrt
from numpy import argmax
from matplotlib import pyplot
#import ppscore as pps
# import keras
import pickle
# from keras import backend as K
# from keras.models import Sequential
# from keras.layers import Activation
# from keras.layers.core import Dense
# from keras.optimizers import Adam
# from keras.metrics import categorical_crossentropy
# from keras.callbacks import EarlyStopping
from sklearn import preprocessing
import smogn
import time
#%%
# Update month here
df = pd.read_csv('D:/!bso/vipRC/dataScore_feb2023_v2.csv')
df = df.set_index('CTACLIENTE', drop=True)

X = df.drop('target', axis = 1)
y = df[['target']]

X_train, X_test, y_train, y_test = train_test_split(X,y ,
                                   random_state=104, 
                                   test_size=0.25, 
                                   shuffle=True)

col_names = list(X_train.columns)
mm_scaler = preprocessing.StandardScaler()
mm_scaler.fit_transform(X_train)

X_train_mm = mm_scaler.transform(X_train)
X_train_mm = pd.DataFrame(X_train_mm, columns=col_names, index=X_train.index)
X_train_mm

X_test_mm = mm_scaler.transform(X_test)
X_test_mm = pd.DataFrame(X_test_mm, columns=col_names, index=X_test.index)
X_test_mm

X_full_mm = mm_scaler.transform(X)
X_full_mm = pd.DataFrame(X_full_mm, columns=col_names, index=X.index)
X_full_mm
# %%
# base logit
# Create and fit the logistic regression model
clf_logistic = LogisticRegression(solver='lbfgs', max_iter = 3000).fit(X, np.ravel(y))

# Create predictions of probability for loan status using test data
preds = clf_logistic.predict_proba(X)
clf_logistic_preds = clf_logistic.predict_proba(X)

# Create a dataframe for the probabilities of default
preds_df_lr = pd.DataFrame(preds[:,1], columns = ['prob_GC'], index=X.index)
clf_logistic_preds = pd.DataFrame(preds[:,1], columns = ['prob_GC'])

# Reassign loan status based on the threshold
preds_df_lr['goodClient'] = preds_df_lr['prob_GC'].apply(lambda x: 1 if x > 0.43 else 0)
# Print the confusion matrix, we should show the updated matrix somewhere
print(confusion_matrix(y,preds_df_lr['goodClient']))

# Print the row counts for each loan status
print(preds_df_lr['goodClient'].value_counts())

# Print the classification report, maybe show it too?
target_names = ['Non-Default', 'GoodClient']
print(classification_report(y, preds_df_lr['goodClient'], target_names=target_names))
print(precision_recall_fscore_support(y,preds_df_lr['goodClient'])[0:1])
# Store the number of loan defaults from the prediction data
num_defaults = preds_df_lr['goodClient'].value_counts()[1]
# Store the default recall from the classification report
default_recall = precision_recall_fscore_support(y,preds_df_lr['goodClient'])[1][1]
# Create predictions and store them in a variable
preds = clf_logistic.predict_proba(X)

# Print the accuracy score the model
print(clf_logistic.score(X, y))

# Compute the AUC and store it in a variable
#auc = roc_auc_score(y, prob_default)
# %%
#calculate precision and recall
# precision, recall, thresholds = precision_recall_curve(y, preds_df_lr['prob_GC'])
# #create precision recall curve
# fig, ax = plt.subplots()
# ax.plot(recall, precision, color='purple')

# #add axis labels to plot
# ax.set_title('Precision-Recall Curve')
# ax.set_ylabel('Precision')
# ax.set_xlabel('Recall')

# #display plot
# plt.show()

# %%
# Update month before exporting to csv
finalDF = pd.concat([X, y, preds_df_lr], axis = 1)
pd.crosstab(finalDF['target'], finalDF['goodClient'])
# finalDF.to_excel('D:/!bso/vipRC/vipList_wScores_nov2022.xlsx')
finalDF.to_csv('D:/!bso/vipRC/vipList_wScores_feb2023_v2.csv')
# %%
# These are the regression parameters
# Update month before exporting to csv
clf_logistic = LogisticRegression(solver='lbfgs', max_iter = 3000).fit(X, np.ravel(y))

coeff_table = pd.DataFrame(np.transpose(clf_logistic.coef_), columns = ['Coef']) 
labs_table = pd.DataFrame(np.transpose(clf_logistic.feature_names_in_) , columns = ['Feats'])
clfTab = pd.concat([labs_table, coeff_table], axis = 1)
r2 = r2_score(y, preds_df_lr['goodClient'])
clfTab.to_csv('D:/!bso/vipRC/clfTab_feb2023_v2.csv')
# %%
