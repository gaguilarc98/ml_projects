# %%
#%reset - f
from modelFunctions import *
import time
import smogn
from sklearn import preprocessing
# from keras.callbacks import EarlyStopping
# from keras.metrics import categorical_crossentropy
# from keras.optimizers import Adam
# from keras.layers.core import Dense
# from keras.layers import Activation
# from keras.models import Sequential
# from keras import backend as K
import pickle
# import keras
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

import joblib

import sys

print(sys.version, sys.platform, sys.executable)
# from mlxtend.plotting import plot_decision_regions
# import ppscore as pps
#%%
df = pd.read_csv(
    'D:/!bso/scoreCobranza/data_score_cobranza_abr2023_Aonly.csv')
# df2 = df.replace([np.inf, -np.inf], np.nan, inplace=True)
# df2 = df.dropna(axis=0).reset_index(drop=True)
# df = df.set_index('OPERACION', drop=True)

df['AGENCIA'] = df['AGENCIA'].astype(str)
# df['CIU'] = df['CIU'].astype(str)
# df['CAEDEC_DEST'] = df['CAEDEC_DEST'].astype(str)
df['MONEDA'] = df['MONEDA'].astype(str)
df['OBJETO_CRED'] = df['OBJETO_CRED'].astype(str)
# df['PATRIMONIO'] = np.where(df['MONEDA'] == '0', df['PATRIMONIO']/6.86, df['PATRIMONIO'])
# df['ING_VENTAS'] = np.where(df['MONEDA'] == '0', df['ING_VENTAS']/6.86, df['ING_VENTAS'])
df.dtypes

# Create two data sets for numeric and non-numeric data
df_num = df.select_dtypes(exclude=['object'])
df_str = df.select_dtypes(include=['object'])

# One-hot encode the non-numeric columns
df_str_onehot = pd.get_dummies(df_str)

# Union the one-hot encoded columns to the numeric ones
cr_loan_prep = pd.concat([df_num, df_str_onehot], axis=1)

# Print the columns in the new data set
print(cr_loan_prep.columns)
cr_loan_prep = cr_loan_prep.set_index('OPERACION', drop=True)
del df
del df_num
del df_str
#%%
# Create the X and y data sets
X = cr_loan_prep.drop(['det'], axis=1)
y = cr_loan_prep[['det', 'tr']]
X_train = X.loc[X['tr'] == 1]
X_train = X_train.drop(['tr'], axis=1)
X_test_full = X.loc[X['tr'] > 1]

#%%
X_test = X.loc[X['tr'] == 12]
X_test = X_test.drop(['tr'], axis=1)

y_train = y.loc[y['tr'] == 1]
y_train = y_train.drop(['tr'], axis=1)
y_test = y.loc[y['tr'] == 12]
y_test = y_test.drop(['tr'], axis=1)
y_test_full = y.loc[y['tr'] > 1]

X_testDic = dict(tuple(X_test_full.groupby('tr')))
y_testDic = dict(tuple(y_test_full.groupby('tr')))

#%%
# Scaling
col_names = list(X_train.columns)
mm_scaler = preprocessing.MinMaxScaler()
X_train_mm = mm_scaler.fit_transform(X_train)
X_train_mm = pd.DataFrame(X_train_mm, columns=col_names)
X_test_mm = mm_scaler.transform(X_test)
X_test_mm = pd.DataFrame(X_test_mm, columns=col_names)

# %%
# Feature selection
columns = list(X_train_mm)
auc_list_t = []
colname_t = []
for i in columns:
    # Picking one feature at a time
    X_train_v = pd.DataFrame(X_train_mm[i])
    X_test_v = pd.DataFrame(X_test_mm[i])

    dt = DecisionTreeClassifier(max_depth=4)

    # Fit dt to the training set
    dt.fit(X_train_v, y_train)

    # Predict test set labels
    _tree = dt.predict_proba(X_test_v)
    # Create a dataframe for the probabilities of default
    _tree2 = pd.DataFrame(_tree[:, 1], columns=['prob_delinq'])
    # Reassign loan status based on the threshold
    _tree2['pred_delinq'] = _tree2['prob_delinq'].apply(
        lambda x: 1 if x > 0.5 else 0)

    # start_time = time.timer(None)
    # time.timer(start_time) # timing ends here for "start_time" variable

    # Print the confusion matrix
    print(confusion_matrix(y_test, _tree2['pred_delinq']))
    cm = confusion_matrix(y_test, _tree2['pred_delinq'])
    recall = cm[1, 1]/(cm[1, 1] + cm[1, 0])
    # print(cm)
    print('Recall is ' + str(recall))
    # Compute the AUC and store it in a variable
    auc_lr_del = roc_auc_score(y_test, _tree2['prob_delinq'])
    auc_list_t.append(auc_lr_del)
    colname_t.append(i)
    print(i)
    # Print the auc for the model
    print(X_train_v.columns)
    print('The AUC is %s' % np.round(auc_lr_del, decimals=4))

df_auc = pd.DataFrame(auc_list_t, columns=['auc'])
df_cols = pd.DataFrame(colname_t, columns=['var'])
df_auc = pd.concat([df_auc, df_cols], axis=1)
df_auc['keep'] = df_auc['auc'].apply(lambda x: 1 if x >= 0.51 else 0)
print(df_auc['keep'].sum())
df_keep = df_auc[df_auc['keep'] == 1]
df_drop = df_auc[df_auc['keep'] == 0]
keep_list = df_keep['var'].tolist()
drop_list = df_drop['var'].tolist()

X_train_fe = X_train_mm[keep_list]
# X_u_fe = X_u[keep_list]
# X_smote_fe = X_smote[keep_list]
X_train_oo = X_train_mm[drop_list]

X_test_fe = X_test_mm[keep_list]
X_test_full_fe = X_test_full[keep_list]
keep_list
#%%
# saving keep list
# joblib.dump(keep_list, 'D:/!bso/alertaDeterioro/features_dic2022.sav')
# Loading list example
keep_list = joblib.load('D:/!bso/alertaDeterioro/features_dic2022.sav')
#%%
# Undersampling
X_y_train_fe = pd.concat([X_train_fe.reset_index(drop=True),
y_train.reset_index(drop=True)], axis=1)
# Get the counts of defaults and non-defaults
count_nondefault, count_default = X_y_train_fe['det'].value_counts()

# Create data sets for defaults and non-defaults
nondefaults = X_y_train_fe[X_y_train_fe['det'] == 0]
defaults = X_y_train_fe[X_y_train_fe['det'] == 1]


# Undersample the non-defaults
nondefaults_under = nondefaults.sample(count_default)

# Concatenate the undersampled nondefaults with defaults
X_y_train_under = pd.concat([nondefaults_under.reset_index(drop=True),
                             defaults.reset_index(drop=True)], axis=0)

X_u = X_y_train_under.drop('det', axis=1)
y_u = X_y_train_under[['det']]
# Use test_train_split to create the training and test sets
X_train_u, X_test_u, y_train_u, y_test_u = train_test_split(
    X_u, y_u, test_size=.3)
# Print the value counts for loan status
print(X_y_train_under['det'].value_counts())
# %%
# Base random forest un undersampled data
start = time.time()
# clf_RF_del_s = RandomForestClassifier(n_jobs=-1, random_state=0).fit(X_train_fe, np.ravel(y_train))
clf_RF_del_s = RandomForestClassifier(
    n_jobs=-1, random_state=0).fit(X_u, np.ravel(y_u))
end = time.time()
elapsed = end - start
print('Time elapsed: ' + str(np.round(elapsed, decimals=2)) + ' seconds')

# Create predictions of probability for loan status using test data
preds = clf_RF_del_s.predict_proba(X_test_fe)
clf_logistic_preds = clf_RF_del_s.predict_proba(X_test_fe)

# Create a dataframe for the probabilities of default
preds_df_lr = pd.DataFrame(preds[:, 1], columns=[
                           'prob_det'], index=X_test_fe.index)
clf_preds = pd.DataFrame(preds[:, 1], columns=['prob_det'])

# Reassign loan status based on the threshold
preds_df_lr['pred_det'] = preds_df_lr['prob_det'].apply(
    lambda x: 1 if x > 0.65 else 0)
# Print the confusion matrix
print(confusion_matrix(y_test, preds_df_lr['pred_det']))

# Print the row counts for each loan status
print(preds_df_lr['pred_det'].value_counts())

# Print the classification report
target_names = ['Non-Default', 'Default']
print(classification_report(
    y_test, preds_df_lr['pred_det'], target_names=target_names))
print(precision_recall_fscore_support(y_test, preds_df_lr['pred_det'])[0:1])
# Store the number of loan defaults from the prediction data
num_defaults = preds_df_lr['pred_det'].value_counts()[1]
# Store the default recall from the classification report
default_recall = precision_recall_fscore_support(
    y_test, preds_df_lr['pred_det'])[1][1]
# Create predictions and store them in a variable
preds = clf_RF_del_s.predict_proba(X_test_fe)
precision, recall, thresholds = precision_recall_curve(
    y_test, preds_df_lr['prob_det'])
pr = pd.DataFrame(precision, columns=['pr'])
re = pd.DataFrame(recall, columns=['re'])
th = pd.DataFrame(thresholds, columns=['th'])
prDF = pd.concat([pr, re, th], axis=1)

preds_df_exp = pd.concat([preds_df_lr, y_test], axis=1)
preds_df_exp.to_csv('D:/!bso/alertaDeterioro/btest_Dic2022_65.csv')

# save the model to disk
filename = 'D:/!bso/alertaDeterioro/plain_RF_dic2022.sav'
pickle.dump(clf_RF_del_s, open(filename, 'wb'))
# %%
# calculate precision and recall
# precision, recall, thresholds = precision_recall_curve(
#     y_test, preds_df_lr['prob_det'])
# f1_scores = 2*recall*precision/(recall+precision)
# print('Best threshold: ', thresholds[np.argmax(f1_scores)])
# print('Best F1-Score: ', np.max(f1_scores))
# # create precision recall curve
# fig, ax = plt.subplots()
# ax.plot(recall, precision, color='purple')
# ax.plot()
# # add axis labels to plot
# ax.set_title('Precision-Recall Curve')
# ax.set_ylabel('Precision')
# ax.set_xlabel('Recall')

# # display plot
# plt.show()
#%%
# training base random forest time over full sample

start = time.time()
clf_RF_del_s = RandomForestClassifier(
    n_jobs=-1, random_state=0).fit(X_train_fe, np.ravel(y_train))
end = time.time()
elapsed = end - start
print('Time elapsed for default random forest: ' +
      str(np.round(elapsed, decimals=2)) + ' seconds')
# Create predictions of probability for loan status using test data
preds = clf_RF_del_s.predict_proba(X_test_fe)
clf_logistic_preds = clf_RF_del_s.predict_proba(X_test_fe)

# Create a dataframe for the probabilities of default
preds_df_lr = pd.DataFrame(preds[:, 1], columns=[
                           'prob_det'], index=X_test_fe.index)
clf_preds = pd.DataFrame(preds[:, 1], columns=['prob_det'])

# Reassign loan status based on the threshold
preds_df_lr['pred_det'] = preds_df_lr['prob_det'].apply(
    lambda x: 1 if x > 0.66 else 0)
# Print the confusion matrix
print(confusion_matrix(y_test, preds_df_lr['pred_det']))

# Print the row counts for each loan status
print(preds_df_lr['pred_det'].value_counts())

# Print the classification report
target_names = ['Non-Default', 'Default']
print(classification_report(
    y_test, preds_df_lr['pred_det'], target_names=target_names))
print(precision_recall_fscore_support(y_test, preds_df_lr['pred_det'])[0:1])
# Store the number of loan defaults from the prediction data
num_defaults = preds_df_lr['pred_det'].value_counts()[1]
# Store the default recall from the classification report
default_recall = precision_recall_fscore_support(
    y_test, preds_df_lr['pred_det'])[1][1]
# Create predictions and store them in a variable
preds = clf_RF_del_s.predict_proba(X_test_fe)

# %%
# Auxialiary functions
def perfThreshold(pred, y_test):
    finalDF = pd.DataFrame()
    pred = pd.DataFrame(pred[:, 1], columns=[
                        'prob_delinq'], index=y_test.index)
    pred['pred_delinq_50'] = pred['prob_delinq'].apply(
        lambda x: 1 if x > 0.50 else 0)
    auc = roc_auc_score(y_test, pred['prob_delinq'])*100
    precision_x, recall_x, thresholds_x = precision_recall_curve(
        y_test, pred['prob_delinq'])
    fscore_x = (2 * precision_x * recall_x) / (precision_x + recall_x)
    # locate the index of the largest f score
    ix_x = argmax(fscore_x)
    # print('Best Threshold=%.4f, F-Score=%.3f' % (thresholds_x[ix_x], fscore_x[ix_x]))
    # Reassign loan status based on the threshold
    pred['pred_delinq_opt'] = pred['prob_delinq'].apply(lambda x: 1 if x > thresholds_x[ix_x] else 0)
    # pred['pred_delinq_opt'] = pred['prob_delinq'].apply(
    #    lambda x: 1 if x > 0.70 else 0)

    # Print the confusion matrix (optimal)
    a00_opt = confusion_matrix(y_test, pred['pred_delinq_opt'])[0, 0]
    a01_opt = confusion_matrix(y_test, pred['pred_delinq_opt'])[0, 1]
    a10_opt = confusion_matrix(y_test, pred['pred_delinq_opt'])[1, 0]
    a11_opt = confusion_matrix(y_test, pred['pred_delinq_opt'])[1, 1]
    acc_opt = (a00_opt+a11_opt)/(a00_opt+a11_opt+a01_opt+a10_opt)*100
    pre_opt = a11_opt/(a11_opt+a01_opt)*100
    rec_opt = a11_opt/(a11_opt+a10_opt)*100
    # print('Optimal Accuracy Score : ' + str((a00+a11)/(a00+a11+a01+a10)))
    # print('Optimal Precision Score : ' + str(a11/(a11+a01)))
    # print('Optimal Recall Score : ' + str(a11/(a11+a10)))
    finalDF['accuracy_opt'] = [acc_opt]
    finalDF['precision_opt'] = [pre_opt]
    finalDF['recall_opt'] = [rec_opt]
    finalDF['auc'] = [auc]
    finalDF['tn_opt'] = [a00_opt]
    finalDF['tp_opt'] = [a11_opt]
    finalDF['fn_opt'] = [a10_opt]
    finalDF['fp_opt'] = [a01_opt]

    # Print the confusion matrix (50%)
    a00_50 = confusion_matrix(y_test, pred['pred_delinq_50'])[0, 0]
    a01_50 = confusion_matrix(y_test, pred['pred_delinq_50'])[0, 1]
    a10_50 = confusion_matrix(y_test, pred['pred_delinq_50'])[1, 0]
    a11_50 = confusion_matrix(y_test, pred['pred_delinq_50'])[1, 1]
    acc_50 = (a00_50+a11_50)/(a00_50+a11_50+a01_50+a10_50)*100
    pre_50 = a11_50/(a11_50+a01_50)*100
    rec_50 = a11_50/(a11_50+a10_50)*100
    # print('Optimal Accuracy Score : ' + str((a00+a11)/(a00+a11+a01+a10)))
    # print('Optimal Precision Score : ' + str(a11/(a11+a01)))
    # print('Optimal Recall Score : ' + str(a11/(a11+a10)))
    finalDF['accuracy_50'] = [acc_50]
    finalDF['precision_50'] = [pre_50]
    finalDF['recall_50'] = [rec_50]
    finalDF['tn_50'] = [a00_50]
    finalDF['tp_50'] = [a11_50]
    finalDF['fn_50'] = [a10_50]
    finalDF['fp_50'] = [a01_50]

    finalDF['bestThreshold'] = [thresholds_x[ix_x]*100]
    # edf = pd.concat([y_test, pred, id_test], axis=1)
    # edf = edf.loc[:,~edf.columns.duplicated()]
    # #display(edf)
    # edf['yint'] = edf['disb'] * edf['intrate']/100
    pd.options.display.float_format = '{:,.0f}'.format
    print('50% confusion matrix')
    print(confusion_matrix(y_test, pred['pred_delinq_50']))
    print('Optimal confusion matrix')
    print(confusion_matrix(y_test, pred['pred_delinq_opt']))
    print('Classification at optimal PD threshold \n')
    print('Best Threshold=%f, F-Score=%.3f' %
          (thresholds_x[ix_x], fscore_x[ix_x]))
    # print(pd.crosstab(edf.default, edf.pred_delinq_opt, margins = True))

    # #print('\n Cartera desembolsada según clasificación óptima \n')
    # valDisbOpt = pd.crosstab(edf.default, edf.pred_delinq_opt, values = edf.disb,
    #                 aggfunc = 'sum', margins = True).round(0)
    # #display(valDisbOpt)
    # moraObs = valDisbOpt.loc[1].loc['All']/valDisbOpt.loc['All'].loc['All']*100
    # moraOpt = valDisbOpt.loc[1].loc[0]/valDisbOpt.loc['All'].loc[0]*100
    # print('Morosidad observada : ' + str(moraObs))
    # print('Morosidad nueva : ' + str(moraOpt))

    # #print('\n Intereses según clasificación óptima \n')
    # valIntOpt = pd.crosstab(edf.default, edf.pred_delinq_opt, values = edf.presentValue,
    #                 aggfunc = 'sum', margins = True).round(0)
    # #display(valIntOpt)
    # finalDF['intLost'] = [valIntOpt.loc[0].loc[1]]

    # #print('\n Previsión según clasificación óptima \n')
    # valPrevOpt = pd.crosstab(edf.default, edf.pred_delinq_opt, values = edf.previsionmonto,
    #                 aggfunc = 'sum', margins = True).round(0)
    # #display(valPrevOpt)
    # finalDF['prevSaved'] = [valPrevOpt.loc[1].loc[1]]
    # finalDF['totalSavings'] = finalDF['prevSaved'] - finalDF['intLost']
    # finalDF['moraObs'] = [moraObs]
    # finalDF['moraOpt'] = [moraOpt]
    finalDF['balAccurracy_opt'] = (
        (a00_opt/(a00_opt+a10_opt)) + (a11_opt/(a11_opt+a01_opt)))/2*100
    finalDF['rejRate_opt'] = (a01_opt + a11_opt) / \
                              (a01_opt + a11_opt + a00_opt + a10_opt)*100
    finalDF['balAccurracy_50'] = (
        (a00_50/(a00_50+a10_50)) + (a11_50/(a11_50+a01_50)))/2*100
    finalDF['flagRate_50'] = (a01_50 + a11_50) / \
                              (a01_50 + a11_50 + a00_50 + a10_50)*100
    finalDF['flagRate_opt'] = (a01_opt + a11_opt) / \
                               (a01_opt + a11_opt + a00_opt + a10_opt)*100

    prec, rec, thresh = precision_recall_curve(y_test, pred['prob_delinq'])
    pr = pd.DataFrame(prec, columns=['pr'])
    re = pd.DataFrame(rec, columns=['re'])
    th = pd.DataFrame(thresh, columns=['th'])
    prDF = pd.concat([pr, re, th], axis=1)
    pd.options.display.float_format = '{:,.4f}'.format
    # display(prDF)
    return pred['prob_delinq'], finalDF, auc, prDF

def perfThreshold_su(pred, y_test, lim):
    finalDF = pd.DataFrame()
    pred = pd.DataFrame(pred[:, 1], columns=[
                        'prob_delinq'], index=y_test.index)
    pred['pred_delinq_lim'] = pred['prob_delinq'].apply(
        lambda x: 1 if x > lim else 0)
    auc = roc_auc_score(y_test, pred['prob_delinq'])*100
    precision_x, recall_x, thresholds_x = precision_recall_curve(
        y_test, pred['prob_delinq'])
    fscore_x = (2 * precision_x * recall_x) / (precision_x + recall_x)
    # locate the index of the largest f score
    ix_x = argmax(fscore_x)
    
    # Print the confusion matrix (50%)
    a00_50 = confusion_matrix(y_test, pred['pred_delinq_lim'])[0, 0]
    a01_50 = confusion_matrix(y_test, pred['pred_delinq_lim'])[0, 1]
    a10_50 = confusion_matrix(y_test, pred['pred_delinq_lim'])[1, 0]
    a11_50 = confusion_matrix(y_test, pred['pred_delinq_lim'])[1, 1]
    acc_50 = (a00_50+a11_50)/(a00_50+a11_50+a01_50+a10_50)*100
    pre_50 = a11_50/(a11_50+a01_50)*100
    rec_50 = a11_50/(a11_50+a10_50)*100
    finalDF['accuracy'] = [acc_50]
    finalDF['precision'] = [pre_50]
    finalDF['recall'] = [rec_50]
    finalDF['tn'] = [a00_50]
    finalDF['tp'] = [a11_50]
    finalDF['fn'] = [a10_50]
    finalDF['fp'] = [a01_50]

    finalDF['bestThreshold'] = [thresholds_x[ix_x]*100]
    
    pd.options.display.float_format = '{:,.0f}'.format
    print('Confusion matrix')
    print(confusion_matrix(y_test, pred['pred_delinq_lim']))
    
    print('Best Threshold=%f, F-Score=%.3f' %
          (thresholds_x[ix_x], fscore_x[ix_x]))
   
    prec, rec, thresh = precision_recall_curve(y_test, pred['prob_delinq'])
    pr = pd.DataFrame(prec, columns=['pr'])
    re = pd.DataFrame(rec, columns=['re'])
    th = pd.DataFrame(thresh, columns=['th'])
    prDF = pd.concat([pr, re, th], axis=1)
    pd.options.display.float_format = '{:,.4f}'.format
    # display(prDF)
    return pred['prob_delinq'], finalDF, auc, prDF
# Grid search Logistic regressions
# function to train logistic regressions and evaluate the performance

def run_LogisticGrid(X_train, X_test, y_train, y_test):
    # Grid search Logistic regressions
    clf = LogisticRegression(max_iter=30000, verbose=True)
    kfold = StratifiedKFold(n_splits=3, shuffle=True)
    grid_values = {'penalty': ['l1', 'l2'], 'C': [
        0.001, .009, 0.01, .09, 1, 5, 10, 25]}
    grid_clf = GridSearchCV(clf, param_grid=grid_values, scoring='recall',
    cv=kfold.split(X_train, y_train), n_jobs=-1)
    grid_clf.fit(X_train, np.ravel(y_train))
    best_logistic_grid = grid_clf.best_estimator_
    best_logistic_grid.fit(X_train, np.ravel(y_train))

    print('Test set')
    pred = best_logistic_grid.predict_proba(X_test)
    print(
        'Logistic regression roc-auc: {}'.format(roc_auc_score(y_test, pred[:, 1])))
    df_pred, finalDF_LogGrid, auc, prDF = perfThreshold(pred, y_test)
    dftr = best_logistic_grid.predict_proba(X_train)
    dftr = pd.DataFrame(dftr[:,1], columns = ['prob_det'], index = X_train.index)

    return auc, finalDF_LogGrid, df_pred, best_logistic_grid, prDF, dftr
    
# just re-sampling methods (no classifier)
resampling_dict = {

    'random': RandomUnderSampler(
        sampling_strategy='auto',
        replacement=False,
    ),

    # 'smote': SMOTE(
    #     sampling_strategy='auto',
    #     k_neighbors=5,
    #     n_jobs=-1,
    # ),
}

ensemble_dict = {

    # balanced random forests (bagging)
    'balancedRF': BalancedRandomForestClassifier(
        n_estimators=500,
        criterion='gini',
        max_depth=5,
        sampling_strategy='auto',
        n_jobs=-1,
    ),
    # bagging of Logistic regression, no resampling
    'baggingLog': BaggingClassifier(
        estimator=LogisticRegression(C=25),
        n_estimators=500,
        n_jobs=-1,
    ),
    # bagging of decision tree, no resampling
    'baggingTree': BaggingClassifier(
        estimator=DecisionTreeClassifier(),
        n_estimators=500,
        n_jobs=-1,
    ),
    # bagging of Logistic regression, with resampling
    'balancedbaggingLog': BalancedBaggingClassifier(
        estimator=LogisticRegression(C=25),
        n_estimators=500,
        max_samples=1.0,  # The number of samples to draw from X to train each base estimator
        max_features=10,  # The number of features to draw from X to train each base estimator
        bootstrap=True,
        bootstrap_features=False,
        sampling_strategy='auto',
        n_jobs=-1,
    ),
    # bagging of Logistic regression, with resampling
    'balancedbaggingTree': BalancedBaggingClassifier(
        estimator=DecisionTreeClassifier(),
        n_estimators=500,
        max_samples=1.0,  # The number of samples to draw from X to train each base estimator
        max_features=10,  # The number of features to draw from X to train each base estimator
        bootstrap=True,
        bootstrap_features=False,
        sampling_strategy='auto',
        n_jobs=-1,
    ),
    # boosting + undersampling
    'rusboost': RUSBoostClassifier(
        estimator=None,
        n_estimators=500,
        learning_rate=0.1,
        sampling_strategy='auto',
    ),
    # bagging + boosting + under-sammpling
    'easyEnsemble': EasyEnsembleClassifier(
        n_estimators=500,
        sampling_strategy='auto',
        n_jobs=-1,
    ),
}

def run_ensemble(ensemble, X_train, X_test, y_train, y_test):

    ensemble.fit(X_train, np.ravel(y_train))
    print('Test set')
    pred = ensemble.predict_proba(X_test)
    print(
        'ensembleBoost roc-auc: {}'.format(roc_auc_score(y_test, pred[:, 1])))
    df_pred, finalDF_Ens, auc, prdf = perfThreshold(pred, y_test)
    pd.options.display.float_format = '{:,.4f}'.format
    dftr = ensemble.predict_proba(X_train)
    dftr = pd.DataFrame(dftr[:,1], columns = ['prob_det'], index = X_train.index)

    return df_pred, finalDF_Ens, auc, prdf, ensemble, dftr

# function to train random forests and evaluate the performance with grid search

def run_gridRandomForests(X_train, X_test, y_train, y_test):
    n_estimators = [int(x) for x in np.linspace(start=400, stop=1600, num=4)]
    max_features = ['auto', 'sqrt']
    max_depth = [int(x) for x in np.linspace(10, 100, num=5)]
    max_depth.append(None)
    min_samples_split = [2, 5, 10]
    min_samples_leaf = [1, 2, 4]
    bootstrap = [True, False]
    # Create the random grid
    random_grid = {'n_estimators': n_estimators,
                'max_features': max_features,
                'max_depth': max_depth,
                'min_samples_split': min_samples_split,
                'min_samples_leaf': min_samples_leaf,
                'bootstrap': bootstrap}
    print(random_grid)
    cv = StratifiedKFold(n_splits=4, shuffle=True)
    clf_RF_del_s = RandomForestClassifier()
    # Random search of parameters, using 3 fold cross validation,
    clf_RF_del_s_rand = RandomizedSearchCV(estimator=clf_RF_del_s, param_distributions=random_grid,
    n_iter=100, cv=cv.split(X_train, y_train), verbose=2,  n_jobs=-1)

    clf_RF_del_s_rand.fit(X_train, np.ravel(y_train))
    best_rf = clf_RF_del_s_rand.best_estimator_
    print('Test set')
    pred = clf_RF_del_s_rand.best_estimator_.predict_proba(X_test)
    print(
        'Random Forests roc-auc: {}'.format(roc_auc_score(y_test, pred[:, 1])))
    df_pred, finalDF_gridRF, auc, prdf = perfThreshold(pred, y_test)
    dftr = best_rf.predict_proba(X_train)
    dftr = pd.DataFrame(dftr[:,1], columns = ['prob_det'], index = X_train.index)

    return auc, finalDF_gridRF, df_pred, best_rf, prdf, dftr

def run_xgBoost(X_train, X_test, y_train, y_test):
    # A parameter grid for XGBoost
    params = {
            "learning_rate": [0.05, 0.10, 0.15, 0.20],
            'min_child_weight': [1, 5, 10],
            'gamma': [0.5, 1, 1.5, 2, 5],
            'subsample': [0.6, 0.8, 1.0],
            'colsample_bytree': [0.6, 0.8, 1.0],
            'max_depth': [4, 5, 7]
            }
    # Create and train the model on the training data
    clf_gbt = xgb.XGBClassifier(silent=False,
                    scale_pos_weight=1,
                    objective='binary:logistic',
                    n_estimators=1000,
                    reg_alpha=0.3
                    )
    folds = 3
    param_comb = 6

    skf = StratifiedKFold(n_splits=folds, shuffle=True)
    start = time.time()
    random_search = RandomizedSearchCV(clf_gbt, param_distributions=params,
                                    n_iter=param_comb, scoring='roc_auc',
                                    n_jobs=-1, cv=skf.split(X_train, y_train), verbose=3)
    random_search.fit(X_train, y_train)
    end = time.time()
    print('Elapsed time ' + str(end-start))
    best_xgb = random_search.best_estimator_
    pred = best_xgb.predict_proba(X_test)
    print('XGBoost roc-auc: {}'.format(roc_auc_score(y_test, pred[:, 1])))
    df_pred, finalDF_RF, auc, prdf = perfThreshold(pred, y_test)
    dftr = best_xgb.predict_proba(X_train)
    dftr = pd.DataFrame(dftr[:,1], columns = ['prob_det'], index = X_train.index)

    return auc, finalDF_RF, df_pred, best_xgb, prdf, dftr


# %%
# result storage
results_dict = {}
dfs_dict = {}
dfTR_dict = {}
dfTE_dict = {}
prTE_dict = {}
algo_dict = {}
#%%
# Model training
start = time.time()
roc, resdf, tedf, algo, prdf, dftr = run_xgBoost(X_u, X_test_fe, y_u, y_test)
nKey = 'u_xgb'
outModel = 'D:/!bso/scoreCobranza/' + nKey + '.pkl'
pickle_out = open(outModel, mode='wb')
pickle.dump(algo, pickle_out)
pickle_out.close()
results_dict[nKey] = roc
dfs_dict[nKey] = resdf
dfTE_dict[nKey] = tedf
dfTR_dict[nKey] = dftr
algo_dict[nKey] = algo
prTE_dict[nKey] = prdf
print()
end = time.time()
print('Elapsed time for ',  nKey, ':', str(round(end-start, 2)))

# start = time.time()
# roc, resdf, tedf, algo, prdf, dftr = run_xgBoost(X_train_fe, X_test_fe, y_train, y_test)
# nKey = 'full_xgb'
# outModel = 'D:/!bso/alertaDeterioro/' + nKey + '.pkl'
# pickle_out = open(outModel, mode='wb')
# pickle.dump(algo, pickle_out)
# pickle_out.close()
# results_dict[nKey] = roc
# dfs_dict[nKey] = resdf
# dfTE_dict[nKey] = tedf
# algo_dict[nKey] = algo
# prTE_dict[nKey] = prdf
# dfTR_dict[nKey] = dftr
# print()
# end = time.time()
# print('Elapsed time for ',  nKey, ':', str(round(end-start, 2)))

# start = time.time()
# roc, resdf, tedf, algo, prdf = run_gridRandomForests(X_train_fe, X_test_fe, y_train, y_test)
# nKey = 'full_RandomForest'
# outModel = 'D:/!bso/alertaDeterioro/' + nKey + '.pkl'
# pickle_out = open(outModel, mode='wb')
# pickle.dump(algo, pickle_out)
# pickle_out.close()
# results_dict[nKey] = roc
# dfs_dict[nKey] = resdf
# dfTE_dict[nKey] = tedf
# algo_dict[nKey] = algo
# prTE_dict[nKey] = prdf
# print()
# end = time.time()
# print('Elapsed time for ',  nKey, ':', str(round(end-start, 2)))

start = time.time()
roc, resdf, tedf, algo, prdf, dftr = run_LogisticGrid(
    X_train_fe, X_test_fe, y_train, y_test)
nKey = 'full_gridLog'
outModel = 'D:/!bso/alertaDeterioro/' + nKey + '.pkl'
pickle_out = open(outModel, mode='wb')
pickle.dump(algo, pickle_out)
pickle_out.close()
results_dict[nKey] = roc
dfs_dict[nKey] = resdf
dfTE_dict[nKey] = tedf
algo_dict[nKey] = algo
prTE_dict[nKey] = prdf
dfTR_dict[nKey] = dftr
print()
end = time.time()
print('Elapsed time for ',  nKey, ':', str(round(end-start, 2)))

start = time.time()
roc, resdf, tedf, algo, prdf, dftr = run_LogisticGrid(
    X_u, X_test_fe, y_u, y_test)
nKey = 'u_gridLog'
outModel = 'D:/!bso/alertaDeterioro/' + nKey + '.pkl'
pickle_out = open(outModel, mode='wb')
pickle.dump(algo, pickle_out)
pickle_out.close()
results_dict[nKey] = roc
dfs_dict[nKey] = resdf
dfTE_dict[nKey] = tedf
algo_dict[nKey] = algo
prTE_dict[nKey] = prdf
dfTR_dict[nKey] = dftr
print()
end = time.time()
print('Elapsed time for ',  nKey, ':', str(round(end-start, 2)))

ensstart = time.time()
for ensemble in ensemble_dict.keys():
    start = time.time()
    # try:
    print(ensemble)
    key = 'full_' + ensemble
        # train model and store result
    tedf, resdf, auc, prdf, algo, dftr = run_ensemble(ensemble_dict[ensemble], X_train_fe,
                                                X_test_fe, y_train, y_test)

    outModel = 'D:/!bso/alertaDeterioro/' + key + '.pkl'
    pickle_out = open(outModel, mode='wb')
    pickle.dump(algo, pickle_out)
    pickle_out.close()
    # results_dict[key] = roc
    dfs_dict[key] = resdf
    prTE_dict[key] = prdf
    dfTE_dict[key] = tedf
    algo_dict[key] = algo
    dfTR_dict[key] = dftr
    print()
    # except:
       # pass
    end = time.time()
    print('Elapsed time for ',  key, ':', str(round(end-start, 1)))
#%%
# I need to check this
# final stack
del dfTR_dict['u_xgb']
del dfTR_dict['u_gridLog']
del dfTE_dict['u_xgb']
del dfTE_dict['u_gridLog']

trDFs = pd.concat(dfTR_dict, axis=1)
trTEs = pd.concat(dfTE_dict, axis=1)

start = time.time()
roc, resdf, tedf, algo, prdf, dftr = run_LogisticGrid(trDFs, trTEs, y_train, y_test)
nKey = 'stack_LogisticGrid'
outModel = 'D:/!bso/alertaDeterioro/' + nKey + '.pkl'
pickle_out = open(outModel, mode='wb')
pickle.dump(algo, pickle_out)
pickle_out.close()
results_dict[nKey] = roc
dfs_dict[nKey] = resdf
dfTE_dict[nKey] = tedf
algo_dict[nKey] = algo
prTE_dict[nKey] = prdf
dfTR_dict[nKey] = dftr
print()
end = time.time()
print('Elapsed time for ',  nKey, ':', str(round(end-start, 2)))
#%%
# Appending results
resDFs = pd.concat(dfs_dict, axis=0)
# resDFs['Sc_Savings'] = resDFs['totalSavings'].rank(ascending=True)
resDFs['Sc_Recall'] = resDFs['recall_50'].rank(ascending=True)
resDFs['Sc_BalAcc'] = resDFs['balAccurracy_50'].rank(ascending=True)
resDFs['Sc_Avg'] = resDFs[['Sc_Recall', 'Sc_BalAcc']].mean(axis=1)
pd.options.display.float_format = '{:,.2f}'.format

display(resDFs.sort_values(by='Sc_Avg', ascending=False))
# Results storing
resDFs.to_csv('D:/!bso/resDF_collect_dic2022.csv')
# %%
pdDFs = pd.concat(dfTE_dict, axis=1)
pdDFs.to_csv('D:/!bso/pdDFs_collect_dic2022.csv')
#probDFs = pdDFs.iloc[:,pdDFs.columns.str.startswith("prob")]
#display(probDFs)
#%%
fdt_dict = {}
for smodel in list(algo_dict.keys()) :
    try:
        #smodel = 'full_rusboost_0'
        fpreds = pdDFs[smodel]
        fpreds = pd.concat([fpreds, y_test], axis = 1)
        fpreds[smodel] = fpreds[smodel]*100

        #netSaveList = []
        tpList = []
        fpList = []
        tnList = []
        fnList = []
        accList = []
        recList = []
        preList = []
        balList = []
        rejList = []
        for i in range(100):
            name = 'clf_' + str(i)
            fpreds[name] = fpreds[smodel].apply(lambda x: 1 if x > i else 0)
            #print(i)
            #print(confusion_matrix(fpreds['default'],fpreds[name]))
            tn = confusion_matrix(fpreds['det'],fpreds[name])[0,0]
            fp = confusion_matrix(fpreds['det'],fpreds[name])[0,1]
            fn = confusion_matrix(fpreds['det'],fpreds[name])[1,0]
            tp = confusion_matrix(fpreds['det'],fpreds[name])[1,1]
            acc = (tn+tp)/(tn+fn+tp+fp)*100
            pre = tp/(tp+fp)*100
            rec = tp/(tp+fn)*100
            bal = ((tn/(tn+fn)) + (tp/(tp+fp)))/2*100
            rej = (tp+fp)/(tn+fn+tp+fp)*100
            tpList.append(tp)
            tnList.append(tn)
            fpList.append(fp)
            fnList.append(fn)
            accList.append(acc) 
            recList.append(rec) 
            preList.append(pre) 
            balList.append(bal) 
            rejList.append(rej)
        
        #dfNetSavings = pd.DataFrame(netSaveList, columns = ['netSavings'])
        dftp = pd.DataFrame(tpList, columns = ['tp'])
        dffp = pd.DataFrame(fpList, columns = ['fp'])
        dffn = pd.DataFrame(fnList, columns = ['fn'])
        dftn = pd.DataFrame(tnList, columns = ['tn'])
        dfacc = pd.DataFrame(accList, columns = ['acc'])
        dfrec = pd.DataFrame(recList, columns = ['rec'])
        dfpre = pd.DataFrame(preList, columns = ['pre'])
        dfbal = pd.DataFrame(balList, columns = ['bal'])
        dfrej = pd.DataFrame(rejList, columns = ['rej'])
        dfThresh = pd.concat([dftp, dffp, dffn, dftn, dfacc, dfpre, dfrec, dfbal, dfrej], axis=1)
        dfThresh['model'] = smodel
        fdt_dict[smodel] = dfThresh
    except:
        pass

#%%
fdtDFs = pd.concat(fdt_dict, axis=0)
fdtDFs = fdtDFs.unstack(level=0)
fdtFDs = fdtDFs.droplevel(0, axis = 1)
fdtDFs.to_csv('D:/!bso/alertaDeterioro/fdtDFs_dic2022.csv')

#%%
# individual model testing
model = pickle.load(open('D:/!bso/alertaDeterioro/stack_xgb.pkl', 'rb'))
pred = model.predict_proba(trTEs)
df_pred, finalDF, auc, prdf = perfThreshold_su(pred, y_test, 0.9)
#%%
# final model predictions
model = pickle.load(open('D:/!bso/alertaDeterioro/full_balancedbaggingLog.pkl', 'rb'))
pred = model.predict_proba(X_test_fe)
df_pred, finalDF, auc, prdf = perfThreshold_su(pred, y_test, 0.65)
finalDF
# df_pred.to_csv('D:/!bso/alertaDeterioro/pred_test_set_dic2022.csv')
#%%
# Forecasting set
dff = pd.read_csv(
    'D:/!bso/alertaDeterioro/fcast_score_cobranza_mar2023_Aonly.csv')
dff = dff.set_index('OPERACION', drop=True)

dff['AGENCIA'] = dff['AGENCIA'].astype(str)
dff['MONEDA'] = dff['MONEDA'].astype(str)
dff['OBJETO_CRED'] = dff['OBJETO_CRED'].astype(str)
dff.dtypes

# Create two data sets for numeric and non-numeric data
dff_num = dff.select_dtypes(exclude=['object'])
dff_str = dff.select_dtypes(include=['object'])
# One-hot encode the non-numeric columns
dff_str_onehot = pd.get_dummies(dff_str)
# Union the one-hot encoded columns to the numeric ones
cr_loan_prepF = pd.concat([dff_num, dff_str_onehot], axis=1)
# Print the columns in the new data set
X_fcast_fe = cr_loan_prepF[keep_list]

#%%
# Create predictions of probability for loan status using test data
model = pickle.load(open('D:/!bso/alertaDeterioro/full_balancedbaggingLog.pkl', 'rb'))
fcast_preds = model.predict_proba(X_fcast_fe)
fcast_df_lr = pd.DataFrame(fcast_preds[:, 1], columns=[
                           'prob_det'], index=X_fcast_fe.index)
fcast_df_lr['pred_det'] = fcast_df_lr['prob_det'].apply(
    lambda x: 1 if x > 0.52 else 0)

fcast_alerts = fcast_df_lr[fcast_df_lr['pred_det'] == 1]
fcast_alerts.to_csv(
    'D:/!bso/alertaDeterioro/detForecast_Marzo2023_Aonly.csv')
fcast_df_lr
fcast_df_lr.pred_det.value_counts()
# %%
fcast_df_lr.prob_det.describe()
# %%
# Life cycle analysis: Training adding a new semester of data
df_new = pd.read_csv(
    'D:/!bso/alertaDeterioro/data_score_cobranza_dic2022_Aonly_synthetic_1_newTrain.csv')
# df2 = df.replace([np.inf, -np.inf], np.nan, inplace=True)
# df2 = df.dropna(axis=0).reset_index(drop=True)
# df = df.set_index('OPERACION', drop=True)

df_new['AGENCIA'] = df_new['AGENCIA'].astype(str)
# df['CIU'] = df['CIU'].astype(str)
# df['CAEDEC_DEST'] = df['CAEDEC_DEST'].astype(str)
df_new['MONEDA'] = df_new['MONEDA'].astype(str)
df_new['OBJETO_CRED'] = df_new['OBJETO_CRED'].astype(str)
# df['PATRIMONIO'] = np.where(df['MONEDA'] == '0', df['PATRIMONIO']/6.86, df['PATRIMONIO'])
# df['ING_VENTAS'] = np.where(df['MONEDA'] == '0', df['ING_VENTAS']/6.86, df['ING_VENTAS'])
df_new.dtypes

# Create two data sets for numeric and non-numeric data
df_new_num = df_new.select_dtypes(exclude=['object'])
df_new_str = df_new.select_dtypes(include=['object'])

# One-hot encode the non-numeric columns
df_new_str_onehot = pd.get_dummies(df_new_str)

# Union the one-hot encoded columns to the numeric ones
cr_loan_prep_new = pd.concat([df_new_num, df_new_str_onehot], axis=1)

# Print the columns in the new data set
print(cr_loan_prep_new.columns)
cr_loan_prep_new = cr_loan_prep_new.set_index('OPERACION', drop=True)
del df_new
del df_new_num
del df_new_str
#%%
# Create the X and y data sets
X_new = cr_loan_prep_new.drop(['det'], axis=1)
y_new = cr_loan_prep_new[['det', 'tr2']]
X_new_train = X_new.loc[X_new['tr2'] == 1]
X_new_train = X_new_train.drop(['tr2'], axis=1)
X_new_test_full = X_new.loc[X_new['tr2'] > 1]

#%%
X_new_test = X_new.loc[X_new['tr2'] == 6]
X_new_test = X_new_test.drop(['tr2'], axis=1)

y_new_train = y_new.loc[y_new['tr2'] == 1]
y_new_train = y_new_train.drop(['tr2'], axis=1)
y_new_test = y_new.loc[y_new['tr2'] == 6]
y_new_test = y_new_test.drop(['tr2'], axis=1)
y_new_test_full = y_new.loc[y_new['tr2'] > 1]

X_new_testDic = dict(tuple(X_new_test_full.groupby('tr2')))
y_new_testDic = dict(tuple(y_new_test_full.groupby('tr2')))

# %%
# Scaling for new training data
col_names_new = list(X_new_train.columns)
mm_scaler_new = preprocessing.MinMaxScaler()
X_new_train_mm = mm_scaler_new.fit_transform(X_new_train)
X_new_train_mm = pd.DataFrame(X_new_train_mm, columns=col_names_new)
X_new_test_mm = mm_scaler_new.transform(X_new_test)
X_new_test_mm = pd.DataFrame(X_new_test_mm, columns=col_names_new)

# %%
# Feature selection for new training data
columns_new = list(X_new_train_mm)
auc_list_t_new = []
colname_t_new = []
for i in columns_new:
    # Picking one feature at a time
    X_new_train_v = pd.DataFrame(X_new_train_mm[i])
    X_new_test_v = pd.DataFrame(X_new_test_mm[i])

    dt = DecisionTreeClassifier(max_depth=4)

    # Fit dt to the training set
    dt.fit(X_new_train_v, y_new_train)

    # Predict test set labels
    _tree = dt.predict_proba(X_new_test_v)
    # Create a dataframe for the probabilities of default
    _tree2 = pd.DataFrame(_tree[:, 1], columns=['prob_delinq'])
    # Reassign loan status based on the threshold
    _tree2['pred_delinq'] = _tree2['prob_delinq'].apply(
        lambda x: 1 if x > 0.5 else 0)

    # start_time = time.timer(None)
    # time.timer(start_time) # timing ends here for "start_time" variable

    # Print the confusion matrix
    print(confusion_matrix(y_new_test, _tree2['pred_delinq']))
    cm = confusion_matrix(y_new_test, _tree2['pred_delinq'])
    recall = cm[1, 1]/(cm[1, 1] + cm[1, 0])
    # print(cm)
    print('Recall is ' + str(recall))
    # Compute the AUC and store it in a variable
    auc_lr_del = roc_auc_score(y_test, _tree2['prob_delinq'])
    auc_list_t_new.append(auc_lr_del)
    colname_t_new.append(i)
    print(i)
    # Print the auc for the model
    print(X_new_train_v.columns)
    print('The AUC is %s' % np.round(auc_lr_del, decimals=4))

df_auc_new = pd.DataFrame(auc_list_t_new, columns=['auc'])
df_cols_new = pd.DataFrame(colname_t_new, columns=['var'])
df_auc_new = pd.concat([df_auc_new, df_cols_new], axis=1)
df_auc_new['keep'] = df_auc_new['auc'].apply(lambda x: 1 if x >= 0.51 else 0)
print(df_auc['keep'].sum())
df_keep_new = df_auc_new[df_auc_new['keep'] == 1]
df_drop_new = df_auc_new[df_auc_new['keep'] == 0]
keep_list_new = df_keep_new['var'].tolist()
drop_list_new = df_drop_new['var'].tolist()

X_new_train_fe = X_new_train_mm[keep_list_new]
# X_u_fe = X_u[keep_list]
# X_smote_fe = X_smote[keep_list]
X_new_train_oo = X_new_train_mm[drop_list_new]

X_new_test_fe = X_new_test_mm[keep_list_new]
X_new_test_full_fe = X_new_test_full[keep_list_new]
keep_list_new
#%%
joblib.dump(keep_list_new, 'D:/!bso/alertaDeterioro/features_dic2022_new.sav')
# Loading list example
# kl = joblib.load('D:/!bso/alertaDeterioro/features_dic2022.sav')
#%%
# Undersampling
X_y_new_train_fe = pd.concat([X_new_train_fe.reset_index(drop=True),
y_new_train.reset_index(drop=True)], axis=1)
# Get the counts of defaults and non-defaults
count_nondefault_new, count_default_new = X_y_new_train_fe['det'].value_counts()

# Create data sets for defaults and non-defaults
nondefaults_new = X_y_new_train_fe[X_y_new_train_fe['det'] == 0]
defaults_new = X_y_new_train_fe[X_y_new_train_fe['det'] == 1]

# Undersample the non-defaults
nondefaults_under_new = nondefaults_new.sample(count_default_new)

# Concatenate the undersampled nondefaults with defaults
X_y_new_train_under = pd.concat([nondefaults_under_new.reset_index(drop=True),
                             defaults_new.reset_index(drop=True)], axis=0)

X_new_u = X_y_new_train_under.drop('det', axis=1)
y_new_u = X_y_new_train_under[['det']]
# Use test_train_split to create the training and test sets
X_new_train_u, X_new_test_u, y_new_train_u, y_new_test_u = train_test_split(
    X_new_u, y_new_u, test_size=.3)
# Print the value counts for loan status
print(X_y_new_train_under['det'].value_counts())
# %%
# Base random forest un undersampled data
start = time.time()
# clf_RF_del_s = RandomForestClassifier(n_jobs=-1, random_state=0).fit(X_train_fe, np.ravel(y_train))
clf_RF_del_s_new = RandomForestClassifier(
    n_jobs=-1, random_state=0).fit(X_new_u, np.ravel(y_new_u))
end = time.time()
elapsed = end - start
print('Time elapsed: ' + str(np.round(elapsed, decimals=2)) + ' seconds')

# Create predictions of probability for loan status using test data
preds_new = clf_RF_del_s_new.predict_proba(X_new_test_fe)
clf_logistic_preds_new = clf_RF_del_s_new.predict_proba(X_new_test_fe)

# Create a dataframe for the probabilities of default
preds_df_lr_new = pd.DataFrame(preds_new[:, 1], columns=[
                           'prob_det'], index=X_test_fe.index)
clf_preds_new = pd.DataFrame(preds_new[:, 1], columns=['prob_det'])

# Reassign loan status based on the threshold
preds_df_lr_new['pred_det'] = preds_df_lr_new['prob_det'].apply(
    lambda x: 1 if x > 0.65 else 0)
# Print the confusion matrix
print(confusion_matrix(y_new_test, preds_df_lr_new['pred_det']))

# Print the row counts for each loan status
print(preds_df_lr_new['pred_det'].value_counts())

# Print the classification report
target_names = ['Non-Default', 'Default']
print(classification_report(
    y_new_test, preds_df_lr_new['pred_det'], target_names=target_names))
print(precision_recall_fscore_support(y_new_test, preds_df_lr_new['pred_det'])[0:1])
# Store the number of loan defaults from the prediction data
num_defaults = preds_df_lr_new['pred_det'].value_counts()[1]
# Store the default recall from the classification report
default_recall = precision_recall_fscore_support(
    y_new_test, preds_df_lr_new['pred_det'])[1][1]
# Create predictions and store them in a variable
preds_new = clf_RF_del_s_new.predict_proba(X_new_test_fe)
precision, recall, thresholds = precision_recall_curve(
    y_new_test, preds_df_lr_new['prob_det'])
pr = pd.DataFrame(precision, columns=['pr'])
re = pd.DataFrame(recall, columns=['re'])
th = pd.DataFrame(thresholds, columns=['th'])
prDF_new = pd.concat([pr, re, th], axis=1)

preds_df_exp_new = pd.concat([preds_df_lr, y_test], axis=1)
preds_df_exp_new.to_csv('D:/!bso/alertaDeterioro/btest_Dic2022_65_new.csv')

# save the model to disk
filename = 'D:/!bso/alertaDeterioro/plain_RF_new.sav'
pickle.dump(clf_RF_del_s_new, open(filename, 'wb'))
# %%
# calculate precision and recall
# precision, recall, thresholds = precision_recall_curve(
#     y_test, preds_df_lr['prob_det'])
# f1_scores = 2*recall*precision/(recall+precision)
# print('Best threshold: ', thresholds[np.argmax(f1_scores)])
# print('Best F1-Score: ', np.max(f1_scores))
# # create precision recall curve
# fig, ax = plt.subplots()
# ax.plot(recall, precision, color='purple')
# ax.plot()
# # add axis labels to plot
# ax.set_title('Precision-Recall Curve')
# ax.set_ylabel('Precision')
# ax.set_xlabel('Recall')

# # display plot
# plt.show()
#%%
# training base random forest time over full sample

start = time.time()
clf_RF_del_s = RandomForestClassifier(
    n_jobs=-1, random_state=0).fit(X_train_fe, np.ravel(y_train))
end = time.time()
elapsed = end - start
print('Time elapsed for default random forest: ' +
      str(np.round(elapsed, decimals=2)) + ' seconds')
# Create predictions of probability for loan status using test data
preds = clf_RF_del_s.predict_proba(X_test_fe)
clf_logistic_preds = clf_RF_del_s.predict_proba(X_test_fe)

# Create a dataframe for the probabilities of default
preds_df_lr = pd.DataFrame(preds[:, 1], columns=[
                           'prob_det'], index=X_test_fe.index)
clf_preds = pd.DataFrame(preds[:, 1], columns=['prob_det'])

# Reassign loan status based on the threshold
preds_df_lr['pred_det'] = preds_df_lr['prob_det'].apply(
    lambda x: 1 if x > 0.66 else 0)
# Print the confusion matrix
print(confusion_matrix(y_test, preds_df_lr['pred_det']))

# Print the row counts for each loan status
print(preds_df_lr['pred_det'].value_counts())

# Print the classification report
target_names = ['Non-Default', 'Default']
print(classification_report(
    y_test, preds_df_lr['pred_det'], target_names=target_names))
print(precision_recall_fscore_support(y_test, preds_df_lr['pred_det'])[0:1])
# Store the number of loan defaults from the prediction data
num_defaults = preds_df_lr['pred_det'].value_counts()[1]
# Store the default recall from the classification report
default_recall = precision_recall_fscore_support(
    y_test, preds_df_lr['pred_det'])[1][1]
# Create predictions and store them in a variable
preds = clf_RF_del_s.predict_proba(X_test_fe)
# %%
# result storage
results_dict_new = {}
dfs_dict_new = {}
dfTR_dict_new = {}
dfTE_dict_new = {}
prTE_dict_new = {}
algo_dict_new = {}

# Model training
start = time.time()
roc, resdf, tedf, algo, prdf, dftr = run_xgBoost(X_new_u, X_new_test_fe, y_new_u, y_new_test)
nKey = 'u_xgb_new'
outModel = 'D:/!bso/alertaDeterioro/' + nKey + '.pkl'
pickle_out = open(outModel, mode='wb')
pickle.dump(algo, pickle_out)
pickle_out.close()
results_dict_new[nKey] = roc
dfs_dict_new[nKey] = resdf
dfTE_dict_new[nKey] = tedf
dfTR_dict_new[nKey] = dftr
algo_dict_new[nKey] = algo
prTE_dict_new[nKey] = prdf
print()
end = time.time()
print('Elapsed time for ',  nKey, ':', str(round(end-start, 2)))

# start = time.time()
# roc, resdf, tedf, algo, prdf, dftr = run_xgBoost(X_new_train_fe, X_new_test_fe, y_new_train, y_new_test)
# nKey = 'full_xgb_new'
# outModel = 'D:/!bso/alertaDeterioro/' + nKey + '.pkl'
# pickle_out = open(outModel, mode='wb')
# pickle.dump(algo, pickle_out)
# pickle_out.close()
# results_dict[nKey] = roc
# dfs_dict[nKey] = resdf
# dfTE_dict[nKey] = tedf
# algo_dict[nKey] = algo
# prTE_dict[nKey] = prdf
# dfTR_dict[nKey] = dftr
# print()
# end = time.time()
# print('Elapsed time for ',  nKey, ':', str(round(end-start, 2)))

# start = time.time()
# roc, resdf, tedf, algo, prdf = run_gridRandomForests(X_train_fe, X_test_fe, y_train, y_test)
# nKey = 'full_RandomForest'
# outModel = 'D:/!bso/alertaDeterioro/' + nKey + '.pkl'
# pickle_out = open(outModel, mode='wb')
# pickle.dump(algo, pickle_out)
# pickle_out.close()
# results_dict[nKey] = roc
# dfs_dict[nKey] = resdf
# dfTE_dict[nKey] = tedf
# algo_dict[nKey] = algo
# prTE_dict[nKey] = prdf
# print()
# end = time.time()
# print('Elapsed time for ',  nKey, ':', str(round(end-start, 2)))

start = time.time()
roc, resdf, tedf, algo, prdf, dftr = run_LogisticGrid(
    X_new_train_fe, X_new_test_fe, y_new_train, y_new_test)
nKey = 'full_gridLog_new'
outModel = 'D:/!bso/alertaDeterioro/' + nKey + '.pkl'
pickle_out = open(outModel, mode='wb')
pickle.dump(algo, pickle_out)
pickle_out.close()
results_dict_new[nKey] = roc
dfs_dict_new[nKey] = resdf
dfTE_dict_new[nKey] = tedf
algo_dict_new[nKey] = algo
prTE_dict_new[nKey] = prdf
dfTR_dict_new[nKey] = dftr
print()
end = time.time()
print('Elapsed time for ',  nKey, ':', str(round(end-start, 2)))

start = time.time()
roc, resdf, tedf, algo, prdf, dftr = run_LogisticGrid(
    X_new_u, X_new_test_fe, y_new_u, y_new_test)
nKey = 'u_gridLog_new'
outModel = 'D:/!bso/alertaDeterioro/' + nKey + '.pkl'
pickle_out = open(outModel, mode='wb')
pickle.dump(algo, pickle_out)
pickle_out.close()
results_dict[nKey] = roc
dfs_dict_new[nKey] = resdf
dfTE_dict_new[nKey] = tedf
algo_dict_new[nKey] = algo
prTE_dict_new[nKey] = prdf
dfTR_dict_new[nKey] = dftr
print()
end = time.time()
print('Elapsed time for ',  nKey, ':', str(round(end-start, 2)))

ensstart = time.time()
for ensemble in ensemble_dict.keys():
    start = time.time()
    # try:
    print(ensemble)
    key = 'new_full_' + ensemble
        # train model and store result
    tedf, resdf, auc, prdf, algo, dftr = run_ensemble(ensemble_dict[ensemble], X_new_train_fe,
                                                X_new_test_fe, y_new_train, y_new_test)

    outModel = 'D:/!bso/alertaDeterioro/' + key + '.pkl'
    pickle_out = open(outModel, mode='wb')
    pickle.dump(algo, pickle_out)
    pickle_out.close()
    # results_dict[key] = roc
    dfs_dict_new[key] = resdf
    prTE_dict_new[key] = prdf
    dfTE_dict_new[key] = tedf
    algo_dict_new[key] = algo
    dfTR_dict_new[key] = dftr
    print()
    # except:
       # pass
    end = time.time()
    print('Elapsed time for ',  key, ':', str(round(end-start, 1)))
# I need to check this
# final stack
del dfTR_dict_new['u_xgb_new']
del dfTR_dict_new['u_gridLog_new']
del dfTE_dict_new['u_xgb_new']
del dfTE_dict_new['u_gridLog_new']

trDFs_new = pd.concat(dfTR_dict_new, axis=1)
trTEs_new = pd.concat(dfTE_dict_new, axis=1)
#%%
start = time.time()
roc, resdf, tedf, algo, prdf, dftr = run_LogisticGrid(trDFs_new, trTEs_new, y_new_train, y_new_test)
nKey = 'stack_LogisticGrid_new'
outModel = 'D:/!bso/alertaDeterioro/' + nKey + '.pkl'
pickle_out = open(outModel, mode='wb')
pickle.dump(algo, pickle_out)
pickle_out.close()
results_dict_new[nKey] = roc
dfs_dict_new[nKey] = resdf
dfTE_dict_new[nKey] = tedf
algo_dict_new[nKey] = algo
prTE_dict_new[nKey] = prdf
dfTR_dict_new[nKey] = dftr
print()
end = time.time()
print('Elapsed time for ',  nKey, ':', str(round(end-start, 2)))
#%%
# Appending results
resDFs_new = pd.concat(dfs_dict_new, axis=0)
# resDFs['Sc_Savings'] = resDFs['totalSavings'].rank(ascending=True)
resDFs_new['Sc_Recall'] = resDFs_new['recall_50'].rank(ascending=True)
resDFs_new['Sc_BalAcc'] = resDFs_new['balAccurracy_50'].rank(ascending=True)
resDFs_new['Sc_Avg'] = resDFs_new[['Sc_Recall', 'Sc_BalAcc']].mean(axis=1)
pd.options.display.float_format = '{:,.2f}'.format

display(resDFs_new.sort_values(by='Sc_Avg', ascending=False))
# Results storing
resDFs_new.to_csv('D:/!bso/alertaDeterioro/resDF_collect_dic2022_new.csv')
# %%
pdDFs_new = pd.concat(dfTE_dict_new, axis=1)
pdDFs_new.to_csv('D:/!bso/alertaDeterioro/pdDFs_new_collect_dic2022_new.csv')
#probDFs = pdDFs.iloc[:,pdDFs.columns.str.startswith("prob")]
#display(probDFs)
#%%
fdt_dict_new = {}
for smodel in list(algo_dict_new.keys()) :
    try:
        #smodel = 'full_rusboost_0'
        fpreds = pdDFs_new[smodel]
        fpreds = pd.concat([fpreds, y_test], axis = 1)
        fpreds[smodel] = fpreds[smodel]*100

        #netSaveList = []
        tpList = []
        fpList = []
        tnList = []
        fnList = []
        accList = []
        recList = []
        preList = []
        balList = []
        rejList = []
        for i in range(100):
            name = 'clf_' + str(i)
            fpreds[name] = fpreds[smodel].apply(lambda x: 1 if x > i else 0)
            #print(i)
            #print(confusion_matrix(fpreds['default'],fpreds[name]))
            tn = confusion_matrix(fpreds['det'],fpreds[name])[0,0]
            fp = confusion_matrix(fpreds['det'],fpreds[name])[0,1]
            fn = confusion_matrix(fpreds['det'],fpreds[name])[1,0]
            tp = confusion_matrix(fpreds['det'],fpreds[name])[1,1]
            acc = (tn+tp)/(tn+fn+tp+fp)*100
            pre = tp/(tp+fp)*100
            rec = tp/(tp+fn)*100
            bal = ((tn/(tn+fn)) + (tp/(tp+fp)))/2*100
            rej = (tp+fp)/(tn+fn+tp+fp)*100
            tpList.append(tp)
            tnList.append(tn)
            fpList.append(fp)
            fnList.append(fn)
            accList.append(acc) 
            recList.append(rec) 
            preList.append(pre) 
            balList.append(bal) 
            rejList.append(rej)
        
        #dfNetSavings = pd.DataFrame(netSaveList, columns = ['netSavings'])
        dftp = pd.DataFrame(tpList, columns = ['tp'])
        dffp = pd.DataFrame(fpList, columns = ['fp'])
        dffn = pd.DataFrame(fnList, columns = ['fn'])
        dftn = pd.DataFrame(tnList, columns = ['tn'])
        dfacc = pd.DataFrame(accList, columns = ['acc'])
        dfrec = pd.DataFrame(recList, columns = ['rec'])
        dfpre = pd.DataFrame(preList, columns = ['pre'])
        dfbal = pd.DataFrame(balList, columns = ['bal'])
        dfrej = pd.DataFrame(rejList, columns = ['rej'])
        dfThresh = pd.concat([dftp, dffp, dffn, dftn, dfacc, dfpre, dfrec, dfbal, dfrej], axis=1)
        dfThresh['model'] = smodel
        fdt_dict_new[smodel] = dfThresh
    except:
        pass

#%%
fdtDFs_new = pd.concat(fdt_dict_new, axis=0)
fdtDFs_new = fdtDFs_new.unstack(level=0)
fdtFDs_new = fdtDFs_new.droplevel(0, axis = 1)
fdtDFs_new.to_csv('D:/!bso/alertaDeterioro/fdtDFs_dic2022_new.csv')
# %%
