# -*- coding: utf-8 -*-
"""
Created on Sun May 24 10:56:50 2020

@author: carie
"""



#%%
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.dummy import DummyClassifier
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score 
from sklearn.svm import SVC
from sklearn.metrics import balanced_accuracy_score
from sklearn.metrics import f1_score
from sklearn.metrics import recall_score

#%%
model = "model1.csv"

features_m1 = pd.read_csv(model)
features_m1.head(5)

#One-hot encode the data using pandas get_dummies
features = pd.get_dummies(features_m1)
features.iloc[:,5:].head(5)
labels = np.array(features['tired'])

features= features.drop('tired', axis = 1)
#%%
features_list = list(features.columns)
features= np.array(features)
#%%
# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(features, labels, test_size = 0.2, random_state = 42)

#%%
#Feature Scaling 
from sklearn.preprocessing import StandardScaler 
sc = StandardScaler() 
X_train = sc.fit_transform(X_train) 
X_test = sc.transform(X_test)

#%%
 #%%

param_grid = {'C': [0.1,1,10,100], 'gamma' : [1,0.1,0.01,0.001],'kernel': ['rbf',"sigmoid"]}
grid = GridSearchCV(SVC(),param_grid, refit = True, verbose = 2)
grid.fit(X_train, y_train)

#SVC(C=1, break_ties=False, cache_size=200, class_weight=None, coef0=0.0,
#    decision_function_shape='ovr', degree=3, gamma=1, kernel='rbf', max_iter=-1,
 #   probability=False, random_state=None, shrinking=True, tol=0.001,
  #  verbose=False)
#%%

print(grid.best_params_)


# m1: {'C': 10, 'gamma': 0.1, 'kernel': 'rbf'}
#m1 : {'C': 1, 'gamma': 1, 'kernel': 'rbf'}
#m12: {'C': 1, 'gamma': 0.01, 'kernel': 'sigmoid'}

#m1a: {'C': 100, 'gamma': 0.01, 'kernel': 'rbf'}
#m1a1: {'C': 10, 'gamma': 0.1, 'kernel': 'rbf'}
#m1a2: {'C': 100, 'gamma': 0.01, 'kernel': 'rbf'}

#m2: {'C': 100, 'gamma': 0.001, 'kernel': 'rbf'}
#m21: {'C': 1, 'gamma': 0.1, 'kernel': 'rbf'}
#m22: {'C': 100, 'gamma': 0.001, 'kernel': 'rbf'}

#m3: {'C': 100, 'gamma': 0.01, 'kernel': 'rbf'}
#m31: {'C': 1, 'gamma': 1, 'kernel': 'rbf'}
#m32: {'C': 0.1, 'gamma': 1, 'kernel': 'rbf'}


#m3a: {'C': 100, 'gamma': 0.01, 'kernel': 'rbf'}
#m3a1: {'C': 100, 'gamma': 0.01, 'kernel': 'rbf'}
#m3a2: {'C': 100, 'gamma': 0.001, 'kernel': 'rbf'}

#m4:{'C': 1, 'gamma': 1, 'kernel': 'rbf'}
#m41: {'C': 1, 'gamma': 1, 'kernel': 'rbf'}
#m42: {'C': 1, 'gamma': 1, 'kernel': 'rbf'}

#4a: {'C': 100, 'gamma': 1, 'kernel': 'rbf'}
#4a1: {'C': 1, 'gamma': 1, 'kernel': 'rbf'}
#4a2: {'C': 10, 'gamma': 1, 'kernel': 'rbf'}

#m5: {'C': 1, 'gamma': 1, 'kernel': 'rbf'}
#m51: {'C': 1, 'gamma': 1, 'kernel': 'rbf'}

#%%

#%%
model = "model1.csv"

features_m1 = pd.read_csv(model)
features_m1.head(5)

#One-hot encode the data using pandas get_dummies
features = pd.get_dummies(features_m1)
features.iloc[:,5:].head(5)
labels = np.array(features['tired'])

features= features.drop('tired', axis = 1)
#%%
features_list = list(features.columns)
features= np.array(features)
#%%
# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(features, labels, test_size = 0.2, random_state = 42)

#%%
#Feature Scaling 
from sklearn.preprocessing import StandardScaler 
sc = StandardScaler() 
X_train = sc.fit_transform(X_train) 
X_test = sc.transform(X_test)

#%%
print(classification_report(y_test,y_pred)) 
print(confusion_matrix(y_test,y_pred)) 
print ('F1 score:', f1_score(y_test, y_pred,average=None))
print ('Recall:', recall_score(y_test, y_pred,
                              average=None))
print("accuracy:",accuracy_score(y_test, y_pred))
print("balanced:",balanced_accuracy_score(y_test, y_pred))

#%%
dummy = DummyClassifier(strategy="most_frequent",random_state=42)
dummy.fit(X_train, y_train)
score = dummy.score(X_test, y_test) 
print("baseline:",score)

