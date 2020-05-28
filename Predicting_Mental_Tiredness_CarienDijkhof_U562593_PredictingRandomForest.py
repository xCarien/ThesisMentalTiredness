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
from sklearn.ensemble import RandomForestClassifier 
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score 
from sklearn.model_selection import GridSearchCV
from sklearn.svm import SVC
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

SVC(C=1, break_ties=False, cache_size=200, class_weight=None, coef0=0.0,
    decision_function_shape='ovr', degree=3, gamma=1, kernel='rbf', max_iter=-1,
    probability=False, random_state=None, shrinking=True, tol=0.001, verbose=False)
#%%

print(grid.best_params_)

#%%
model = "model1.csv"

features = pd.read_csv(model)
features.head(5)

#One-hot encode the data using pandas get_dummies
target_names = ['not tired', 'tired']
features = pd.get_dummies(features)
features.iloc[:,5:].head(5)
labels = np.array(features['tired'])

features = features.drop('tired', axis = 1)
#%%
features_list = list(features.columns)
features= np.array(features)
#%%
# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(features, labels, test_size = 0.2, random_state = 42)

#%%
min_samples_split = 5
n_estimators = 50


#%%  
classifier = RandomForestClassifier(criterion = "entropy", min_samples_split = min_samples_split, n_estimators = n_estimators)
classifier.fit(X_train, y_train) 
y_pred = classifier.predict(X_test)


#%%
print(confusion_matrix(y_test,y_pred)) 
print(classification_report(y_test,y_pred)) 


#train accuracy
print(confusion_matrix(y_test,y_pred)) 
print ('F1 score:', f1_score(y_test, y_pred,average="weighted"))
print ('Recall:', recall_score(y_test, y_pred,
                              average="weighted"))
print("accuracy:",accuracy_score(y_test, y_pred))
print("balanced:",balanced_accuracy_score(y_test, y_pred))


#%%
dummy = DummyClassifier(strategy="most_frequent",random_state=42)
dummy.fit(X_train, y_train)
score = dummy.score(X_test, y_test) 
print("baseline:",score)

