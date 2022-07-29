# Perform multilinear regression for GEM data produces by sampling scheme with polinomials of order 1

import numpy as np 
import pandas as pd

from sklearn import linear_model

features_names = ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho']
target_names = ['te_transp_flux', 'ti_transp_flux', 'te_transp_flux_std', 'ti_transp_flux_std']

# Load data

df = pd.read_csv('gem_data/resuq_main_ti_transp_flux_all_moj202gj_11.csv')

#print(data)

# Multilinear regression

mlr = linear_model.LinearRegression()
mlr.fit(df[features_names], df[target_names[1]])

print(mlr.coef_)

# Predict for training sample

predicted = mlr.predict(df[features_names])

# Calculate and display fit error

abs_error = np.abs(predicted - df[target_names[1]])
rel_error = abs_error / df[target_names[1]]

print('Relative error of the Linear regression fit is: \n {0} '.format(rel_error))
