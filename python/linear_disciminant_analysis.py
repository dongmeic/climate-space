# Created by Dongmei Chen

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.preprocessing import StandardScaler

inpath = "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
outpath = "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"

indata = pd.read_csv(inpath + "bioclimatic_values_1996_2015_r.csv")
y = indata['beetles']
data = indata.drop(['beetles','hosts'], axis=1)
scaler = StandardScaler()
x = scaler.fit_transform(data)
# set up a classifier 
clf = LinearDiscriminantAnalysis(n_components=4)
# transform x matrix into linear discriminant
scaled_x = clf.fit(x, y).transform(x)
scaled_x.means_
scaled_x.scalings_
plt.plot(scaled_x[y==1,0], scaled_x[y==1,1])

