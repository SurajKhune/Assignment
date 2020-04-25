# -*- coding: utf-8 -*-
"""
Created on Sat Mar  7 19:41:12 2020

@author: Suraj
"""
import pandas as pd
import numpy as np
q7 = pd.read_csv('Q7.csv')
q7['Points'].mean()
q7['Points'].median()
q7['Points'].mode()
np.var(q7.Points)
np.std(q7.Points)
q7['Points'].range()
range =max(q7['Weigh'])- min(q7['Weigh'])
range
np.median(q7['Score'])
q7['Score'].mode
np.var(q7.Score)
np.std(q7.Score)
np.mean(q7['Weigh'])
np.median(q7['Weigh'])
np.var(q7['Weigh'])
np.std(q7['Weigh'])
q9 = pd.read_csv('Q9_b.csv')
import matplotlib.pyplot as plt
q9['SP'].skew()
q9['SP'].kurt()
q9['WT'].skew()
q9['WT'].kurt()
p = [34,36,36,38,38,39,39,40,40,41,41,41,41,42,42,45,49,56]
p.mean()
np.mean(p)
np.median(p)
np.var(p)
np.std(p)
import scipy.stats as stats
stats.norm.ppf(0.950,0,1)
stats.norm.ppf(0.970,0,1)
stats.norm.ppf(0.800,0,1)
stats.t.ppf(0.975,25)
stats.t.ppf(0.980,25)
stats.t.ppf(0.995,25)
Cars = pd.read_csv('Cars.csv')
import scipy.stats as stats
import pyplot as plt
import statsmodels.api as sm
sm.qqplot(Cars)
stats.probplot(Cars['MPG'], dist="norm",plot=pylab)
import pylab
import scipy.stats as st
st.probplot(Cars['MPG'],dist="norm",plot=pylab)
wc = pd.read_csv('wc-at.csv')
st.probplot(wc['AT'],dist='norm',plot=pylab)
