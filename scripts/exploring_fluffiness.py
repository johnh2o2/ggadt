#Exploring effects of porosity on scattering, extinction, absorption			

from math import *
import sys
import os
import numpy as np 
from scipy.interpolate import RectBivariateSpline
from scipy.interpolate import interp1d
import matplotlib.pyplot as plt
from matplotlib.colors import BoundaryNorm
from matplotlib.ticker import MaxNLocator
from matplotlib.ticker import MultipleLocator, FormatStrFormatter
from matplotlib import rc
from installation_vars import *
import plot_utilities as pu


#Want to look at: 
# 1) Change in AVERAGE efficiency of {scattering, absorption, extinction} as a function of porosity
#			+ monte carlo over the different BA's that Bruce has
#			A) Graphite
#			B) Silicate
#
# 2) A plot like fig 7 in D03a, except the ratio of BA and sphere efficiencies
rc('font',**{'family':'serif'})
## for Palatino and other serif fonts use:
#rc('font',**{'family':'serif','serif':['Palatino']})
