from math import *
import numpy as np
import matplotlib.pyplot as plt
import plot_utilities as pu

fname = "testing_ellips_integrated.dat"
flags = ['integrated']
params = {
		'aeff' : 0.2,
		'material-file' : '../data/materials/index_silD03',
		'grain-axis-x' : 1.414,
		'grain-axis-y' : 1.000,
		'grain-axis-z' : 1.000,
		'nephots' : 256,
		'ephot-min' : 0.1,
		'ephot-max' : 3.0
	}

pu.make_data(params,fname,flags=flags)

dat = pu.load_sed_data(fname)

fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(dat['ephot'],dat['sig_ext'])
plt.show()
