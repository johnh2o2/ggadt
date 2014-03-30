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

rc('font',**{'family':'serif'})

FORCE_REDO = True

tag1 = "iron"
tag2 = "silicate"

cluster = cluster_dir + "/BA.16.1.targ"

file1 = data_dir + "/materials/index_Fe"
file2 = data_dir + "/materials/index_silD03"

all1 =  data_dir + "/BA.16.1.composition"
all2 =  data_dir + "/BA.16.1.composition2"
mix12 = data_dir + "/BA.16.1.composition3"


output1 = "output1.dat"
output2 = "output2.dat"
output12 = "output12.dat"


flags = [ 'sed' ]
params = {
	'material-file1' 	: file1,
	'material-file2' 	: file2,
	'material-tag1' 	: tag1,
	'material-tag2' 	: tag2,	
	'agglom-composition-file' : None,
	'grain-geometry' 	: 'spheres',
	'aeff' 				: 0.2,
	'ephot-min'			: 0.2,
	'ephot-max'			: 3.0,
	'dephot'			: 0.001,
	'cluster-file-name'	: cluster,
	'grain-axis-x'		: 1.0,
	'grain-axis-y'		: 1.0,
	'grain-axis-z'		: 1.0,
	'ngrain'			: 128,
	'norientations'		: 64
}

Area = np.pi*pow(params['aeff'],2)

params['agglom-composition-file'] = all1
if FORCE_REDO or not os.path.exists(output1) : pu.make_data(params,output1,flags=flags)

params['agglom-composition-file'] = all2
if FORCE_REDO or not os.path.exists(output2) : pu.make_data(params,output2,flags=flags)

params['agglom-composition-file'] = mix12
if FORCE_REDO or not os.path.exists(output12) : pu.make_data(params,output12,flags=flags)

data1 = pu.load_sed_data(output1)
data2 = pu.load_sed_data(output2)
data12 = pu.load_sed_data(output12)

f = plt.figure()


ax_scat = f.add_subplot(211)
ax_abs = f.add_subplot(212, sharex=ax_scat)

for AX in [ ax_scat, ax_abs ]:
	AX.xaxis.set_major_locator(plt.MultipleLocator(1.0))
	AX.xaxis.set_minor_locator(plt.MultipleLocator(0.2))

	AX.yaxis.set_major_locator(plt.MultipleLocator(0.3))
	AX.yaxis.set_minor_locator(plt.MultipleLocator(0.1))

	AX.set_ylim(-0.05,1.5)
plt.setp(ax_scat.get_xticklabels(), visible=False)
plt.setp(ax_abs.get_xticklabels(), visible=True)
ax_abs.set_xlabel("E [keV]")
ax_abs.set_ylabel("$Q_{abs}$")
ax_scat.set_ylabel("$Q_{scat}$")
ax_scat.plot(data1['ephot'], data1['sig_scat']/Area,color='b',label="All "+tag1)
ax_scat.plot(data12['ephot'], data12['sig_scat']/Area ,color='k',ls='--',label="Mix "+tag1+" and "+tag2)
ax_scat.plot(data2['ephot'], data2['sig_scat']/Area,color='r',label="All "+tag2)

ax_abs.plot(data1['ephot'], data1['sig_abs']/Area,color='b',label="All "+tag1)
ax_abs.plot(data12['ephot'], data12['sig_abs']/Area,color='k',ls='--',label="Mix "+tag1+" and "+tag2)
ax_abs.plot(data2['ephot'], data2['sig_abs']/Area,color='r',label="All "+tag2)

ax_scat.legend(loc='best')

f.subplots_adjust(hspace=0)

plt.show()