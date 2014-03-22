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

rc('font',**{'family':'serif','serif':['Palatino']})

ForceRedo = False
Material = "index_silD03"
cluster = "BA.256.1"
ngrains = np.array([ 32, 64, 128, 256, 512, 1024 ])
norientations = np.array([ 10, 50, 100, 150, 200])

'''
MigrationModes = [ '', 'M1', 'M2' ]
Realizations = np.arange(1,5) #np.arange(1,13) is max (goes up to 12) 
NumSpheres = [ 8, 16, 32, 64, 128, 256, 512, 1024 ] #[ 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096 ]
'''

cluster_file_name = cluster_dir + "/"+cluster+".targ"
material_dir = data_dir + "/materials"
material_file_name = material_dir + "/" + Material


MaterialNames = { 	'index_CpeD03' :  "graphite, E perp to c axis, improved eps2 in X-ray", 
					'index_silD03' : "amorphous olivine-like astrosilicate" 
				}


material_name = MaterialNames[Material]
flags = [ 'sed' ]



params = {
	'material' 			: 'custom',
	'material-file' 	: material_file_name,
	'grain-geometry' 	: 'spheres',
	'aeff' 				: 0.2,
	'ephot-min'			: 1.0,
	'ephot-max'			: 7.0,
	'dephot'			: 0.1,
	'cluster-file-name'	: cluster_file_name,
	'grain-axis-x'		: 1.0,
	'grain-axis-y'		: 1.0,
	'grain-axis-z'		: 1.0,
	'ngrain'			: None,
	'norientations'		: None
}

params['ngrain'] = ngrains[-1]
params['norientations'] = norientations[-1]
ans_fname = data_dir + "/exploring_fluffiness/ggadt_ans"+cluster+"_emin%.1f_emax%.1f_deph%.1e_ng%d_no%d_%s.dat"%(params['ephot-min'],params['ephot-max'],params['dephot'],params['ngrain'],params['norientations'],Material)


if ForceRedo or not os.path.exists(ans_fname) or os.stat(ans_fname).st_size == 0: pu.make_data(params,ans_fname,flags=flags)

ans_dat = pu.load_sed_data(ans_fname)

RMS_scat = np.zeros((len(ngrains),len(norientations)))
RMS_abs = np.zeros((len(ngrains),len(norientations)))
RMS_ext = np.zeros((len(ngrains),len(norientations)))


for j, ng in enumerate(ngrains):
	for o, nor in enumerate(norientations):
		params['ngrain'] = ng 
		params['norientations'] = nor
		fname = data_dir + "/exploring_fluffiness/ggadt_"+cluster+"_emin%.1f_emax%.1f_deph%.1e_ng%d_no%d_%s.dat"%(params['ephot-min'],params['ephot-max'],params['dephot'],params['ngrain'],params['norientations'],Material)

		if ForceRedo or not os.path.exists(fname) or os.stat(fname).st_size == 0: pu.make_data(params,fname,flags=flags)
		
		dat = pu.load_sed_data(fname)

		RMS_scat[j][o] = sqrt(np.mean([ pow(dat['sig_scat'][i]/ans_dat['sig_scat'][i] - 1, 2) for i in range(0,len(dat)) ]))
		RMS_abs[j][o] = sqrt(np.mean([ pow(dat['sig_abs'][i]/ans_dat['sig_abs'][i] - 1, 2) for i in range(0,len(dat)) ]))
		RMS_ext[j][o] = sqrt(np.mean([ pow((dat['sig_scat'][i]+dat['sig_abs'][i])/(ans_dat['sig_scat'][i] + ans_dat['sig_abs'][i]) - 1, 2) for i in range(0,len(dat)) ]))


footnote = "Cluster: %s"%(cluster)
footnote = footnote + "\nMaterial: %s"%(material_name)
footnote = footnote + "\nMaterial file: %s"%(Material)
footnote = footnote + "\nEnergy range: [%.1f, %.1f] keV (step: %.2f eV)"%(params['ephot-min'],params['ephot-max'],params['dephot']*1000)
footnote = footnote + "\na_eff: %.1f"%(params['aeff'])
footnote = footnote + "\nnorientations: %d"%(params['norientations'])

f = plt.figure()
pu.add_timestamp(f)
f.text(0.15,0.15,footnote,va='bottom',ha='left',fontsize=10)
ax = f.add_subplot(111)
ax.set_xlabel("ngrain")
ax.set_ylabel("Accuracy")
colors = ['b','g','r','k']
lss = [ ':', '--','-']
for i in range(0,len(norientations)):
	ax.plot(ngrains,RMS_ext[:,i]*100,label="ext, norien. = %d"%(norientations[i]),color=colors[i%len(colors)],ls=lss[i/len(colors)])

ax.set_xscale('log')
ax.set_yscale('log')
ax.legend(loc='best')
plt.show()

