#Exploring effects of porosity on scattering, extinction, absorption			

from math import *
import sys
import os
import numpy as np 
from scipy.interpolate import RectBivariateSpline
from scipy.interpolate import interp1d
import matplotlib.pyplot as plt
import pylab
from matplotlib.colors import BoundaryNorm
from matplotlib.ticker import MaxNLocator
from matplotlib.ticker import MultipleLocator, FormatStrFormatter
from matplotlib import rc
from installation_vars import *
import plot_utilities as pu

params = {'legend.fontsize': 10,
          'legend.linewidth': 1}
pylab.rcParams.update(params)
rc('font',**{'family':'serif'})

ForceRedo = False
ForceRedo_answer = False
Material = "index_silD03"
cluster = "BA.4096.1"
#ngrains = np.array([ 16, 32, 64, 128 ])
#norientations = np.array([ 1, 5, 10, 50, 100, 200 ])
ngrains = np.array([32, 64, 128, 256])
norientations = np.array([64])
#dephots = np.logspace(-3,0.5,num=10)
dephots = np.array([pow(10,-2)])


Answer_norientations = 100
Answer_ngrain = 1024
Answer_dephot = pow(10,-2)



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

params['ngrain'] = Answer_ngrain
params['norientations'] = Answer_norientations
params['dephot'] = Answer_dephot
ans_fname = data_dir + "/exploring_fluffiness/ggadt_ans"+cluster+"_emin%.1f_emax%.1f_deph%.1e_ng%d_no%d_%s.dat"%(params['ephot-min'],params['ephot-max'],Answer_dephot,params['ngrain'],params['norientations'],Material)


if ForceRedo_answer or not os.path.exists(ans_fname) or os.stat(ans_fname).st_size == 0: 
	print "making answer..."
	pu.make_data(params,ans_fname,flags=flags)

ans_dat = pu.load_sed_data(ans_fname)

RMS_scat = np.zeros(( len(ngrains), len(norientations), len(dephots) ))
RMS_abs = np.zeros(( len(ngrains), len(norientations), len(dephots) ))
RMS_ext = np.zeros(( len(ngrains), len(norientations), len(dephots) ))

avg_scat_ans = np.mean(ans_dat['sig_scat'])
avg_abs_ans = np.mean(ans_dat['sig_abs'])
avg_ext_ans = avg_scat_ans + avg_abs_ans

for j, ng in enumerate(ngrains):
	for o, nor in enumerate(norientations):
		for q, de in enumerate(dephots):
			params['ngrain'] = ng 
			params['norientations'] = nor
			params['dephot'] = de 
			fname = data_dir + "/exploring_fluffiness/ggadt_"+cluster+"_emin%.1f_emax%.1f_deph%.1e_ng%d_no%d_%s.dat"%(params['ephot-min'],params['ephot-max'],params['dephot'],params['ngrain'],params['norientations'],Material)

			if ForceRedo or not os.path.exists(fname) or os.stat(fname).st_size == 0: pu.make_data(params,fname,flags=flags)
			
			dat = pu.load_sed_data(fname)

			avg_scat = np.mean(dat['sig_scat'])
			avg_abs = np.mean(dat['sig_abs'])
			avg_ext = avg_abs + avg_scat


			RMS_scat[j][o][q] = avg_scat/avg_scat_ans - 1
			RMS_abs[j][o][q] = avg_abs/avg_abs_ans - 1
			RMS_ext[j][o][q] = avg_ext/avg_ext_ans - 1
			#sqrt(np.mean([ pow(dat['sig_abs'][i]/ans_dat['sig_abs'][i] - 1, 2) for i in range(0,len(dat)) ]))
			#RMS_ext[j][o][q] = sqrt(np.mean([ pow((dat['sig_scat'][i]+dat['sig_abs'][i])/(ans_dat['sig_scat'][i] + ans_dat['sig_abs'][i]) - 1, 2) for i in range(0,len(dat)) ]))


footnote = "Cluster: %s"%(cluster)
footnote = footnote + "\nMaterial: %s"%(material_name)
footnote = footnote + "\nMaterial file: %s"%(Material)
#footnote = footnote + "\nEnergy range: [%.1f, %.1f] keV (step: %.2f eV)"%(params['ephot-min'],params['ephot-max'],params['dephot']*1000)
footnote = footnote + "\na_eff: %.1f"%(params['aeff'])

f = plt.figure()
pu.add_timestamp(f)
f.text(0.15,0.15,footnote,va='bottom',ha='left',fontsize=10)
ax = f.add_subplot(311)
ax2 = f.add_subplot(312)
ax3 = f.add_subplot(313)

colors = ['b','g','r','k']
lss = [ ':','--','-']
dats = [ RMS_abs, RMS_scat , RMS_ext ]
labs = ["abs", "scat", "ext"]
for j,AX in enumerate([ax, ax2, ax3 ]):
	for i in range(0,len(norientations)):
		AX.plot(ngrains,dats[j][:,i,0],label="norien=%d"%(norientations[i]),color=colors[i%len(colors)],ls=lss[i/len(colors)])
	AX.text(0.1,0.9,labs[j],transform=AX.transAxes, ha='left',va='top')
	#AX.axhline(0.01,color='k',ls='-',lw=2)
	#AX.yaxis.set_major_locator(plt.MultipleLocator(0.01))
	#AX.yaxis.set_minor_locator(plt.MultipleLocator(0.005))
	if j != 2: AX.set_xticks(())
	AX.set_xscale('log')
	AX.set_yscale('log')
	AX.set_ylim(pow(10,-5),pow(10,-1))
	#AX.legend(loc='best')
ax.legend(loc='upper center', bbox_to_anchor=(0.5, 1.2),
          ncol=2, fancybox=True, shadow=True)

f.text(0.5, 0.04, 'ngrain', ha='center', va='center')
f.text(0.06, 0.5, 'fractional difference', ha='center', va='center', rotation='vertical')
f.subplots_adjust(hspace=0)
plt.show()

