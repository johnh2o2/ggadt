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

PLOT_EMT = False

ForceRedo = False
Material = "index_silD03"
MigrationModes = [ '', 'M1', 'M2' ]
Realizations = np.arange(1,13) #np.arange(1,13) is max (goes up to 12) 
NumSpheres = [ 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096 ] #[ 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096 ]


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
	'grain-geometry' 	: 'sphere',
	'aeff' 				: 0.2,
	'ephot-min'			: 1.0,
	'ephot-max'			: 7.0,
	'dephot'			: 0.01,
	'cluster-file-name'	: None,
	'grain-axis-x'		: 1.0,
	'grain-axis-y'		: 1.0,
	'grain-axis-z'		: 1.0,
	'ngrain'			: 64,
	'norientations'		: 64
}

i = 0
Total = len(Realizations)*len(NumSpheres)*len(MigrationModes)
dt = np.dtype([
	('mig_mode',np.str_),
	('num_sph',np.int_),
	('realization',np.float_),
	('porosity',np.float_),
	('avg_qabs',np.float_),
	('avg_qscat',np.float_),
	('avg_qext',np.float_),
	('fdiff_scat',np.float_),
	('fdiff_abs',np.float_),
	('fdiff_ext',np.float_),
			])
all_data = np.empty(Total,dtype=dt)
I = 0

def stats(vals):
	avg = np.mean(vals)
	var = 0.0
	for v in vals:
		var += pow(v - avg,2)

	var/=(len(vals) - 1)
	return avg, sqrt(var)

for mig in MigrationModes:
	for ns in NumSpheres:
		for n in Realizations:
			
			
			print i," of ",Total
			params['cluster-file-name'] = cluster_dir + "/BA"+mig+"."+`ns`+"."+`n`+".targ"
			params['grain-geometry'] = 'spheres'
			sphfname = data_dir + "/exploring_fluffiness/ggadt_BA"+mig+"."+`ns`+"."+`n`+"_emin%.1f_emax%.1f_deph%.1e_ng%d_no%d_%s.dat"%(params['ephot-min'],params['ephot-max'],params['dephot'],params['ngrain'],params['norientations'],Material)

			if ForceRedo or not os.path.exists(sphfname) or os.stat(sphfname).st_size == 0: pu.make_data(params,sphfname,flags=flags)

			a,b,c = pu.get_eq_ellipsoid(params['cluster-file-name'])
			params['grain-geometry'] = 'ellipsoid'
			params['grain-axis-x'] = a 
			params['grain-axis-y'] = b 
			params['grain-axis-z'] = c 
			eqellfname = data_dir + "/exploring_fluffiness/ggadt_eqellBA"+mig+"."+`ns`+"."+`n`+"_emin%.1f_emax%.1f_deph%.1e_ng%d_no%d_%s.dat"%(params['ephot-min'],params['ephot-max'],params['dephot'],params['ngrain'],params['norientations'],Material)


			if ForceRedo or not os.path.exists(eqellfname) or os.stat(sphfname).st_size == 0: pu.make_data(params,eqellfname,flags=flags)

			sph_data = pu.load_sed_data(sphfname)
			ell_data = pu.load_sed_data(eqellfname)

			avg_sph_scat = np.mean(sph_data['sig_scat'])
			avg_ell_scat = np.mean(ell_data['sig_scat'])

			avg_sph_abs = np.mean(sph_data['sig_abs'])
			avg_ell_abs = np.mean(ell_data['sig_abs'])

			avg_sph_ext = avg_sph_abs + avg_sph_scat
			avg_ell_ext = avg_ell_abs + avg_ell_scat


			all_data[i]['avg_qscat'] = avg_sph_scat
			all_data[i]['avg_qabs'] = avg_sph_abs
			all_data[i]['avg_qext'] = avg_sph_ext
			all_data[i]['fdiff_scat'] = (avg_sph_scat - avg_ell_scat)/avg_ell_scat
			all_data[i]['fdiff_abs'] = (avg_sph_abs - avg_ell_abs)/avg_ell_abs
			all_data[i]['fdiff_ext'] = (avg_sph_ext - avg_ell_ext)/avg_ell_ext
			all_data[i]['porosity'] = pu.get_porosity(params['cluster-file-name'] )

			all_data[i]['mig_mode'] = mig 
			all_data[i]['num_sph'] = ns 
			all_data[i]['realization'] = n

			
			i+=1

all_data = np.sort(all_data,order='porosity')



f = plt.figure()
ax = f.add_subplot(111)
ax.set_xlim(0,1)
#ax.set_ylabel("$\\left<Q\\right>$")
ax.set_ylabel("$\\Delta\\sigma /\\sigma$")
ax.set_xlabel("Porosity")



mode = "fdiff_"
name="scat"
ax.scatter(all_data['porosity'],all_data[mode+name],color='b',alpha=0.5,label = name)

name="abs"
ax.scatter(all_data['porosity'],all_data[mode+name],color='g',alpha=0.5, label = name)


name="ext"
ax.scatter(all_data['porosity'],all_data[mode+name],color='k', label=name)

ax.legend(loc=2)

if PLOT_EMT:

	por_emt, abs_emt, scat_emt, ext_emt, fudge = np.load(data_dir + "/exploring_fluffiness/emt_results.npy")
	ax.plot(por_emt, scat_emt, color='b',lw=2,alpha=0.5,ls='--')
	ax.plot(por_emt, abs_emt, color='g',lw=2,alpha=0.5,ls='--')
	ax.plot(por_emt, ext_emt, color='k',lw=2,alpha=0.5,ls='--')


	porind = int(0.25*len(por_emt))
	di = 1
	ax.annotate('EMT scat', xy=(por_emt[porind+2*di+1],scat_emt[porind+2*di+2]),  xycoords='data',
	            xytext=(0.1,0.35), textcoords='axes fraction',
	            arrowprops=dict(facecolor='black', width=0.5,headwidth=2.0),
	            horizontalalignment='left', verticalalignment='top',
	            )
	ax.annotate('EMT ext', xy=(por_emt[porind+di],ext_emt[porind+di]-0.01),  xycoords='data',
	            xytext=(0.08,0.4), textcoords='axes fraction',
	            arrowprops=dict(facecolor='black', width=0.5,headwidth=2.0),
	            horizontalalignment='left', verticalalignment='top',
	            )
	ax.annotate('EMT abs', xy=(por_emt[porind],abs_emt[porind]-0.01),  xycoords='data',
	            xytext=(0.06,0.45), textcoords='axes fraction',
	            arrowprops=dict(facecolor='black', width=0.5,headwidth=2.0),
	            horizontalalignment='left', verticalalignment='top',
	            )

	ax.text(0.85,0.05,"EMT: $f_{vac}=%.2f P$"%(fudge),va='bottom',ha='right',fontsize=12,transform=ax.transAxes)

footnote = "Material: %s"%(material_name)
footnote = footnote + "\nMaterial file: %s"%(Material)
footnote = footnote + "\nEnergy range: [%.1f, %.1f] keV (step: %.2f eV)"%(params['ephot-min'],params['ephot-max'],params['dephot']*1000)
footnote = footnote + "\na_eff: %.1f"%(params['aeff'])
footnote = footnote + "\nngrain: %d"%(params['ngrain'])
footnote = footnote + "\nnorientations: %d"%(params['norientations'])

f.text(0.15,0.15,footnote,va='bottom',ha='left',fontsize=10)
pu.add_timestamp(f)
f.savefig(parent_dir+"/cross_sections_vs_porosity.png")
plt.show()

