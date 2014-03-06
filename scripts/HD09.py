from math import *
import sys
import os
import numpy as np 
from scipy.interpolate import RectBivariateSpline
import matplotlib.pyplot as plt
from matplotlib.colors import BoundaryNorm
from matplotlib.ticker import MaxNLocator
from installation_vars import *
import plot_utilities as pu

def make_data(params,fname):
	clargs = " -d "
	for p in params:
		clargs = clargs + " --"+p+"="+`params[p]`

	command = ggadt+clargs+" --use-experimental-fft > "+fname
	print command
	os.system(command)
aeff=0.25398416831
NS = 256
defaults = {
				'aeff' 					: aeff,
				'grain-geometry' 		: "spheres",
			    'ephot' 				: 2.0,
			    'ior-re' 				: -1.920*pow(10.0,-4.0),
			    'ior-im' 				: 2.807*pow(10.0,-5.0),
			    'ngrain' 				: 1024,
			    'nscatter'				: 512,
			    'norientations'			: 1,
			    'euler-angle-mode'		: 'random',
			    'euler-angle-file'		: None,
			    'max-angle'				: 3500.
			   #'nangle' 				: 1,
			   #'euler-angle-mode' 		: 'file',
			   #'euler-angle-file' 		: parent_dir+'/eul_angle_file.dat'
}
#Figure 3


params_3BA = { 
			   'cluster-file-name' 		: cluster_dir+"/BA.256.1.targ",

			   'grain-geometry' 		: defaults['grain-geometry'],
			   'aeff' 					: defaults['aeff'],
			   'ephot' 					: defaults['ephot'],
			   'ior-re' 				: defaults['ior-re'],
			   'ior-im' 				: defaults['ior-im'],
			   'ngrain' 				: defaults['ngrain'],
			   'nscatter'				: defaults['nscatter'],
			   'norientations'			: defaults['norientations'],
			   'euler-angle-mode'		: defaults['euler-angle-mode'],
			   'euler-angle-file'		: defaults['euler-angle-file'],
			   'max-angle'				: defaults['max-angle']
			   
			   }
params_3BAM1 = { 
			   'cluster-file-name' 		: cluster_dir+"/BAM1.256.1.targ",

			   'grain-geometry' 		: defaults['grain-geometry'],
			   'aeff' 					: defaults['aeff'],
			   'ephot' 					: defaults['ephot'],
			   'ior-re' 				: defaults['ior-re'],
			   'ior-im' 				: defaults['ior-im'],
			   'ngrain' 				: defaults['ngrain'],
			   'nscatter'				: defaults['nscatter'],
			   'norientations'			: defaults['norientations'],
			   'euler-angle-mode'		: defaults['euler-angle-mode'],
			   'euler-angle-file'		: defaults['euler-angle-file'],
			   'max-angle'				: defaults['max-angle']
			   }
params_3BAM2 = { 
			   'cluster-file-name' 		: cluster_dir+"/BAM2.256.1.targ",

			   'grain-geometry' 		: defaults['grain-geometry'],
			   'aeff' 					: defaults['aeff'],
			   'ephot' 					: defaults['ephot'],
			   'ior-re' 				: defaults['ior-re'],
			   'ior-im' 				: defaults['ior-im'],
			   'ngrain' 				: defaults['ngrain'],
			   'nscatter'				: defaults['nscatter'],
			   'norientations'			: defaults['norientations'],
			   'euler-angle-mode'		: defaults['euler-angle-mode'],
			   'euler-angle-file'		: defaults['euler-angle-file'],
			   'max-angle'				: defaults['max-angle']
			   }
fname_3BA = data_dir+"/HD09_f3BA.dat"
fname_3BAM1 = data_dir+"/HD09_f3BAM1.dat"
fname_3BAM2 = data_dir+"/HD09_f3BAM2.dat"

#Make data
make_data(params_3BA,fname_3BA)
make_data(params_3BAM1,fname_3BAM1)
make_data(params_3BAM2,fname_3BAM2)

#Load data
data_3BA = pu.load_data(fname_3BA)
data_3BAM1 = pu.load_data(fname_3BAM1)
data_3BAM2 = pu.load_data(fname_3BAM2)

#Convert to arcsec
data_3BA = pu.convert_to_arcseconds(data_3BA)
data_3BAM1 = pu.convert_to_arcseconds(data_3BAM1)
data_3BAM2 = pu.convert_to_arcseconds(data_3BAM2)

#Filter out high scattering angles
data_3BA = pu.filter_data(data_3BA)
data_3BAM1 = pu.filter_data(data_3BAM1)
data_3BAM2 = pu.filter_data(data_3BAM2)

#Make interpolation functions
func_3BA = pu.make_data_function(data_3BA)
func_3BAM1 = pu.make_data_function(data_3BAM1)
func_3BAM2 = pu.make_data_function(data_3BAM2)

#Get slices
theta_3BA,avg_3BA,var_3BA = pu.get_1d_avg(func_3BA)
theta_3BAM1,avg_3BAM1,var_3BAM1 = pu.get_1d_avg(func_3BAM1)
theta_3BAM2,avg_3BAM2,var_3BAM2 = pu.get_1d_avg(func_3BAM2)


Figure3 = plt.figure(1)
ax3 = Figure3.add_subplot(111)
ax3.set_xlim(10,2000)
ax3.set_xscale('log')
ax3.set_yscale('log')
ax3.text(30, 2*pow(10,-4),"$E_{\\gamma}=%.1f$ keV"%(defaults['ephot']))
ax3.text(600,pow(10,-2),"$a_{eff}=%.3f$\n$N_s=%d$\n$N_{grain}=%d$\n$N_{scatter}=%d$"%(defaults['aeff'],NS,defaults['ngrain'],defaults['nscatter']))

ax3.set_xlabel("$\\Theta$ [arcsec]")
ax3.set_ylabel("$\\Theta^2\\left<dQ_{sca}/d\\Omega\\right>$")
ax3.plot(theta_3BA,np.multiply(np.power(theta_3BA/pu.conv,2),avg_3BA),label='BA',color='blue',lw=2)
#ax3.fill_between(theta_3BA,np.multiply(np.power(theta_3BA/pu.conv,2),avg_3BA-var_3BA),np.multiply(np.power(theta_3BA/pu.conv,2),avg_3BA+var_3BA),facecolor='blue',alpha=0.3)

ax3.plot(theta_3BAM1,np.multiply(np.power(theta_3BAM1/pu.conv,2),avg_3BAM1),label='BAM1',color='red',lw=2)
#ax3.fill_between(theta_3BAM1,np.multiply(np.power(theta_3BAM1/pu.conv,2),avg_3BAM1-var_3BAM1),np.multiply(np.power(theta_3BAM1/pu.conv,2),avg_3BAM1+var_3BAM1),facecolor='red',alpha=0.3)

ax3.plot(theta_3BAM2,np.multiply(np.power(theta_3BAM2/pu.conv,2),avg_3BAM2),label='BAM2',color='orange',lw=2)
#ax3.fill_between(theta_3BAM2,np.multiply(np.power(theta_3BAM2/pu.conv,2),avg_3BAM2-var_3BAM2),np.multiply(np.power(theta_3BAM2/pu.conv,2),avg_3BAM2+var_3BAM2),facecolor='orange',alpha=0.3)

ax3.legend(loc=3)
Figure3.suptitle("Figure 3 (Heng & Draine 2009)")
Figure3.savefig(data_dir+"/HD09_f3.png")
plt.show()

