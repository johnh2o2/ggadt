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


draine_dt = np.dtype([('theta',np.float_),('adt',np.float_),('mie',np.float_)])

sph_data_name = data_dir + "/testing_spheres.dat"

adt_mie_data = np.loadtxt("../calladt.out",usecols=(0,2,3),dtype=draine_dt)
theta_draine = adt_mie_data['theta']*pu.conv
ADT = adt_mie_data['adt']
MIE = adt_mie_data['mie']

params = {
				'aeff' 					: 0.2,
				'grain-geometry' 		: "spheres",
			    'ephot' 				: 2.0,
			    'ior-re' 				: -1.00*pow(10.0,-4.0),
			    'ior-im' 				: 1.00*pow(10.0,-4.0),
			    'ngrain' 				: 512,
			    'nscatter'				: 256,
			    'norientations'			: 1,
			    'euler-angle-mode'		: 'random',
			    'euler-angle-file'		: None,
			    'max-angle'				: 10000.,
			    'cluster-file-name'		: cluster_dir + "/one_sphere.targ"
			   #'nangle' 				: 1,
			   #'euler-angle-mode' 		: 'file',
			   #'euler-angle-file' 		: parent_dir+'/eul_angle_file.dat'
}

pu.make_data(params,sph_data_name)
spheres_mod_data = pu.load_data(sph_data_name)
spheres_mod_data = pu.convert_to_arcseconds(spheres_mod_data)

func = pu.make_data_function(spheres_mod_data)
theta_ggadt, spheres_dQdScat, spheres_dQdScat_var = pu.get_1d_avg(func,boundaries=[0,params['max-angle'],0,params['max-angle']])

f = plt.figure()
ax = f.add_subplot(111)
ax.set_yscale('log')
ax.plot(theta_draine,np.multiply(np.power(theta_draine/pu.conv,2),ADT),color='k',label='ADT',ls='-',lw=2)
ax.plot(theta_ggadt,np.multiply(np.power(theta_ggadt/pu.conv,2),spheres_dQdScat),color='b',label="GGADT",ls='--')
ax.set_xscale('log')
ax.set_ylabel("$dQ_{scat}/d\\Omega$")
ax.set_xlabel("$\\Theta$ [arcseconds]")
ax.legend(loc='best')
plt.show()
