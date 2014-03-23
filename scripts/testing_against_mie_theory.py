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
rc('font',**{'family':'serif'})

ForceRedo = False
PLOT_DIFFERENCE = False
Material = "index_silD03"

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
	'dephot'			: 0.1,
	'cluster-file-name'	: '',
	'grain-axis-x'		: 1.0,
	'grain-axis-y'		: 1.0,
	'grain-axis-z'		: 1.0,
	'ngrain'			: 128,
	'norientations'		: 50
}



def conditional_make_data(fname,params,flags=[]):
	if ForceRedo or not os.path.exists(fname) or os.stat(fname).st_size == 0: pu.make_data(params,fname,flags=flags)

eps_vac = 1

def comp_pow(x,a):
	if x.real == 0.0: 
		if x.imag > 0: phase = np.pi/2.0
		elif x.imag < 0: phase = 3*np.pi/2.0
		else: 
			print "w"
			return 0.0
	else:
		phase = atan(x.imag/x.real)
	if a == 0:
		A = abs(x)
	else:
		A = pow(abs(x).real,a)
	return pow(abs(x),a)*( cos(phase*a) + 1j*sin(phase*a) )


def q_mie(ephot,ior_re, ior_im, a_eff):
	hc = 0.00124
	k = 2*np.pi*ephot/hc

	m = complex(ior_re, ior_im)
	rho = 2*k*a_eff*(m-1)

	rho1 = rho.real 
	rho2 = rho.imag 
	
	mag_rho = comp_pow(rho1*rho1 + rho2*rho2,0.5)
	beta = atan(rho2/rho1)

	q_abs = 1 + exp(-2*rho2)*comp_pow(rho2,-1) + 0.5*(exp(-2*rho2) - 1)*comp_pow(rho2,-2)
	q_ext = 2 + (4.0*comp_pow(mag_rho,-2))*(cos(2*beta) - exp(-rho2)*(cos(rho1 - 2*beta) - mag_rho*sin(rho1 - beta)) )
	q_scat = q_ext - q_abs 

	
	return q_abs.real,q_scat.real,q_ext.real

mat_dt = np.dtype([
	('ephot', np.float_),
	('delm_re',np.float_),
	('delm_im',np.float_),
	('deleps_re',np.float_),
	('deleps_im',np.float_)

	])

mat_data = np.loadtxt(material_file_name, dtype=mat_dt, skiprows=2)
mat_ior_re_func = interp1d(mat_data['ephot']/1000,mat_data['delm_re'] + 1)
mat_ior_im_func = interp1d(mat_data['ephot']/1000,mat_data['delm_im'])



#Test sphere
fname = data_dir+"/debugging_sphere.dat"
conditional_make_data(fname, params, flags=flags)
sph_dat = pu.load_sed_data(fname)

ephots = sph_dat['ephot']
mie_qscat = np.zeros(len(ephots))
mie_qabs = np.zeros(len(ephots))
mie_qext = np.zeros(len(ephots))
for i,E in enumerate(ephots):
	mie_qabs[i], mie_qscat[i], mie_qext[i] = q_mie(E, mat_ior_re_func(E), mat_ior_im_func(E), params['aeff'])

#Test ellipsoid
fname = data_dir+"/debugging_ellipsoid.dat"
params['grain-geometry'] = 'ellipsoid'
params['grain-axis-x'] = 1.0
params['grain-axis-y'] = 1.0
params['grain-axis-z'] = 1.0
conditional_make_data(fname, params, flags=flags)
ell_dat = pu.load_sed_data(fname)


#Test spheres
fname = data_dir+"/debugging_spheres.dat"
params['grain-geometry'] = 'spheres'
params['cluster-file-name'] = cluster_dir + "/one_sphere.targ"
conditional_make_data(fname, params, flags=flags)
clu_dat = pu.load_sed_data(fname)


Area = np.pi * pow(params['aeff'],2)

f = plt.figure()
ax_abs = f.add_subplot(311)
ax_scat = f.add_subplot(312)
ax_ext = f.add_subplot(313)

if PLOT_DIFFERENCE:

	ax_abs.plot(sph_dat['ephot'], np.divide(sph_dat['sig_abs']/Area,mie_qabs) - 1, label="sphere", ls='--')
	ax_abs.plot(ell_dat['ephot'], np.divide(ell_dat['sig_abs']/Area,mie_qabs) - 1, label="ellipsoid", ls='--')
	ax_abs.plot(clu_dat['ephot'], np.divide(clu_dat['sig_abs']/Area,mie_qabs) - 1, label="spheres", ls='--')


	ax_scat.plot(sph_dat['ephot'], np.divide(sph_dat['sig_scat']/Area,mie_qscat) - 1, label="sphere", ls='--')
	ax_scat.plot(ell_dat['ephot'], np.divide(ell_dat['sig_scat']/Area,mie_qscat) - 1, label="ellipsoid", ls='--')
	ax_scat.plot(clu_dat['ephot'], np.divide(clu_dat['sig_scat']/Area,mie_qscat) - 1, label="spheres", ls='--')


	ax_ext.plot(sph_dat['ephot'], np.divide((sph_dat['sig_scat']+ sph_dat['sig_abs'])/Area,mie_qext), label="sphere", ls='--')
	ax_ext.plot(ell_dat['ephot'], np.divide((ell_dat['sig_scat']+ ell_dat['sig_abs'])/Area,mie_qext), label="ellipsoid", ls='--')
	ax_ext.plot(clu_dat['ephot'], np.divide((clu_dat['sig_scat']+ clu_dat['sig_abs'])/Area,mie_qext), label="spheres", ls='--')

	ax_abs.set_ylabel("$Q_{abs}/Q_{MIE} - 1$")
	ax_scat.set_ylabel("$Q_{scat}/Q_{MIE} - 1$")
	ax_ext.set_ylabel("$Q_{ext}/Q_{MIE} - 1$")


else:

	ax_scat.plot(sph_dat['ephot'], sph_dat['sig_scat']/Area, label="sphere", ls='--')
	ax_scat.plot(ell_dat['ephot'], ell_dat['sig_scat']/Area, label="ellipsoid", ls='--')
	ax_scat.plot(clu_dat['ephot'], clu_dat['sig_scat']/Area, label="spheres", ls='--')

	ax_abs.plot(sph_dat['ephot'], sph_dat['sig_abs']/Area, label="sphere", ls='--')
	ax_abs.plot(ell_dat['ephot'], ell_dat['sig_abs']/Area, label="ellipsoid", ls='--')
	ax_abs.plot(clu_dat['ephot'], clu_dat['sig_abs']/Area, label="spheres", ls='--')

	ax_ext.plot(sph_dat['ephot'], (sph_dat['sig_scat']+ sph_dat['sig_abs'])/Area, label="sphere", ls='--')
	ax_ext.plot(ell_dat['ephot'], (ell_dat['sig_scat']+ ell_dat['sig_abs'])/Area, label="ellipsoid", ls='--')
	ax_ext.plot(clu_dat['ephot'], (clu_dat['sig_scat']+ clu_dat['sig_abs'])/Area, label="spheres", ls='--')

	ax_abs.plot(ephots,mie_qabs, label="MIE", color='k',lw=2,alpha=0.5)
	ax_scat.plot(ephots,mie_qscat, label="MIE", color='k',lw=2,alpha=0.5)
	ax_ext.plot(ephots,mie_qext, label="MIE", color='k',lw=2,alpha=0.5)

	ax_abs.set_ylabel("$Q_{abs}$")
	ax_scat.set_ylabel("$Q_{scat}$")
	ax_ext.set_ylabel("$Q_{ext}$")

ax_abs.set_xlabel("E (keV)")
ax_scat.set_xlabel("E (keV)")
ax_ext.set_xlabel("E (keV)")

ax_abs.legend(loc='best')
ax_scat.legend(loc='best')
ax_ext.legend(loc='best')
plt.show()