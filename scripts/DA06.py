#Reproduces figures in DA06			

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

# min/max scattering angles to deal with
xmin = -4000
xmax = 4000
ymin = xmin
ymax = xmax


def make_data(params,fname):
	clargs = " -d "
	for p in params:
		clargs = clargs + " --"+p+"="+`params[p]`

	command = ggadt+clargs+" > "+fname
	#print command
	os.system(command)

ba = 0.707106781

#FIGURE 1 PARAMS
params_f1r1c1 = {'grain-geometry' 		: "sphere",
			   #'cluster-file-name' 		: None,
			   'aeff' 					: 0.1,
			   #'grain-axis-x' 			: 1.0,
			   #'grain-axis-y' 			: 1.0,
			   #'grain-axis-z' 			: 1.0,
			   'ephot' 					: 0.5,
			   'ior-re' 				: -2.079*pow(10.0,-3.0),
			   'ior-im' 				: 3.201*pow(10.0,-3.0),
			   'grid-width' 			: 8.0,
			   'ngrid' 					: 1024,
			   'nangle' 				: 1,
			   'euler-angle-mode' 		: 'random',
			   #'euler-angle-file' 		: parent_dir+'/eul_angle_file.dat'
			   }
params_f1r1c2 = { 'grain-geometry' 		: "sphere",
			   'cluster-file-name' 		: None,
			   'aeff' 					: 0.1,
			   'ephot' 					: 1.0,
			   'ior-re' 				: -7.152*pow(10.0,-4.0),
			   'ior-im' 				: 1.887*pow(10.0,-4.0),
			   'grid-width' 			: 8.0,
			   'ngrid' 					: 1024,
			   'nangle' 				: 1,
			   'euler-angle-mode' 		: 'file',
			   'euler-angle-file' 		: parent_dir+'/eul_angle_file.dat'
			  }
params_f1r1c3 = { 'grain-geometry' 		: "sphere",
			   'cluster-file-name' 		: None,
			   'aeff' 					: 0.1,
			   'ephot' 					: 2.0,
			   'ior-re' 				: -1.920*pow(10.0,-4.0),
			   'ior-im' 				: 2.807*pow(10.0,-5.0),
			   'grid-width' 			: 8.0,
			   'ngrid' 					: 1024,
			   'nangle' 				: 1,
			   'euler-angle-mode' 		: 'file',
			   'euler-angle-file' 		: parent_dir+'/eul_angle_file.dat'
			   }
params_f1r2c1 = {'grain-geometry' 		: "sphere",
			   #'cluster-file-name' 		: None,
			   'aeff' 					: 0.2,
			   #'grain-axis-x' 			: 1.0,
			   #'grain-axis-y' 			: 1.0,
			   #'grain-axis-z' 			: 1.0,
			   'ephot' 					: 0.5,
			   'ior-re' 				: -2.079*pow(10.0,-3.0),
			   'ior-im' 				: 3.201*pow(10.0,-3.0),
			   'grid-width' 			: 8.0,
			   'ngrid' 					: 1024,
			   'nangle' 				: 1,
			   'euler-angle-mode' 		: 'random',
			   #'euler-angle-file' 		: parent_dir+'/eul_angle_file.dat'
			   }
params_f1r2c2 = { 'grain-geometry' 		: "sphere",
			   'cluster-file-name' 		: None,
			   'aeff' 					: 0.2,
			   'ephot' 					: 1.0,
			   'ior-re' 				: -7.152*pow(10.0,-4.0),
			   'ior-im' 				: 1.887*pow(10.0,-4.0),
			   'grid-width' 			: 8.0,
			   'ngrid' 					: 1024,
			   'nangle' 				: 1,
			   'euler-angle-mode' 		: 'file',
			   'euler-angle-file' 		: parent_dir+'/eul_angle_file.dat'
			  }
params_f1r2c3 = { 'grain-geometry' 		: "sphere",
			   'cluster-file-name' 		: None,
			   'aeff' 					: 0.2,
			   'ephot' 					: 2.0,
			   'ior-re' 				: -1.920*pow(10.0,-4.0),
			   'ior-im' 				: 2.807*pow(10.0,-5.0),
			   'grid-width' 			: 8.0,
			   'ngrid' 					: 1024,
			   'nangle' 				: 1,
			   'euler-angle-mode' 		: 'file',
			   'euler-angle-file' 		: parent_dir+'/eul_angle_file.dat'
			   }

#FIGURE 2 PARAMS
params_f2r1c1 = {'grain-geometry' 		: "ellipsoid",
			   #'cluster-file-name' 		: None,
			   'aeff' 					: 0.1,
			   'grain-axis-x' 			: ba,
			   'grain-axis-y' 			: 1.0,
			   'grain-axis-z' 			: 1.0,
			   'ephot' 					: 0.5,
			   'ior-re' 				: -2.079*pow(10.0,-3.0),
			   'ior-im' 				: 3.201*pow(10.0,-3.0),
			   'grid-width' 			: 8.0,
			   'ngrid' 					: 1024,
			   'nangle' 				: 1,
			   'euler-angle-mode' 		: 'file',
			   'euler-angle-file' 		: parent_dir+'/eul_angle_file.dat'
			   }
params_f2r1c2 = { 'grain-geometry' 		: "ellipsoid",
			   'cluster-file-name' 		: None,
			   'grain-axis-x' 			: ba,
			   'grain-axis-y' 			: 1.0,
			   'grain-axis-z' 			: 1.0,
			   'aeff' 					: 0.1,
			   'ephot' 					: 1.0,
			   'ior-re' 				: -7.152*pow(10.0,-4.0),
			   'ior-im' 				: 1.887*pow(10.0,-4.0),
			   'grid-width' 			: 8.0,
			   'ngrid' 					: 1024,
			   'nangle' 				: 1,
			   'euler-angle-mode' 		: 'file',
			   'euler-angle-file' 		: parent_dir+'/eul_angle_file.dat'
			  }
params_f2r1c3 = { 'grain-geometry' 		: "ellipsoid",
			   'cluster-file-name' 		: None,
			   'aeff' 					: 0.1,
			   'grain-axis-x' 			: ba,
			   'grain-axis-y' 			: 1.0,
			   'grain-axis-z' 			: 1.0,
			   'ephot' 					: 2.0,
			   'ior-re' 				: -1.920*pow(10.0,-4.0),
			   'ior-im' 				: 2.807*pow(10.0,-5.0),
			   'grid-width' 			: 8.0,
			   'ngrid' 					: 1024,
			   'nangle' 				: 1,
			   'euler-angle-mode' 		: 'file',
			   'euler-angle-file' 		: parent_dir+'/eul_angle_file.dat'
			   }
params_f2r2c1 = {'grain-geometry' 		: "ellipsoid",
			   #'cluster-file-name' 		: None,
			   'aeff' 					: 0.2,
			   'grain-axis-x' 			: ba,
			   'grain-axis-y' 			: 1.0,
			   'grain-axis-z' 			: 1.0,
			   #'grain-axis-x' 			: 1.0,
			   #'grain-axis-y' 			: 1.0,
			   #'grain-axis-z' 			: 1.0,
			   'ephot' 					: 0.5,
			   'ior-re' 				: -2.079*pow(10.0,-3.0),
			   'ior-im' 				: 3.201*pow(10.0,-3.0),
			   'grid-width' 			: 8.0,
			   'ngrid' 					: 1024,
			   'nangle' 				: 1,
			   'euler-angle-mode' 		: 'file',
			   'euler-angle-file' 		: parent_dir+'/eul_angle_file.dat'
			   }
params_f2r2c2 = { 'grain-geometry' 		: "ellipsoid",
			   'cluster-file-name' 		: None,
			   'aeff' 					: 0.2,
			   'grain-axis-x' 			: ba,
			   'grain-axis-y' 			: 1.0,
			   'grain-axis-z' 			: 1.0,
			   'ephot' 					: 1.0,
			   'ior-re' 				: -7.152*pow(10.0,-4.0),
			   'ior-im' 				: 1.887*pow(10.0,-4.0),
			   'grid-width' 			: 8.0,
			   'ngrid' 					: 1024,
			   'nangle' 				: 1,
			   'euler-angle-mode' 		: 'file',
			   'euler-angle-file' 		: parent_dir+'/eul_angle_file.dat'
			  }
params_f2r2c3 = { 'grain-geometry' 		: "ellipsoid",
			   'cluster-file-name' 		: None,
			   'aeff' 					: 0.2,
			   'grain-axis-x' 			: ba,
			   'grain-axis-y' 			: 1.0,
			   'grain-axis-z' 			: 1.0,
			   'ephot' 					: 2.0,
			   'ior-re' 				: -1.920*pow(10.0,-4.0),
			   'ior-im' 				: 2.807*pow(10.0,-5.0),
			   'grid-width' 			: 8.0,
			   'ngrid' 					: 1024,
			   'nangle' 				: 1,
			   'euler-angle-mode' 		: 'file',
			   'euler-angle-file' 		: parent_dir+'/eul_angle_file.dat'
			   }

f1 = plt.figure(1)
f1.suptitle("Figure 1 (Draine & Allaf-Akbari 2006): Spherical grains")
ax111 = f1.add_subplot(231)
ax112 = f1.add_subplot(232,sharex=ax111,sharey=ax111)
ax113 = f1.add_subplot(233,sharex=ax111,sharey=ax111)
ax121 = f1.add_subplot(234,sharex=ax111,sharey=ax111)
ax122 = f1.add_subplot(235,sharex=ax111,sharey=ax111)
ax123 = f1.add_subplot(236,sharex=ax111,sharey=ax111)
f1.subplots_adjust(hspace=0,wspace=0)

boundaries = [ xmin, xmax, ymin, ymax]


paramlists = [ params_f1r1c1, params_f1r1c2, params_f1r1c3, params_f1r2c1, params_f1r2c2, params_f1r2c3]
axes = [ ax111, ax112, ax113, ax121, ax122, ax123 ]
fnames = [ 'f1r1c1.dat', 'f1r1c2.dat', 'f1r1c3.dat', 'f1r2c1.dat', 'f1r2c2.dat','f1r2c3.dat']
for i in range(0,len(paramlists)):
	plt.locator_params(axis='x', nbins=4)
	fname =parent_dir+"/data/da06_"+fnames[i]
	ax = axes[i]
	ax.set_ylim(0.1,2*pow(10,5))
	params = paramlists[i]
	make_data(params, fname)
	data = pu.load_data(fname)
	if pu.all_zeros(data):
		print "Data set is all zeros :("
		sys.exit()
	data = pu.convert_to_arcseconds(data)
	data = pu.filter_data(data,boundaries=boundaries)
	func = pu.make_data_function(data)
	pu.add_1d_slice(ax,func,PlotMinMax=False,PlotOneSigma=False,PlotAvg=False,boundaries=boundaries,AddLegend=False,AddLabels=False,phis=[0.0],colors=['k'],linestyles=['-'])
	ax.set_xlim(0,xmax)
	if fnames[i] in ['f1r1c1.dat','f1r1c2.dat','f1r1c3.dat']: plt.setp(ax.get_xticklabels(), visible=False)
	else:
		plt.setp(ax.get_xticklabels(), visible=True)
		#ax.set_xticklabels([0,1000,2000,3000])
		ax.set_xlabel("$\\Theta$ [arcsec]")
	if fnames[i] in ['f1r1c1.dat', 'f1r2c1.dat']: 
		plt.setp(ax.get_xticklabels(), visible=True)
		ax.set_ylabel("$dQ_{sca}/d\\Omega$ [sr$^{-1}$]")
	else: plt.setp(ax.get_yticklabels(), visible=False)
	ax.text(0.2,0.9,"E=%.1f keV, a=%.2f $\\mu$m"%(params['ephot'],params['aeff']),horizontalalignment='left',transform=ax.transAxes,fontsize=9)

f1.savefig(data_dir+"/DA06_f1_spheres.png")

f2 = plt.figure(2)
f2.suptitle("Figure 2 (Draine & Allaf-Akbari 2006): Ellipsoidal grains")
ax211 = f2.add_subplot(231)
ax212 = f2.add_subplot(232,sharex=ax211,sharey=ax211)
ax213 = f2.add_subplot(233,sharex=ax211,sharey=ax211)
ax221 = f2.add_subplot(234,sharex=ax211,sharey=ax211)
ax222 = f2.add_subplot(235,sharex=ax211,sharey=ax211)
ax223 = f2.add_subplot(236,sharex=ax211,sharey=ax211)
f2.subplots_adjust(hspace=0,wspace=0)


paramlists = [ params_f2r1c1, params_f2r1c2, params_f2r1c3, params_f2r2c1, params_f2r2c2, params_f2r2c3]
axes = [ ax211, ax212, ax213, ax221, ax222, ax223 ]
fnames = [ 'f2r1c1.dat', 'f2r1c2.dat', 'f2r1c3.dat', 'f2r2c1.dat', 'f2r2c2.dat','f2r2c3.dat']
for i in range(0,len(paramlists)):
	plt.locator_params(axis='x', nbins=4)
	fname =parent_dir+"/data/da06_"+fnames[i]
	ax = axes[i]
	ax.set_ylim(0.1,2*pow(10,5))
	params = paramlists[i]
	make_data(params, fname)
	data = pu.load_data(fname)
	if pu.all_zeros(data):
		print "Data set is all zeros :("
		sys.exit()
	data = pu.convert_to_arcseconds(data)
	data = pu.filter_data(data,boundaries=boundaries)
	func = pu.make_data_function(data)
	pu.add_1d_slice(ax,func,PlotMinMax=False,PlotOneSigma=False,PlotAvg=False,boundaries=boundaries,AddLegend=False,AddLabels=False,phis=[0.0,np.pi/2.0],colors=['k'],linestyles=['-',':'])
	ax.set_xlim(0,xmax)
	if fnames[i] in ['f2r1c1.dat','f2r1c2.dat','f2r1c3.dat']: plt.setp(ax.get_xticklabels(), visible=False)
	else:
		plt.setp(ax.get_xticklabels(), visible=True)
		#ax.set_xticklabels([0,1000,2000,3000])
		ax.set_xlabel("$\\Theta$ [arcsec]")
	if fnames[i] in ['f2r1c1.dat', 'f2r2c1.dat']: 
		plt.setp(ax.get_xticklabels(), visible=True)
		ax.set_ylabel("$dQ_{sca}/d\\Omega$ [sr$^{-1}$]")
	else: plt.setp(ax.get_yticklabels(), visible=False)
	ax.text(0.2,0.9,"E=%.1f keV, a=%.2f $\\mu$m"%(params['ephot'],params['aeff']),horizontalalignment='left',transform=ax.transAxes,fontsize=9)

f2.savefig(data_dir+"/DA06_f2_ellipsoids.png")

#plt.show()
