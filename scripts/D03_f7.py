#Reproduces figures in Draine 2003a: 			

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

FORCE_REDO = False
plt_abs = True
plt_ext = True
plt_scat = True
rc('font',**{'family':'serif'})
## for Palatino and other serif fonts use:
#rc('font',**{'family':'serif','serif':['Palatino']})

material_file_name = "index_silD03"

material_file=data_dir + "/materials/"+material_file_name
material_dir = data_dir + "/materials"

material_names = { 	'index_CpeD03' :  "graphite, E perp to c axis, improved eps2 in X-ray", 
					'index_silD03' : "amorphous olivine-like astrosilicate" 
				}


material_name = material_names[material_file_name]



cluster = "BAM2.256.1"
cluster2 = "BA.256.1"


flags = [ 'sed' ]
params = {
	'material' 			: 'custom',
	'material-file' 	: material_file,
	'grain-geometry' 	: 'sphere',
	'aeff' 				: 0.2,
	'ephot-min'			: 0.1,
	'ephot-max'			: 7.3,
	'dephot'			: 0.0005,
	'cluster-file-name'	: cluster_dir + "/"+cluster+".targ",
	'grain-axis-x'		: sqrt(2),
	'grain-axis-y'		: 1.0,
	'grain-axis-z'		: 1.0,
	'ngrain'			: 64,
	'norientations'		: 50
}
output_file = pu.data_dir + "/D03_f7_"+material_file_name
if params['grain-geometry'] == 'sphere':
	output_file = output_file + "_sphere.dat"
	fig_title = material_name + ' (sphere, $a_{eff}=%.3f$)'%(params['aeff'])
elif params['grain-geometry'] == 'spheres':
	output_file = output_file + "_"+cluster+".dat"
	fig_title = material_name + '\n\n'+ cluster+', $a_{eff}=%.3f$, $N_g=%d$, $N_o=%d$'%(params['aeff'],params['ngrain'],params['norientations'])
else:
	output_file = output_file + "_ell_a%.3f_b%.3f_c%.3f.dat"%(params['grain-axis-x'],params['grain-axis-y'],params['grain-axis-z'])
	fig_title = material_name + '\n\n ellipsoid, $a_{eff}=%.3f$, $a/b=%.3f$, $N_g=%d$, $N_o=%d$'%(params['aeff'],params['grain-axis-x']/params['grain-axis-y'],params['ngrain'],params['norientations'])
if ((not os.path.exists(output_file)) or FORCE_REDO): pu.make_data(params,output_file,flags=flags)


spheres_output = pu.data_dir + "/D03_f7_"+material_file_name + "_"+cluster+".dat"
spheres_output2 = pu.data_dir + "/D03_f7_"+material_file_name + "_"+cluster2+".dat"

dt = np.dtype([('E',np.float_), ('sig_scat',np.float_),('sig_abs',np.float_)])


comp_data_name = spheres_output
comp_data_name2 = spheres_output2

#if FORCE_REDO or not os.path.exists(comp_data_name): 

data = np.loadtxt(output_file, dtype=dt)

comparison_data = np.loadtxt(comp_data_name,dtype=dt)

comparison_data2 = None#np.loadtxt(comp_data_name2,dtype=dt)


data['E'] *= 1000
scale = 1.0/(np.pi*pow(params['aeff'],2))
data['sig_abs']*=scale
data['sig_scat']*=scale

if comparison_data is not None:
	comparison_data['E']*=1000
	comparison_data['sig_abs']*=scale
	comparison_data['sig_scat']*=scale


if comparison_data2 is not None:
	comparison_data2['E']*=1000
	comparison_data2['sig_abs']*=scale
	comparison_data2['sig_scat']*=scale


sig_abs_func = interp1d(data['E'],data['sig_abs'])
sig_scat_func = interp1d(data['E'],data['sig_scat'])

xlab = 0.75
ylab = 0.25
props = dict(facecolor='none', edgecolor='k')
#props_line=dict(fc='w',edgecolor='r')



def set_axis(emin,emax,name,AXIS,lines=[],comp_data=None,comp_data2=None,comp_color='r',comp2_color = 'b'):
	
	AXIS.set_xlim(emin,emax)
	es = np.linspace(emin,emax,num=100)
	ymax = max([ sig_abs_func(en) + sig_scat_func(en) for en in es])

	AXIS.set_ylim(0,1.5*ymax)
	if plt_abs: AXIS.plot(data['E'],data['sig_abs'],ls='--',color='k')
	if plt_scat: AXIS.plot(data['E'],data['sig_scat'],ls='-',color='k')
	if plt_ext: AXIS.plot(data['E'],data['sig_scat']+data['sig_abs'], lw=2,color='k')
	
	if (comp_data != None):
		if plt_abs: AXIS.plot(comp_data['E'],comp_data['sig_abs'],ls='--',color=comp_color)
		if plt_scat: AXIS.plot(comp_data['E'],comp_data['sig_scat'],ls='-',color=comp_color)
		if plt_ext: AXIS.plot(comp_data['E'],comp_data['sig_scat']+comp_data['sig_abs'], lw=2,color=comp_color)
 	
 	if (comp_data2 != None):
		if plt_abs: AXIS.plot(comp_data['E'],comp_data2['sig_abs'],ls='--',color=comp2_color)
		if plt_scat: AXIS.plot(comp_data['E'],comp_data2['sig_scat'],ls='-',color=comp2_color)
		if plt_ext: AXIS.plot(comp_data['E'],comp_data2['sig_scat']+comp_data2['sig_abs'], lw=2,color=comp2_color)
 	
	width = emax - emin
	xtext = emin + 0.05*width

	if plt_abs: AXIS.text(xtext,1.1*sig_abs_func(xtext), "abs", fontsize=10,va='top')
	if plt_scat: AXIS.text(xtext,1.1*sig_scat_func(xtext), "scat", fontsize=10,va='bottom')
	if plt_ext: AXIS.text(xtext,1.1*sig_abs_func(xtext) + sig_scat_func(xtext), "ext",fontsize=10,va='bottom')

	AXIS.text(xlab,ylab,name,va='top',ha='left', transform=AXIS.transAxes, fontsize=20)
	for line in lines:
		AXIS.axvline(line,ls=':',color='k')
		AXIS.text(line+1,1.35*ymax,"%.1f"%(line),va='bottom',ha='left',fontsize=10)


f, ((ax11, ax12), (ax21, ax22), (junk,ax32)) = plt.subplots(3, 2)
#f.tight_layout()
axes = [ax11, ax12, ax21, ax22, ax32] #ax31]

# Si K edge
set_axis(1810.0,1890.0,"Si K",ax11,lines=[1845.6],comp_data=comparison_data,comp_data2=comparison_data2)

ax11.xaxis.set_major_locator(plt.MultipleLocator(20))
ax11.xaxis.set_minor_locator(plt.MultipleLocator(5))

#ax11.plot(comparison_data['E'],comparison_data['sig_abs'] + comparison_data['sig_scat'],color='r')
# Fe K edge
set_axis(7050.0,7150.0,"Fe K",ax12,lines=[7123.0],comp_data=comparison_data,comp_data2=comparison_data2)
ax12.xaxis.set_major_locator(plt.MultipleLocator(20))
ax12.xaxis.set_minor_locator(plt.MultipleLocator(5))
# Fe L edge
set_axis(695.0,735.0,"Fe L",ax21,lines=[707.8,720.6],comp_data=comparison_data,comp_data2=comparison_data2)
ax21.xaxis.set_major_locator(plt.MultipleLocator(20))
ax21.xaxis.set_minor_locator(plt.MultipleLocator(5))
# Mg K edge
set_axis(1295.0,1335.0,"Mg K",ax22,lines=[1310.6],comp_data=comparison_data,comp_data2=comparison_data2)
ax22.xaxis.set_major_locator(plt.MultipleLocator(20))
ax22.xaxis.set_minor_locator(plt.MultipleLocator(5))
# C K edge
'''
set_axis(275.0,315.0,"C K",ax31,lines=[285.4])
ax31.xaxis.set_major_locator(plt.MultipleLocator(20))
ax31.xaxis.set_minor_locator(plt.MultipleLocator(5))
'''
# O K edge
set_axis(515.0,555.0,"O K",ax32,lines=[537.6],comp_data=comparison_data,comp_data2=comparison_data2)
ax32.xaxis.set_major_locator(plt.MultipleLocator(20))
ax32.xaxis.set_minor_locator(plt.MultipleLocator(5))


f.text(0.5, 0.04, 'E (eV)', ha='center', va='center')
f.text(0.06, 0.5, '$Q = \\sigma/(\\pi a_{eff}^2)$', ha='center', va='center', rotation='vertical')
f.text(0.5,0.89,fig_title,ha='center', va='bottom')
figname = output_file
if comparison_data is not None:
	f.text(0.7,0.05,'Red: '+cluster,fontweight='bold',fontsize=10,color='r',ha='left', va='center')
	figname = figname + "_compared_to_"+cluster
if comparison_data2 is not None:
	f.text(0.7,0.025,'Blue: '+cluster2,fontweight='bold',fontsize=10,color='b',ha='left', va='center')
	figname = figname + "_and_"+cluster2
figname = figname + ".png"
f.savefig(figname)
plt.show()
