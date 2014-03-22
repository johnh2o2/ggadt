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

porosities = []

local_error = True
global_error = True
comparison = True


ForceRedo = False
Material = "index_silD03"
cluster = "BA.256.1"
cluster2 = "BAM2.512.1"
hold_sum_fi_equal_to_one = True
clusters = np.array([ 	
					["BA.64.1", "BA.256.1", "BA.512.1" ], 
					["BAM1.64.1", "BAM1.256.1", "BAM1.512.1"],
					["BAM2.64.1", "BAM2.256.1", "BAM2.512.1"] 
				])

def get_cluster_fname(c):
	return cluster_dir + "/"+c+".targ"
cluster_file_name = get_cluster_fname(cluster)
cluster_file_name2 = get_cluster_fname(cluster2)
P1 = pu.get_porosity(cluster_file_name)
P2 = pu.get_porosity(cluster_file_name2)

#porosities = [P1, P2 ]
porosities = np.linspace(0.0,0.9)

material_dir = data_dir + "/materials"
material_file_name = material_dir + "/" + Material


MaterialNames = { 	'index_CpeD03' :  "graphite, E perp to c axis, improved eps2 in X-ray", 
					'index_silD03' : "amorphous olivine-like astrosilicate" 
				}


material_name = MaterialNames[Material]
flags = [ 'sed' ]
def get_fname(params,c):
	return data_dir + "/exploring_fluffiness/ggadt_"+c+"_emin%.1f_emax%.1f_deph%.1e_ng%d_no%d_%s.dat"%(params['ephot-min'],params['ephot-max'],params['dephot'],params['ngrain'],params['norientations'],Material)


def conditional_make_data(fname,params,flags=[]):
	if ForceRedo or not os.path.exists(fname) or os.stat(fname).st_size == 0: pu.make_data(params,fname,flags=flags)


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
	'ngrain'			: 512,
	'norientations'		: 100
}

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

def solve_quad(a,b,c):
	s1 = (-b + comp_pow(b*b - 4*a*c,0.5))*comp_pow(2*a,-1)
	s2 = (-b - comp_pow(b*b - 4*a*c,0.5))*comp_pow(2*a,-1)
	return s1,s2



def get_eps_eff(f_vac,f_m,eps_m):

	A = -2*(f_vac + f_m)
	B = (2*f_m - f_vac)*eps_m + (2*f_vac - f_m)*eps_vac
	C = (f_vac + f_m)*eps_m*eps_vac

	s1,s2 = solve_quad(A,B,C)

	if s1.imag < 0 and s2.imag < 0:
		print "ERROR!"
		print s1, s2 
	elif s1.imag < 0: return s2
	elif s2.imag < 0: return s1
	else: 
		print "warning... s1, s2 = ", s1, s2
		return s1

def get_eps_eff_old(porosity,eps_m, fud=0.94):
	frac = 1 - fud*porosity
	eps_eff = eps_vac
	eps_eff*= 2*(1 - frac)*eps_vac + (1 + 2*frac)*eps_m
	eps_eff/= (2 + frac)*eps_vac + (1 - frac)*eps_m
	return eps_eff

def get_ior_eff_old(porosity,m,fud=0.94):
	eps_m = comp_pow(m,-2)
	return comp_pow(get_eps_eff_old(porosity,eps_m,fud=fud),-0.5)

def get_ior_eff(porosity,m,fud=0.94):
	eps_m = comp_pow(m,-2)
	return comp_pow(get_eps_eff(fud*porosity,1-porosity,eps_m),-0.5)

def q_emt(porosity,ephot,ior_re,ior_im, a_eff, fud=0.94):
	hc = 0.00124
	k = 2*np.pi*ephot/hc

	if hold_sum_fi_equal_to_one:
		m = get_ior_eff_old(porosity,complex(ior_re,ior_im),fud=fud)
	else:
		m = get_ior_eff(porosity,complex(ior_re,ior_im),fud=fud)
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





params['cluster-file-name'] = get_cluster_fname(cluster)
fname = get_fname(params,cluster)
conditional_make_data(fname, params, flags=flags)
ggadt_data = pu.load_sed_data(fname)
ephots = ggadt_data['ephot']


fudge = 0.94
if global_error:
	fvacs = np.linspace(0.1,0.99)
	err_abs = np.zeros((clusters.size,len(fvacs),len(ephots)))
	err_scat = np.zeros((clusters.size,len(fvacs),len(ephots)))
	err_ext = np.zeros((clusters.size,len(fvacs),len(ephots)))
	pors = np.zeros(clusters.size)

	for i,c in enumerate(np.ravel(clusters)):
		params['cluster-file-name'] = get_cluster_fname(c)
		fname = get_fname(params,c)
		conditional_make_data(fname, params, flags=flags)

		ggadt_data = pu.load_sed_data(fname)

		qscat_ggadt = ggadt_data['sig_scat']/(np.pi*pow(params['aeff'],2))
		qabs_ggadt = ggadt_data['sig_abs']/(np.pi*pow(params['aeff'],2))
		qext_ggadt = qscat_ggadt + qabs_ggadt

		p = pu.get_porosity(params['cluster-file-name'])
		pors[i] = p
		for j,F in enumerate(fvacs):
			for k,E in enumerate(ephots):
				qabs, qscat, qext = q_emt(p,E,mat_ior_re_func(E),mat_ior_im_func(E), params['aeff'],fud=(F/p) )

				 
				err_abs[i][j][k] = pow(np.log(qabs/qabs_ggadt[k]),2)
				err_scat[i][j][k] = pow(np.log(qscat/qscat_ggadt[k]),2)
				err_ext[i][j][k] = pow(np.log(qext/qext_ggadt[k]),2)
				

	

	num = 100*clusters.shape[0] + 10*clusters.shape[1]

	
	f_err = plt.figure()
	pu.add_timestamp(f_err)
	f_err.text(0.5, 0.04, '$f_{vac}$', ha='center', va='center')
	f_err.text(0.06, 0.5, '$\\left<\\log(Q_{EMT}/Q_{ADT})^2\\right>^{1/2}$', ha='center', va='center', rotation='vertical')
	#f_err.text(0.15,0.15,cluster,va='bottom',ha='left',fontsize=18)
	Axes = []

	for i,r in enumerate(clusters):
		for j,c in enumerate(r):
			ind = i*len(r) + j
			
			PLabs = [ sqrt(np.mean(err_abs[ind][k])).real for k in range(0,len(err_abs[0])) ]
			PLscat = [ sqrt(np.mean(err_scat[ind][k])).real for k in range(0,len(err_scat[0])) ]
			PLext = [ sqrt(np.mean(err_ext[ind][k])).real for k in range(0,len(err_ext[0])) ]
		
			Axes.append(f_err.add_subplot(num + ind + 1))
			
			Axes[-1].set_xlim(fvacs[0],fvacs[-1])
			Axes[-1].set_ylim(-0.4,2.2)

			Axes[-1].xaxis.set_major_locator(plt.MultipleLocator(0.2))
			Axes[-1].xaxis.set_minor_locator(plt.MultipleLocator(0.05))

			Axes[-1].yaxis.set_major_locator(plt.MultipleLocator(0.5))
			Axes[-1].yaxis.set_minor_locator(plt.MultipleLocator(0.1))

			Axes[-1].plot(fvacs,PLabs,color='k',ls='--')
			Axes[-1].plot(fvacs,PLscat,color='k',ls=':')
			Axes[-1].plot(fvacs,PLext,color='k',ls='-')
			if i != len(clusters) - 1: Axes[-1].set_xticklabels(())
			if j != 0: Axes[-1].set_yticklabels(())

			lab_ind = 4

			#Axes[-1].axvline(fudge*pors[ind],color='k',ls=':',lw=2,alpha=0.5)
			Axes[-1].plot([ fudge*pors[ind],  fudge*pors[ind]],[ 0.0, 1.5], color='r',ls='--')
			Axes[-1].text(fudge*pors[ind], 1.7, "$%.2f\\times P$"%(fudge), fontsize=10,va='top',ha='center')

			Axes[-1].text(fvacs[lab_ind], 0.95*PLabs[lab_ind], "abs", fontsize=10,va='top')
			Axes[-1].text(fvacs[lab_ind], 0.95*PLscat[lab_ind], "scat", fontsize=10,va='top')
			Axes[-1].text(fvacs[lab_ind], 0.95*PLext[lab_ind], "ext", fontsize=10,va='top')
			
			Axes[-1].text(0.05,0.05,c,va='bottom',ha='left',fontsize=12,transform=Axes[-1].transAxes)
			Axes[-1].text(0.95,0.05,"P=%.2f"%(pors[ind]),va='bottom',ha='right',fontsize=12,transform=Axes[-1].transAxes)
			#Axes[-1].legend(loc='best')
	f_err.subplots_adjust(hspace = 0, wspace=0, left=0.12, bottom=0.12)
	f_err.text(0.5,0.89,material_name+", $a_{eff}=%.3f$"%(params['aeff']),va='bottom',ha='center')
	f_err.savefig(parent_dir + "/emt_global_errors.png")
if comparison:
	

	qabs_emt = np.zeros((len(porosities),len(ephots)))
	qscat_emt = np.zeros((len(porosities),len(ephots)))
	qext_emt = np.zeros((len(porosities),len(ephots)))

	qscat_ggadt = ggadt_data['sig_scat']/(np.pi*pow(params['aeff'],2))
	qabs_ggadt = ggadt_data['sig_abs']/(np.pi*pow(params['aeff'],2))
	qext_ggadt = qscat_ggadt + qabs_ggadt

	for i,p in enumerate(porosities):
		for j,E in enumerate(ephots):
		
			qabs_emt[i][j], qscat_emt[i][j], qext_emt[i][j] = q_emt(p,E,mat_ior_re_func(E),mat_ior_im_func(E), params['aeff'],fud=fudge)
	f = plt.figure()
	pu.add_timestamp(f)
	ax = f.add_subplot(111)


	abss  = [ np.mean(qabs_emt[i]/qabs_emt[0] - 1) for i in range(0,len(porosities)) ]
	scats = [ np.mean(qscat_emt[i]/qscat_emt[0] - 1) for i in range(0,len(porosities)) ]
	exts  = [ np.mean(qext_emt[i]/qext_emt[0] - 1) for i in range(0,len(porosities)) ]

	np.save(data_dir + "/exploring_fluffiness/emt_results",(porosities,abss,scats,exts,fudge))
	ax.plot(porosities,scats,label="scat EMT", color='b',ls=':')
	ax.plot(porosities,abss,label="abs EMT", color='g',ls='--')
	ax.plot(porosities,exts,label="ext EMT", color='k',ls='-')
	ax.set_ylabel("$\\Delta \\sigma / \\sigma$")
	ax.set_xlabel("Porosity")
	ax.legend(loc='best')


if local_error:
	
	dabs = np.zeros((clusters.size,len(ephots)))
	dscat = np.zeros((clusters.size,len(ephots)))
	dext = np.zeros((clusters.size,len(ephots)))

	for i,c in enumerate(np.ravel(clusters)):
		params['cluster-file-name'] = get_cluster_fname(c)
		fname = get_fname(params,c)
		conditional_make_data(fname, params, flags=flags)

		ggadt_data = pu.load_sed_data(fname)

		qscat_ggadt = ggadt_data['sig_scat']/(np.pi*pow(params['aeff'],2))
		qabs_ggadt = ggadt_data['sig_abs']/(np.pi*pow(params['aeff'],2))
		qext_ggadt = qscat_ggadt + qabs_ggadt

		p = pu.get_porosity(params['cluster-file-name'])
		for j,E in enumerate(ephots):
			qabs, qscat, qext = q_emt(p,E,mat_ior_re_func(E),mat_ior_im_func(E), params['aeff'],fud=fudge )

			dabs[i][j] = qabs/qabs_ggadt[j] - 1
			dscat[i][j] = qscat/qscat_ggadt[j] - 1
			dext[i][j] = qext/qext_ggadt[j] - 1
			
				

	num = 100*clusters.shape[0] + 10*clusters.shape[1]

	
	f_err = plt.figure()
	pu.add_timestamp(f_err)
	f_err.text(0.5, 0.04, '$E (keV)$', ha='center', va='center')
	f_err.text(0.06, 0.5, '$Q_{EMT}/Q_{ADT}-1$', ha='center', va='center', rotation='vertical')
	#f_err.text(0.15,0.15,cluster,va='bottom',ha='left',fontsize=18)
	Axes = []

	for i,r in enumerate(clusters):
		for j,c in enumerate(r):
			ind = i*len(r) + j
			
			Axes.append(f_err.add_subplot(num + ind + 1))
			
			Axes[-1].set_xlim(ephots[0]+0.1,ephots[-1]-0.1)
			Axes[-1].set_ylim(-1.1,1.1)

			Axes[-1].yaxis.set_major_locator(plt.MultipleLocator(0.5))
			Axes[-1].yaxis.set_minor_locator(plt.MultipleLocator(0.1))

			Axes[-1].xaxis.set_major_locator(plt.MultipleLocator(1.0))
			Axes[-1].xaxis.set_minor_locator(plt.MultipleLocator(0.2))

			Axes[-1].plot(ephots,dabs[ind],color='k',ls='--')
			Axes[-1].plot(ephots,dscat[ind],color='k',ls=':')
			Axes[-1].plot(ephots,dext[ind],color='k',ls='-')
			if i != len(clusters) - 1: Axes[-1].set_xticklabels(())
			if j != 0: Axes[-1].set_yticklabels(())
			if i == 0 and j ==0 : 
				DY = 0.15
				DX = 0.1
				LW = 0.7
				Xi = 1.5
				Yi = 0.8
				offset = 0.05
				lss = [ '--', ':', '-' ]
				labs = [ "abs", "scat", "ext"]
			
				for n,l in enumerate(labs):
					Axes[-1].plot([ Xi, Xi + LW ],[ Yi - n*DY , Yi - n*DY ], color='k',ls=lss[n])
					Axes[-1].text(Xi + LW + DX, Yi - n*DY + offset, l , fontsize=10,va='center', ha='left')
				
			
			
			Axes[-1].text(0.05,0.05,c,va='bottom',ha='left',fontsize=12,transform=Axes[-1].transAxes)
			Axes[-1].text(0.95,0.05,"P=%.2f"%(pu.get_porosity(get_cluster_fname(c))),va='bottom',ha='right',fontsize=12,transform=Axes[-1].transAxes)
			#Axes[-1].legend(loc='best')
	f_err.subplots_adjust(hspace = 0, wspace=0, left=0.12, bottom=0.12)
	f_err.text(0.5,0.89,material_name+", $a_{eff}=%.3f$"%(params['aeff']),va='bottom',ha='center')
	f_err.savefig(parent_dir + "/emt_local_errors.png")

plt.show()


