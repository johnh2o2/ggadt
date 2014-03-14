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


LOAD_OLD = True
LOAD_OLD_ANS = True
answer_file_name = data_dir + "/answer_spheres.dat"
cluster_file_name = cluster_dir + "/BAM2.256.1.targ"
max_angle1 = 1000.
max_angle2 = 2000.
max_angle3 = 3000.
boundaries=[-max_angle3,max_angle3,-max_angle3,max_angle3]
num1 = 0
num2 = 0
num3 = 0


answer_params = {
	'nscatter' : 100,
	'ngrain'   : 256,
	'norientations' : 1000,
	'cluster-file-name' : cluster_file_name,
	'grain-geometry' : 'spheres'
}
if LOAD_OLD_ANS:
	ans_theta,ans_avg,ans_var = np.load("errana_answer_norien.dat.npy")
else:
	pu.make_data(answer_params,answer_file_name)
	answer_data = pu.load_data(answer_file_name)
	answer_data = pu.convert_to_arcseconds(answer_data)
	ans_func = pu.make_data_function(answer_data)
	ans_theta,ans_avg,ans_var = pu.get_1d_avg(ans_func, boundaries=boundaries)
	np.save("errana_answer_norien.dat",(ans_theta,ans_avg,ans_var))

norientations=np.arange(1,200,10)

if LOAD_OLD: 
	diff1 = np.load("error_diff1_norien.dat.npy")
	diff2 = np.load("error_diff2_norien.dat.npy")
	diff3 = np.load("error_diff3_norien.dat.npy")
else:
	diff1 = np.zeros(len(norientations))
	diff2 = np.zeros(len(norientations))
	diff3 = np.zeros(len(norientations))
	for i in range(0,len(norientations)):
	
		#print nscatters[i],ngrains[j]
		print ""
		print i," out of ",len(norientations)
		pars = {
			'nscatter' : 100,
			'ngrain'   : 256,
			'norientations' : norientations[i],
			'cluster-file-name' : cluster_file_name,
			'grain-geometry' : 'spheres'
		}
		fname = data_dir + "/errana_norien.dat"
		pu.make_data(pars,fname)
		#print "loading..."
		dat = pu.load_data(fname)	
		#print "converting..."
		dat = pu.convert_to_arcseconds(dat)
		#print "making data func..."
		dat_func = pu.make_data_function(dat)
		#print "getting 1d avg..."
		theta,avg,var = pu.get_1d_avg(dat_func, boundaries=boundaries)
		if (len(avg) != len(ans_avg)):
			print "ERROR: len(avg) = ",len(avg)," but len(ans_avg) = ",len(ans_avg)
			sys.exit(0)
		for k in range(0,len(avg)):
			rms_val = pow((avg[k] - ans_avg[k])/ans_avg[k],2)
			if theta[k] < max_angle1:
				diff1[i] += rms_val
				num1+=1
			elif theta[k] < max_angle2:
				diff2[i] += rms_val
				num2+=1
			elif theta[k] < max_angle3:
				diff3[i] += rms_val
				num3+=1
		diff1[i]/=num1
		diff2[i]/=num2
		diff3[i]/=num3
		num1=0
		num2=0
		num3=0
	diff1 = np.sqrt(diff1)
	diff2 = np.sqrt(diff2)
	diff3 = np.sqrt(diff3)


	np.save("error_diff1_norien.dat",diff1)
	np.save("error_diff2_norien.dat",diff2)
	np.save("error_diff3_norien.dat",diff3)


f = plt.figure()
ax1 = f.add_subplot(131)
ax1.plot(norientations,diff1)

ax1.set_xlabel("Norientations")
#ax1.set_yscale('log')
#ax1.set_xscale('log')
ax1.set_title("RMS for $0<\\theta< %.1f$"%(max_angle1))


ax2 = f.add_subplot(132)
ax2.plot(norientations,diff2)

#ax2.set_yscale('log')
#ax2.set_xscale('log')
ax2.set_xlabel("Norientations")
ax2.set_title("RMS for $%.1f<\\theta< %.1f$"%(max_angle1,max_angle2))


ax3 = f.add_subplot(133)
ax3.plot(norientations,diff3)

#ax3.set_yscale('log')
#ax3.set_xscale('log')
ax3.set_xlabel("Norientations")
ax3.set_title("RMS for $%.1f<\\theta< %.1f$"%(max_angle2,max_angle3))
#plt.colorbar(diffplot)
plt.show()

