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


LOAD_OLD = False
LOAD_OLD_ANS = True
answer_file_name = data_dir + "/answer_spheres.dat"
ngrains = []
nscatters = np.arange(10,150,10)
norientations = np.arange(1,100,10)
NGRAIN=512
NSCATTER=64
max_angle1 = 1000.
max_angle2 = 2000.
max_angle3 = 3000.
boundaries=[-max_angle3,max_angle3,-max_angle3,max_angle3]
num1 = 0
num2 = 0
num3 = 0
min_ngrain = 64
max_ngrain = 512

answer_params = {
	'nscatter' : 200,
	'ngrain'   : 1024,
	'norientations' : 500,
	'grain-geometry' : 'spheres',
	'cluster-file-name' : cluster_dir + "/BAM2.128.1.targ"
}
if LOAD_OLD_ANS:
	ans_theta,ans_avg,ans_var = np.load("errana_answer.dat.npy")
else:
	pu.make_data(answer_params,answer_file_name)
	answer_data = pu.load_data(answer_file_name)
	answer_data = pu.convert_to_arcseconds(answer_data)
	ans_func = pu.make_data_function(answer_data)
	ans_theta,ans_avg,ans_var = pu.get_1d_avg(ans_func, boundaries=boundaries)
	np.save("errana_answer.dat",(ans_theta,ans_avg,ans_var))


for i in range(0,12):
	for j in range(0,12):
		for k in range(0,12):
			val = int(pow(2,i)*pow(3,j)*pow(5,k))
			if val < max_ngrain and val > min_ngrain:
				ngrains.append(val)
ngrains = np.array(ngrains)
ngrains = np.sort(ngrains)


if LOAD_OLD: 
	diff_nor = np.load("error_diff_nor.dat.npy")
	
else:
	diff_nor = np.zeros(len(norientations))
	for i in range(0,len(norientations)):
		#print nscatters[i],ngrains[j]
		print ""
		print i," out of ",len(norientations)
		pars = {
			'nscatter' 	: NSCATTER,
			'ngrain'	: NGRAIN,
			'norientations' : norientations[i],
			'grain-geometry' : 'spheres',
			'cluster-file-name' : cluster_dir + "/BAM2.128.1.targ"
		}
		fname = data_dir + "/errana.dat"
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
			diff_nor[i] += pow((avg[k] - ans_avg[k])/ans_avg[k],2)
			
		diff_nor[i]/=len(avg)
	
	diff_nor = np.sqrt(diff_nor)


	np.save("error_diff_nor.dat",diff_nor)
	
'''

if LOAD_OLD: 
	diff1 = np.load("error_diff1.dat.npy")
	diff2 = np.load("error_diff2.dat.npy")
	diff3 = np.load("error_diff3.dat.npy")
else:
	diff1 = np.zeros((len(nscatters),len(ngrains)))
	diff2 = np.zeros((len(nscatters),len(ngrains)))
	diff3 = np.zeros((len(nscatters),len(ngrains)))
	for i in range(0,len(nscatters)):
		for j in range(0,len(ngrains)):
			#print nscatters[i],ngrains[j]
			print ""
			print i*len(ngrains) + j," out of ",len(nscatters)*len(ngrains)
			pars = {
				'nscatter' 	: nscatters[i],
				'ngrain'	: ngrains[j],
				'grain-geometry' : 'spheres',
				'cluster-file-name' : cluster_dir + "/BAM2.128.1.targ"
			}
			fname = data_dir + "/errana.dat"
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
					diff1[i][j] += rms_val
					num1+=1
				elif theta[k] < max_angle2:
					diff2[i][j] += rms_val
					num2+=1
				elif theta[k] < max_angle3:
					diff3[i][j] += rms_val
					num3+=1
			diff1[i][j]/=num1
			diff2[i][j]/=num2
			diff3[i][j]/=num3
			num1=0
			num2=0
			num3=0
	diff1 = np.sqrt(diff1)
	diff2 = np.sqrt(diff2)
	diff3 = np.sqrt(diff3)


	np.save("error_diff1.dat",diff1)
	np.save("error_diff2.dat",diff2)
	np.save("error_diff3.dat",diff3)

dthetas = np.array([ 2.*max_angle3/float(n) for n in nscatters])
yarr = nscatters
ylab = "nscatter"
X,Y = np.meshgrid(ngrains,yarr)

diff_tot = np.zeros((len(nscatters),len(ngrains)))

for i in range(0,len(nscatters)):
	for j in range(0,len(ngrains)):
		diff_tot[i][j] = sqrt( pow(diff1[i][j],2) + pow(diff2[i][j],2) + pow(diff3[i][j],2))/sqrt(3)

f = plt.figure()
ax = f.add_subplot(111)

ax.set_xlabel("ngrain")
ax.set_ylabel(ylab)


levels = MaxNLocator(nbins=40).tick_values(diff_tot.min(), diff_tot.max())
cmap = plt.get_cmap('coolwarm')
norm = BoundaryNorm(levels, ncolors=cmap.N, clip=True)

colorplot = ax.pcolormesh(X,Y,diff_tot,cmap=cmap, norm=norm)
plt.colorbar(colorplot)
ax.axis([X.min(),X.max(),Y.min(),Y.max()])
#diffplot = ax.contour(X,Y,np.log10(diff_tot), [-1,-2])
#plt.clabel(diffplot,fontsize=10)

'''
f = plt.figure()
ax = f.add_subplot(111)
ax.set_xlabel("norientations")
ax.set_ylabel("RMS")
ax.plot(norientations,diff_nor)


'''
f, (ax1, ax2, ax3) = plt.subplots(1,3, sharex=True, sharey=True)
ax1 = f.add_subplot(131)
ax1.set_xlim(ngrains[0],ngrains[-1])
ax1.set_ylim(150,dthetas[-1])
#ax.set_yscale('log')
#ax.set_xscale('log')
#diffplot1 = ax1.contour(X,Y,diff1,[0.0001,0.001,0.01,0.02,0.05,0.07,0.1,0.2,0.3])
diffplot1 = ax1.contour(X,Y,diff1)
plt.clabel(diffplot1,fontsize=10)
ax1.set_xlabel("N grain")
ax1.set_ylabel("$\\Delta\\theta$ (arcseconds)")
ax1.set_title("$0<\\theta< %.1f$"%(max_angle1))



ax2.set_xlim(ngrains[0],ngrains[-1])
ax2.set_ylim(150,dthetas[-1])
#ax.set_yscale('log')
#ax.set_xscale('log')
#diffplot2 = ax2.contour(X,Y,diff2,[0.0001,0.001,0.01,0.02,0.05,0.07,0.1,0.2,0.3])
diffplot2 = ax2.contour(X,Y,diff2)
plt.clabel(diffplot2,fontsize=10)
ax2.set_xlabel("N grain")
ax2.set_title("$%.1f<\\theta< %.1f$"%(max_angle1,max_angle2))



ax3.set_xlim(ngrains[0],ngrains[-1])
ax3.set_ylim(150,dthetas[-1])
#ax.set_yscale('log')
#ax.set_xscale('log')
#diffplot3 = ax3.contour(X,Y,diff3,[0.0001,0.001,0.01,0.02,0.05,0.07,0.1,0.2,0.3])
diffplot3 = ax3.contour(X,Y,diff3)
plt.clabel(diffplot3,fontsize=10)
ax3.set_xlabel("N grain")

ax3.set_title("$%.1f<\\theta< %.1f$"%(max_angle2,max_angle3))
'''
#plt.colorbar(diffplot)

#f.subplots_adjust(vspace=0)
#plt.setp([a.get_yticklabels() for a in [ax2,ax3]], visible=False)
plt.show()

