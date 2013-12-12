import os
import sys
from math import *
import numpy as np 
import matplotlib.pyplot as plt
from scipy.interpolate import RectBivariateSpline
from matplotlib.colors import BoundaryNorm
from matplotlib.ticker import MaxNLocator


prog = "ggadt_v0.16"
N = 6
paramfile = "parameterfile.ini"
thmin = -1000
thmax = 1000
delta = 100
conv = (360*60*60)/(2*np.pi)

dt = np.dtype([('thetax',np.float_), ('thetay',np.float_), ('z', np.float_)])
avgs = []
variance = []
all_data = []
length = 0
answer_file = "data/testdat_ns1024_ng2048_na1000_bw64.dat"
answers = np.loadtxt(answer_file,dtype=dt)
n = int(sqrt(len(answers)))
print n
x = np.reshape(answers['thetax'],(n,n))
y = np.reshape(answers['thetay'],(n,n))
z = np.reshape(answers['z'],(n,n))

BSPL = RectBivariateSpline(x[:,0],y[0,:],np.log10(z))
answer = lambda xi,yi : pow(10,BSPL.ev(xi,yi))


os.system("make clean all")
for i in range(0,N):
	print "Running realization ",i+1," of ",N 
	dfile = "testdat.%d.dat"%(i)
	#os.system("./%s %s > %s"%(prog,paramfile,dfile))
	print "  -->ggadt has finished."
	sys.stdout.flush()
	data = np.loadtxt(dfile,dtype=dt)
	pruned_data = []
	for d in data:
		if ((d['thetax']*conv > thmin) and (d['thetax']*conv < thmax) and (d['thetay']*conv > thmin) and (d['thetay']*conv < thmax)):
			pruned_data.append(d)
	print "  -->pruned data to theta range [",thmin,", ",thmax,"]: ",len(pruned_data)," datapoints"
	sys.stdout.flush()
	all_data.append(pruned_data)
	if i == 0:
		length = len(pruned_data)
		avgs = np.zeros(len(pruned_data))
		variance = np.zeros(len(pruned_data))
	for j in range(0,len(pruned_data)):
		avgs[j]+=pruned_data[j]['z']/float(N)
	print "  -->recalculated averages"
	sys.stdout.flush()

error = np.zeros(len(avgs))


avg_err = 0.0
x = []
y = []
for i in range(0,len(avgs)):
	X=(all_data[0][i])['thetax']
	Y=(all_data[0][i])['thetay']
	x.append(X)
	y.append(Y)
	error[i] = pow((avgs[i] - answer(X,Y))/answer(X,Y),2)
	avg_err+=error[i]/float(len(error))

x = np.array(x)
y = np.array(y)
avg_err = sqrt(avg_err)


for i in range(0,length):
	for j in range(0,N):
		variance[i] += pow(all_data[j][i]['z'] - avgs[i],2)/float(N-1)

variance = np.sqrt(variance)

fracs = np.divide(variance,avgs)

# Figure out dimension and reshape x,y and z
n = 0
while x[n] == x[0]: n+=1

AVG = np.mean(fracs)
VAR = sqrt(np.mean(np.power(fracs - AVG,2)))
MAX = fracs.max()
MIN = fracs.min()

x = np.reshape(x,(n,n))
y = np.reshape(y,(n,n))
error = np.reshape(error,(n,n))
variance = np.reshape(variance,(n,n))
fracs = np.reshape(fracs,(n,n))

BSPL_Error = RectBivariateSpline(x[:,0],y[0,:],error)
error_interp = lambda xi,yi : BSPL_Error.ev(xi,yi)

BSPL_Variance = RectBivariateSpline(x[:,0],y[0,:],variance)
variance_interp = lambda xi,yi : BSPL_Variance.ev(xi,yi)

BSPL_Fracs = RectBivariateSpline(x[:,0],y[0,:],fracs)
fracs_interp = lambda xi,yi : BSPL_Fracs.ev(xi,yi)

Theta = np.linspace(thmin+delta,thmax-delta,n)
Phi = np.linspace(0.0,2.0*np.pi,100)

err_avg_R = np.zeros(len(Theta))
var_avg_R = np.zeros(len(Theta))
fracs_avg_R = np.zeros(len(Theta))

err_min = np.zeros(len(Theta))
var_min = np.zeros(len(Theta))
fracs_min = np.zeros(len(Theta))

err_max = np.zeros(len(Theta))
var_max = np.zeros(len(Theta))
fracs_max = np.zeros(len(Theta))

nplot = 200
zplot = np.zeros((nplot,nplot))
xplot,yplot = np.meshgrid(np.linspace(x.min(),x.max(),nplot),np.linspace(y.min(),y.max(),nplot))
for i in range(0,nplot):
	for j in range(0,nplot):
		zplot[i][j] = error_interp(xplot[i][j],yplot[i][j])

levels = MaxNLocator(nbins=40).tick_values(zplot.min(), zplot.max())
cmap = plt.get_cmap('coolwarm')
norm = BoundaryNorm(levels, ncolors=cmap.N, clip=True)

# Make 2d plot of dQscat/dOmega
f = plt.figure(1)
ax = f.add_subplot(111)
colorplot = ax.pcolormesh(xplot,yplot,zplot,cmap=cmap, norm=norm)
ax.set_ylabel("$\\theta_Y$ [arcseconds]")
ax.set_xlabel("$\\theta_X$ [arcseconds]")
ax.axis([xplot.min(),xplot.max(),yplot.min(),yplot.max()])
plt.colorbar(colorplot)
plt.show()
plt.clf()

nplot = 200
zplot = np.zeros((nplot,nplot))
xplot,yplot = np.meshgrid(np.linspace(x.min(),x.max(),nplot),np.linspace(y.min(),y.max(),nplot))
for i in range(0,nplot):
	for j in range(0,nplot):
		zplot[i][j] = variance_interp(xplot[i][j],yplot[i][j])

levels = MaxNLocator(nbins=40).tick_values(zplot.min(), zplot.max())
cmap = plt.get_cmap('coolwarm')
norm = BoundaryNorm(levels, ncolors=cmap.N, clip=True)

# Make 2d plot of dQscat/dOmega
f = plt.figure(1)
ax = f.add_subplot(111)
colorplot = ax.pcolormesh(xplot,yplot,zplot,cmap=cmap, norm=norm)
ax.set_ylabel("$\\theta_Y$ [arcseconds]")
ax.set_xlabel("$\\theta_X$ [arcseconds]")
ax.axis([xplot.min(),xplot.max(),yplot.min(),yplot.max()])
plt.colorbar(colorplot)
plt.show()
plt.clf()


nplot = 200
zplot = np.zeros((nplot,nplot))
xplot,yplot = np.meshgrid(np.linspace(x.min(),x.max(),nplot),np.linspace(y.min(),y.max(),nplot))
for i in range(0,nplot):
	for j in range(0,nplot):
		zplot[i][j] = fracs_interp(xplot[i][j],yplot[i][j])

levels = MaxNLocator(nbins=40).tick_values(zplot.min(), zplot.max())
cmap = plt.get_cmap('coolwarm')
norm = BoundaryNorm(levels, ncolors=cmap.N, clip=True)

# Make 2d plot of dQscat/dOmega
f = plt.figure(1)
ax = f.add_subplot(111)
colorplot = ax.pcolormesh(xplot,yplot,zplot,cmap=cmap, norm=norm)
ax.set_ylabel("$\\theta_Y$ [arcseconds]")
ax.set_xlabel("$\\theta_X$ [arcseconds]")
ax.axis([xplot.min(),xplot.max(),yplot.min(),yplot.max()])
plt.colorbar(colorplot)
plt.show()
plt.clf()



for phi in Phi:
	for j, th in enumerate(Theta):

		err_phi = error_interp(th*cos(phi),th*sin(phi))
		var_phi = variance_interp(th*cos(phi),th*sin(phi))
		fracs_phi =  fracs_interp(th*cos(phi),th*sin(phi))
		if i==0:
			err_max[j] = err_phi
			err_min[j] = err_phi

			var_max[j] = var_phi
			var_min[j] = var_phi

			fracs_max[j] = fracs_phi
			fracs_min[j] = fracs_phi

		if err_max[j] < err_phi: err_max[j] = err_phi
		if err_min[j] > err_phi: err_min[j] = err_phi

		if var_max[j] < var_phi: var_max[j] = var_phi
		if var_min[j] > var_phi: var_min[j] = var_phi

		if fracs_max[j] < fracs_phi: fracs_max[j] = fracs_phi
		if fracs_min[j] > fracs_phi: fracs_min[j] = fracs_phi

		err_avg_R[j]+=err_phi/len(Phi)
		var_avg_R[j]+=var_phi/len(Phi)
		fracs_avg_R[j]+=fracs_phi/len(Phi)


f = plt.figure(1)
ax_err = f.add_subplot(311)
ax_var = f.add_subplot(312)
ax_fracs = f.add_subplot(313)

#ax_fracs.fill_between(Theta, fracs_min, fracs_max, facecolor='k',alpha=0.25, interpolate=True)
ax_fracs.plot(Theta,fracs_avg_R,color='k',lw=2,label="$\\sigma/$avg")

#ax_var.fill_between(Theta, var_min, var_max, facecolor='k',alpha=0.25, interpolate=True)
ax_var.plot(Theta,var_avg_R,color='k',lw=2,label="$\\sigma$")

#ax_err.fill_between(Theta, err_min, err_max, facecolor='k',alpha=0.25, interpolate=True)
ax_err.plot(Theta,err_avg_R,color='k',lw=2,label="Error")

ax_fracs.legend(loc='best')
ax_var.legend(loc='best')
ax_err.legend(loc='best')

plt.show()

print "========================"



print "AVG",AVG
print "VAR",VAR
print "MAX",MAX 
print "MIN",MIN 
print "err",avg_err