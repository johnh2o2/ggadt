#Usage: ipython plot.py <path to output file>
#outputs: (plots) which are saved as dQscat_dOmega_1d.png and dQscat_dOmega_2d.png.
#
# Plots:
# ======
#	dQscat_dOmega_2d -- plot of the full, head-on 2D scattering surface of the grain 
#						(averaged over nangle orientations)
#
#	dQscat_dOmega_1d -- Slices of the 2D scattering surface at phi=0 and phi=90 
#						

from math import *
import sys
import numpy as np 
from scipy.interpolate import RectBivariateSpline
import matplotlib.pyplot as plt
from matplotlib.colors import BoundaryNorm
from matplotlib.ticker import MaxNLocator
import plot_utilities as pu

LOGPLOT = True
verbose = False

nplot = 250 # dimension of plotted grid (interpolated)
delta = 250 # distance above or below theta min/max for interpolation purposes

conv = (360*60*60)/(2*np.pi) # convert from radians to arcseconds (a more sensible unit)

fname = sys.argv[1]
huge = pow(10,10)
tiny = pow(10,-10)

print "Plotting file '%s'"%(fname)
data_dt = np.dtype([('theta', np.float_), ('phi', np.float_), ('f', np.float_)])
data = np.loadtxt(fname,dtype=data_dt)

x = data['theta']*conv
y = data['phi']*conv
z = data['f']


#Tests if data file contains only zeros.



num_infs = 0
num_zeros = 0
th_max_data = 0.0
for i in range(0,len(z)):
	if np.isinf(z[i]):
		if i == 0:
			z[i] = 2*z[i+1] - z[i+2]
		elif i == len(z) - 1:
			z[i] = 2*z[i-1] - z[i-2]
		else:
			q = i+1
			while q < len(z) and np.isinf(z[q]):
				q += 1
			z[i] = pow(q - i,-1)*(z[i-1] + z[q])
		num_infs += 1
	elif abs(z[i]) < tiny:
		num_zeros += 1
		z[i] = tiny
	if x[i] > th_max_data:
		th_max_data = x[i]

max_angle = th_max_data
if verbose: print "max_angle = ",max_angle

# min/max scattering angles to deal with
xmin = -max_angle
xmax = max_angle
ymin = xmin
ymax = xmax

if num_zeros == len(z):
	print "ALL ZERO!"
	sys.exit()
if num_infs > 0: print num_infs," infinities found in the data. Replaced them with linearly interpolated values."


# Now filter out data at high scattering angles (otherwise it takes too long to interpolate)
x_new = []
y_new = []
z_new = []

for i in range(0,len(x)):
	if x[i] >= (xmin-delta) and x[i] <= (xmax+delta) and y[i] >= (ymin-delta) and y[i] <= (ymax+delta):
		x_new.append(x[i])
		y_new.append(y[i])
		z_new.append(z[i])

x = np.array(x_new)
y = np.array(y_new)
z = np.array(z_new)

# Figure out dimension and reshape x,y and z
n = 0
while x[n] == x[0]: n+=1
#print n
x = np.reshape(x,(n,n))
y = np.reshape(y,(n,n))
z = np.reshape(z,(n,n))

# Interpolate
if LOGPLOT:
	BSPL = RectBivariateSpline(x[:,0],y[0,:],np.log10(z))
else:
	BSPL = RectBivariateSpline(x[:,0],y[0,:],z)
zinterp = lambda xi,yi : BSPL.ev(xi,yi)


# Interpolate original grid onto a high-density grid for plotting purposes.
zplot = np.zeros((nplot,nplot))
xplot,yplot = np.meshgrid(np.linspace(xmin,xmax,nplot),np.linspace(ymin,ymax,nplot))
for i in range(0,nplot):
	for j in range(0,nplot):
		zplot[i][j] = zinterp(xplot[i][j],yplot[i][j])


# Set up color plot
levels = MaxNLocator(nbins=40).tick_values(zplot.min(), zplot.max())
cmap = plt.get_cmap('coolwarm')
norm = BoundaryNorm(levels, ncolors=cmap.N, clip=True)

# Make 2d plot of dQscat/dOmega
f = plt.figure(1)
ax = f.add_subplot(111)
colorplot = ax.pcolormesh(xplot,yplot,zplot,cmap=cmap, norm=norm)
ax.set_ylabel("$\\theta_Y$ [arcseconds]")
ax.set_xlabel("$\\theta_X$ [arcseconds]")
ax.axis([xplot.min(),xplot.max(),xplot.min(),xplot.max()])
plt.colorbar(colorplot)
f.savefig("dQscat_dOmega_2d.png")

# Plot (1) radial average of dQscat/dOmega, 
#      (2) variance about this average, 
#      (3) max/min deviations from this average
#      (4) & (5) slice of dQscat/dOmega through x-axis & y-axis
f2 = plt.figure(2)
ax2 = f2.add_subplot(111)
theta_arr = np.linspace(xmin,xmax,1000)
z_phiavg = np.zeros(len(theta_arr))
z_phivar = np.zeros(len(theta_arr))
z_max = np.zeros(len(theta_arr))
z_min = np.zeros(len(theta_arr))
phi_ints = np.linspace(0,2*np.pi,100)


for i,phi in enumerate(phi_ints):
	for j, th in enumerate(theta_arr):
		if LOGPLOT:
			z_phi = pow(10,zinterp(th*cos(phi),th*sin(phi)))
		else:
			z_phi = zinterp(th*cos(phi),th*sin(phi))
		if i==0:
			z_max[j] = z_phi
			z_min[j] = z_phi

		if z_max[j] < z_phi: z_max[j] = z_phi
		if z_min[j] > z_phi: z_min[j] = z_phi

		z_phiavg[j]+=z_phi/len(phi_ints)
for i,phi in enumerate(phi_ints):
	for j, th in enumerate(theta_arr):
		if LOGPLOT:
			z_phi = pow(10,zinterp(th*cos(phi),th*sin(phi)))
		else:
			z_phi = zinterp(th*cos(phi),th*sin(phi))
		z_phivar[j]+=pow(z_phi-z_phiavg[j],2)/len(phi_ints)


z_phivar = np.power(z_phivar,0.5)

phis = [ 0.0, np.pi/2.0 ]
colors = [ 'b', 'r', 'c', 'g' ]
linestyles = [ '--', '-.', '..']
ax2.fill_between(theta_arr, z_min, z_phiavg-0.5*z_phivar, facecolor='k',alpha=0.25, interpolate=True)
ax2.fill_between(theta_arr, z_phiavg-0.5*z_phivar, z_phiavg+0.5*z_phivar, facecolor='g', alpha=0.5, interpolate=True)
ax2.fill_between(theta_arr, z_phiavg+0.5*z_phivar,z_max, facecolor='k',alpha=0.25, interpolate=True)
ax2.plot(theta_arr,z_phiavg,color='k',lw=2,label="Avg.")
for i,phi in enumerate(phis):
	if LOGPLOT:
		z_plot = np.array([ pow(10,zinterp(th*cos(phi),th*sin(phi))) for th in theta_arr ])
	else:
		z_plot = np.array([ zinterp(th*cos(phi),th*sin(phi)) for th in theta_arr ])
	ax2.plot(theta_arr, z_plot , label="$\\phi=%.2f\\pi$"%(phi/np.pi), color=colors[i], ls=linestyles[i%len(linestyles)], lw=1.5)

ax2.set_ylabel("$dQ_{scat.}/d\\Omega$")
ax2.set_xlabel("$\\theta$ [arcseconds]")
ax2.set_xlim(xmin,xmax)
ax2.set_yscale('log')
ax2.legend(loc='best')

f2.savefig("dQscat_dOmega_1d.png")

plt.show()
