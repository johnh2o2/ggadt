from math import *
import sys
import numpy as np 
from scipy.interpolate import RectBivariateSpline
import matplotlib.pyplot as plt
from matplotlib.colors import BoundaryNorm
from matplotlib.ticker import MaxNLocator

conv = (360*60*60)/(2*np.pi)
fname = 'testdat_thetaphi.dat'
data_dt = np.dtype([('theta', np.float_), ('phi', np.float_), ('f', np.float_)])
data = np.loadtxt(fname,dtype=data_dt)

x = data['theta']*conv
y = data['phi']*conv
z = data['f']

ALL_ZERO = True 

for Z in z:
	if Z != 0:
		#print "Not all zero :)"
		ALL_ZERO = False
		break

if ALL_ZERO:
	print "ALL ZERO!"
	sys.exit()

dx = x[1] - x[0]
dy = y[1] - y[0]

print "dx = %e; dy = %e"%(dx,dy)
delta = 500
xmin = -2000
xmax = 2000
ymin = xmin
ymax = xmax
'''
def smooth(x_old,y_old,z_old,old_dim,new_dim):
	print "smoothing from ",old_dim," to ",new_dim
	if old_dim <= new_dim : return x_old,y_old,z_old
	x_new = np.zeros(new_dim*new_dim)
	y_new = np.zeros(new_dim*new_dim)
	z_new = np.zeros(new_dim*new_dim)
	
	print old_dim,"=",num,"*",new_dim
	for i in range(0,new_dim):
		for j in range(0,new_dim):
			for k in range(0,num):
				x_new[i*new_dim+j] += x_old[(i*num + k)*old_dim + j*num]/num
				y_new[i*new_dim+j] += y_old[i*num*old_dim + j*num + k]/num 
	for i in range(0,new_dim):
		for j in range(0,new_dim):
			for k in range(0,num):
				for l in range(0,num):
					z_new[i*new_dim+j]+=z_old[(i*num+k)*old_dim+(j*num+l)]/(num*num)
	print "Done!"

	return x_new,y_new,z_new
'''

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
print sqrt(len(x)), sqrt(len(y)), sqrt(len(z))



n = 0

while x[n] == x[0]: n+=1


print int(sqrt(len(x))), n
#print x, y, z 


x = np.reshape(x,(n,n))
y = np.reshape(y,(n,n))
z = np.reshape(z,(n,n))


print "--Going to interpolate..."
BSPL = RectBivariateSpline(x[:,0],y[0,:],np.log10(z))
zinterp = lambda xi,yi : BSPL.ev(xi,yi)
print "  +Interpolated."



nplot = 500
zplot = np.zeros((nplot,nplot))
xplot,yplot = np.meshgrid(np.linspace(xmin,xmax,nplot),np.linspace(ymin,ymax,nplot))
for i in range(0,nplot):
	for j in range(0,nplot):
		zplot[i][j] = zinterp(xplot[i][j],yplot[i][j])

#xplot = x
#yplot = y
#zplot = np.log10(z)

levels = MaxNLocator(nbins=40).tick_values(zplot.min(), zplot.max())

# pick the desired colormap, sensible levels, and define a normalization
# instance which takes data values and translates those into levels.
cmap = plt.get_cmap('coolwarm')
norm = BoundaryNorm(levels, ncolors=cmap.N, clip=True)

f = plt.figure(1)
ax = f.add_subplot(111)
colorplot = ax.pcolormesh(xplot,yplot,zplot,cmap=cmap, norm=norm)
#plt.contourf(x[:-1, :-1] + dx / 2.,y[:-1, :-1] + dy / 2.,z,levels=levels, cmap=cmap)
ax.set_ylabel("$\\theta_Y$ [arcseconds]")
ax.set_xlabel("$\\theta_X$ [arcseconds]")
ax.axis([xplot.min(),xplot.max(),xplot.min(),xplot.max()])
plt.colorbar(colorplot)

print "--Plotted figure 1"
#plt.show()
#sys.exit()
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
		z_phi = pow(10,zinterp(th*cos(phi),th*sin(phi)))
		if i==0:
			z_max[j] = z_phi
			z_min[j] = z_phi

		if z_max[j] < z_phi: z_max[j] = z_phi
		if z_min[j] > z_phi: z_min[j] = z_phi

		z_phiavg[j]+=z_phi/len(phi_ints)
for i,phi in enumerate(phi_ints):
	for j, th in enumerate(theta_arr):
		z_phi = pow(10,zinterp(th*cos(phi),th*sin(phi)))
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
	z_plot = np.array([ pow(10,zinterp(th*cos(phi),th*sin(phi))) for th in theta_arr ])
	ax2.plot(theta_arr, z_plot , label="$\\phi=%.2f\\pi$"%(phi/np.pi), color=colors[i], ls=linestyles[i%len(linestyles)], lw=1.5)

ax2.set_ylabel("$dQ_{scat.}/d\\Omega$")
ax2.set_xlabel("$\\theta$ [arcseconds]")
ax2.set_yscale('log')
ax2.legend(loc='best')

plt.show()
