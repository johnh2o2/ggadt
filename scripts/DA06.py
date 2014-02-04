#Reproduces figures in DA06			

from math import *
import sys
import os
import numpy as np 
from scipy.interpolate import RectBivariateSpline
import matplotlib.pyplot as plt
from matplotlib.colors import BoundaryNorm
from matplotlib.ticker import MaxNLocator

parent_dir = "/Users/jah5/Desktop/Draine_temp/GGADT_JohnsMac/GGADT"

binary = parent_dir+"/src/ggadt"

nplot = 250 # dimension of plotted grid (interpolated)
delta = 250 # distance above or below theta min/max for interpolation purposes

# min/max scattering angles to deal with
xmin = -4000
xmax = 4000
ymin = xmin
ymax = xmax

conv = (360*60*60)/(2*np.pi) # convert from radians to arcseconds (a more sensible unit)

def add_plot(fname, ax, mode='one-d', phis = [ 0.0, np.pi/2.0 ]):

	print "Plotting file '%s'"%(fname)
	data_dt = np.dtype([('theta', np.float_), ('phi', np.float_), ('f', np.float_)])
	data = np.loadtxt(fname,dtype=data_dt)

	x = data['theta']*conv
	y = data['phi']*conv
	z = data['f']

	'''
	#Tests if data file contains only zeros.
	ALL_ZERO = True 
	for Z in z:
		if Z != 0:
			#print "Not all zero :)"
			ALL_ZERO = False
			break

	if ALL_ZERO:
		print "ALL ZERO!"
		sys.exit()
	'''

	# Now filter out data at high scattering angles (otherwise it takes too long to interpolate)
	x_new = []
	y_new = []
	z_new = []

	npoints_inbounds = 0
	for i in range(0,len(x)):
		if x[i] >= (xmin-delta) and x[i] <= (xmax+delta) and y[i] >= (ymin-delta) and y[i] <= (ymax+delta):
			x_new.append(x[i])
			y_new.append(y[i])
			z_new.append(z[i])
			npoints_inbounds+=1
	if npoints_inbounds > 0: npoints_inbounds = sqrt(npoints_inbounds)
	print npoints_inbounds, " points are within boundary per dimension"
	x = np.array(x_new)
	y = np.array(y_new)
	z = np.array(z_new)

	# Figure out dimension and reshape x,y and z
	n = 0
	while x[n] == x[0]: n+=1

	x = np.reshape(x,(n,n))
	y = np.reshape(y,(n,n))
	z = np.reshape(z,(n,n))

	# Interpolate
	BSPL = RectBivariateSpline(x[:,0],y[0,:],np.log10(z))
	zinterp = lambda xi,yi : BSPL.ev(xi,yi)


	# Interpolate original grid onto a high-density grid for plotting purposes.
	zplot = np.zeros((nplot,nplot))
	xplot,yplot = np.meshgrid(np.linspace(xmin,xmax,nplot),np.linspace(ymin,ymax,nplot))
	for i in range(0,nplot):
		for j in range(0,nplot):
			zplot[i][j] = zinterp(xplot[i][j],yplot[i][j])

	if mode == "two-d":
		# Set up color plot
		levels = MaxNLocator(nbins=40).tick_values(zplot.min(), zplot.max())
		cmap = plt.get_cmap('coolwarm')
		norm = BoundaryNorm(levels, ncolors=cmap.N, clip=True)

		# Make 2d plot of dQscat/dOmega
		
		colorplot = ax.pcolormesh(xplot,yplot,zplot,cmap=cmap, norm=norm)
		ax.set_ylabel("$\\theta_Y$ [arcseconds]")
		ax.set_xlabel("$\\theta_X$ [arcseconds]")
		ax.axis([xplot.min(),xplot.max(),xplot.min(),xplot.max()])
		
	elif mode == "one-d":
		# Plot (1) radial average of dQscat/dOmega, 
		#      (2) variance about this average, 
		#      (3) max/min deviations from this average
		#      (4) & (5) slice of dQscat/dOmega through x-axis & y-axis
		
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

		
		colors = [ 'b', 'r', 'c', 'g' ]
		linestyles = [ '--', '-.', '..']
		#ax.fill_between(theta_arr, z_min, z_phiavg-0.5*z_phivar, facecolor='k',alpha=0.25, interpolate=True)
		#ax.fill_between(theta_arr, z_phiavg-0.5*z_phivar, z_phiavg+0.5*z_phivar, facecolor='g', alpha=0.5, interpolate=True)
		#ax.fill_between(theta_arr, z_phiavg+0.5*z_phivar,z_max, facecolor='k',alpha=0.25, interpolate=True)
		#ax.plot(theta_arr,z_phiavg,color='k',lw=2,label="Avg.")
		for i,phi in enumerate(phis):
			z_plot = np.array([ pow(10,zinterp(th*cos(phi),th*sin(phi))) for th in theta_arr ])
			ax.plot(theta_arr, z_plot , label="$\\phi=%.2f\\pi$"%(phi/np.pi), color=colors[i], ls=linestyles[i%len(linestyles)], lw=1.5)

		ax.set_ylabel("$dQ_{scat.}/d\\Omega$")
		ax.set_xlabel("$\\theta$ [arcseconds]")
		ax.set_xlim(0,4000)
		ax.set_yscale('log')
		ax.legend(loc='best')
	else:
		print "I don't understand mode" + mode


def make_data(params,fname):
	clargs = " -d "
	for p in params:
		clargs = clargs + " --"+p+"="+`params[p]`

	command = binary+clargs+" > "+fname
	print command
	os.system(command)

ba = 0.707106781


params_r1c1 = {'grain-geometry' 		: "sphere",
			   #'cluster-file-name' 		: None,
			   'aeff' 					: 0.1,
			   #'grain-axis-x' 			: 1.0,
			   #'grain-axis-y' 			: 1.0,
			   #'grain-axis-z' 			: 1.0,
			   'ephot' 					: 0.5,
			   'RE-index-of-refraction' : -2.079*pow(10.0,-3.0),
			   'IM-index-of-refraction' : 3.201*pow(10.0,-3.0),
			   'grid-width' 			: 64.0,
			   'ngrid' 					: 1024,
			   'nangle' 				: 1,
			   'euler-angle-mode' 		: 'random',
			   #'euler-angle-file' 		: parent_dir+'/eul_angle_file.dat'
			   }
params_r1c2 = { 'grain-geometry' 		: "sphere",
			   'cluster-file-name' 		: None,
			   'aeff' 					: 0.2,
			   'grain-axis-x' 			: 0.8,
			   'grain-axis-y' 			: 1.0,
			   'grain-axis-z' 			: 1.0,
			   'ephot' 					: 2.0,
			   'RE-index-of-refraction' : -2.079*pow(10.0,-4.0),
			   'IM-index-of-refraction' : 3.201*pow(10.0,-3.0),
			   'grid-width' 			: 8.0,
			   'ngrid' 					: 1024,
			   'nangle' 				: 1,
			   'euler-angle-mode' 		: 'file',
			   'euler-angle-file' 		: parent_dir+'/eul_angle_file.dat'
			   }

f1 = plt.figure(1)
ax1 = f1.add_subplot(111)

params = params_r1c1
fname =parent_dir+"/data/output_r1c1.dat"

make_data(params, fname)
add_plot(fname,ax1,mode='one-d',phis=[ 0.0 ] )
ax1.set_title("E=%.1f keV, a=%.2f $\\mu$m"%(params['ephot'],params['aeff']))


plt.show()
