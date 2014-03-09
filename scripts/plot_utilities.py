from math import *
import sys
import os
import numpy as np 
from scipy.interpolate import RectBivariateSpline
import matplotlib.pyplot as plt
from matplotlib.colors import BoundaryNorm
from matplotlib.ticker import MaxNLocator
from installation_vars import *


data_dt = np.dtype([('theta', np.float_), ('phi', np.float_), ('f', np.float_)])
conv = (360*60*60)/(2*np.pi) # convert from radians to arcseconds (a more sensible unit)


def make_data(params,fname,use_experimental=True):
	clargs = " "
	add = ""
	for p in params:
		clargs = clargs + " --"+p+"="+`params[p]`
	command = ggadt+clargs+add+" > "+fname
	print command
	os.system(command)

def quick_plots(fname,scale='log'):
	data = load_data(fname)
	data = filter_data(data)
	data = convert_to_arcseconds(data)
	func = make_data_function(data)

	f = plt.figure(1)
	ax = f.add_subplot(111)
	add_1d_slice(ax,func)


	f2 = plt.figure(2)
	ax2 = f2.add_subplot(111)
	add_2d_heatmap(ax2,func,scale=scale)

	plt.show()

def all_zeros(data):
	ALL_ZERO = True 
	for Z in data['f']:
		if Z != 0:
			ALL_ZERO = False
			break
	return ALL_ZERO

def load_data(fname):
	return np.loadtxt(fname,dtype=data_dt)

def filter_data(data,boundaries=[-2000,2000,-2000,2000],delta=100):
	xmin = boundaries[0]
	xmax = boundaries[1]
	ymin = boundaries[2]
	ymax = boundaries[3]

	x = data['theta']
	y = data['phi']
	z = data['f']

	# Now filter out data at high scattering angles (otherwise it takes too long to interpolate)
	x_new = []
	y_new = []
	z_new = []

	for i in range(0,len(x)):
		if x[i] >= (xmin-delta) and x[i] <= (xmax+delta) and y[i] >= (ymin-delta) and y[i] <= (ymax+delta):
			x_new.append(x[i])
			y_new.append(y[i])
			z_new.append(z[i])

	data_new = np.empty(len(x_new),dtype=data.dtype)
	data_new['theta'] = x_new
	data_new['phi'] = y_new
	data_new['f'] = z_new

	return data_new

def convert_to_arcseconds(data):
	data['theta']*=conv
	data['phi']*=conv 
	return data 

def make_data_function(data):
	x = data['theta']
	y = data['phi']
	z = data['f']
	n = 0
	while x[n] == x[0]: n+=1

	x = np.reshape(x,(n,n))
	y = np.reshape(y,(n,n))
	z = np.reshape(z,(n,n))

	# Interpolate
	BSPL = RectBivariateSpline(x[:,0],y[0,:],z)
	data_function = lambda xi,yi : BSPL.ev(xi,yi)

	return data_function

def add_1d_slice(ax,data_function,phis=[ 0.0, np.pi/2.0 ],boundaries=[-2000,2000,-2000,2000],PlotAvg=True,PlotMinMax=True,PlotOneSigma=True,AddLabels=True, AddLegend=True, colors = [ 'b', 'r', 'c', 'g' ],linestyles = [ '--', '-.', '..']):
	xmin = boundaries[0]
	xmax = boundaries[1]
	ymin = boundaries[2]
	ymax = boundaries[3]

	theta_arr = np.linspace(xmin,xmax,1000)
	z_phiavg = np.zeros(len(theta_arr))
	z_phivar = np.zeros(len(theta_arr))
	z_max = np.zeros(len(theta_arr))
	z_min = np.zeros(len(theta_arr))
	phi_ints = np.linspace(0,2*np.pi,100)


	for i,phi in enumerate(phi_ints):
		for j, th in enumerate(theta_arr):
			z_phi = data_function(th*cos(phi),th*sin(phi))
			if i==0:
				z_max[j] = z_phi
				z_min[j] = z_phi

			if z_max[j] < z_phi: z_max[j] = z_phi
			if z_min[j] > z_phi: z_min[j] = z_phi

			z_phiavg[j]+=z_phi/len(phi_ints)
	for i,phi in enumerate(phi_ints):
		for j, th in enumerate(theta_arr):
			z_phi = data_function(th*cos(phi),th*sin(phi))
			z_phivar[j]+=pow(z_phi-z_phiavg[j],2)/len(phi_ints)


	z_phivar = np.power(z_phivar,0.5)


	
	if PlotMinMax:
		ax.fill_between(theta_arr, z_min, z_max, facecolor='k',alpha=0.25, interpolate=True)
	if PlotOneSigma:
		ax.fill_between(theta_arr, z_phiavg-0.5*z_phivar, z_phiavg+0.5*z_phivar, facecolor='g', alpha=0.5, interpolate=True)
	if PlotAvg:
		ax.plot(theta_arr,z_phiavg,color='k',lw=2,label="Avg.")
	for i,phi in enumerate(phis):
		z_plot = np.array([ data_function(th*cos(phi),th*sin(phi)) for th in theta_arr ])
		ax.plot(theta_arr, z_plot , label="$\\phi=%.2f\\pi$"%(phi/np.pi), color=colors[i%len(colors)], ls=linestyles[i%len(linestyles)], lw=1.5)
	if AddLabels:
		ax.set_ylabel("$dQ_{scat.}/d\\Omega$")
		ax.set_xlabel("$\\theta$ [arcseconds]")
	ax.set_xlim(xmin,xmax)
	ax.set_yscale('log')
	if AddLegend:
		ax.legend(loc='best')
def get_1d_slice(data_function,phi=0.0,boundaries=[-2000,2000,-2000,2000]):
	xmin = boundaries[0]
	xmax = boundaries[1]

	theta_arr = np.linspace(xmin,xmax,1000)
	
	z_plot = np.zeros(len(theta_arr))
	for i in range(0,len(theta_arr)):
		z_plot[i] = data_function(theta_arr[i]*cos(phi),theta_arr[i]*sin(phi))

	return theta_arr,z_plot

def get_1d_avg(data_function,boundaries=[-2000,2000,-2000,2000]):
	xmin = boundaries[0]
	xmax = boundaries[1]

	theta_arr = np.linspace(xmin,xmax,1000)
	z_phiavg = np.zeros(len(theta_arr))
	z_phivar = np.zeros(len(theta_arr))
	z_max = np.zeros(len(theta_arr))
	z_min = np.zeros(len(theta_arr))
	phi_ints = np.linspace(0,2*np.pi,1000)


	for i,phi in enumerate(phi_ints):
		for j, th in enumerate(theta_arr):
			z_phi = data_function(th*cos(phi),th*sin(phi))
			if i==0:
				z_max[j] = z_phi
				z_min[j] = z_phi

			if z_max[j] < z_phi: z_max[j] = z_phi
			if z_min[j] > z_phi: z_min[j] = z_phi

			z_phiavg[j]+=z_phi/len(phi_ints)
	for i,phi in enumerate(phi_ints):
		for j, th in enumerate(theta_arr):
			z_phi = data_function(th*cos(phi),th*sin(phi))
			z_phivar[j]+=pow(z_phi-z_phiavg[j],2)/len(phi_ints)


	z_phivar = np.power(z_phivar,0.5)
	return theta_arr,z_phiavg,z_phivar
def add_2d_heatmap(ax,data_function,nplot=250,boundaries=[-2000,2000,-2000,2000],AddLabels=True,AddColorbar=True,scale='log'):
	xmin = boundaries[0]
	xmax = boundaries[1]
	ymin = boundaries[2]
	ymax = boundaries[3]
	# Interpolate original grid onto a high-density grid for plotting purposes.
	zplot = np.zeros((nplot,nplot))
	xplot,yplot = np.meshgrid(np.linspace(xmin,xmax,nplot),np.linspace(ymin,ymax,nplot))
	clip = pow(10,-12.0)
	for i in range(0,nplot):
		for j in range(0,nplot):
			zplot[i][j] = data_function(xplot[i][j],yplot[i][j])
			if zplot[i][j] < clip: zplot[i][j] = clip
			else: zplot[i][j] = log10(zplot[i][j])

	# Set up color plot
	levels = MaxNLocator(nbins=40).tick_values(zplot.min(), zplot.max())
	cmap = plt.get_cmap('coolwarm')
	norm = BoundaryNorm(levels, ncolors=cmap.N, clip=True)

	# Make 2d plot of dQscat/dOmega
	colorplot = ax.pcolormesh(xplot,yplot,zplot,cmap=cmap, norm=norm)
	if AddLabels:
		ax.set_ylabel("$\\theta_Y$ [arcseconds]")
		ax.set_xlabel("$\\theta_X$ [arcseconds]")
		if scale == 'log':
			ax.set_title("$\\log_{10}{dQ_{scat}/d\\Omega}$")
		else:
			ax.set_title("$dQ_{scat}/d\\Omega$")
	ax.axis([xplot.min(),xplot.max(),xplot.min(),xplot.max()])
	if AddColorbar: plt.colorbar(colorplot)

def add_subplot_axes(fig,ax,rect,axisbg='w'):
    box = ax.get_position()
    width = box.width
    height = box.height
    inax_position  = ax.transAxes.transform(rect[0:2])
    transFigure = fig.transFigure.inverted()
    infig_position = transFigure.transform(inax_position)    
    x = infig_position[0]
    y = infig_position[1]
    width *= rect[2]
    height *= rect[3]  # <= Typo was here
    subax = fig.add_axes([x,y,width,height],axisbg=axisbg)
    x_labelsize = subax.get_xticklabels()[0].get_size()
    y_labelsize = subax.get_yticklabels()[0].get_size()
    x_labelsize *= rect[2]**0.5
    y_labelsize *= rect[3]**0.5
    subax.xaxis.set_tick_params(labelsize=x_labelsize)
    subax.yaxis.set_tick_params(labelsize=y_labelsize)
    return subax

