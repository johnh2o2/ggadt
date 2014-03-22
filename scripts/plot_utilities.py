from math import *
import sys
import os
import numpy as np 
from scipy.interpolate import RectBivariateSpline
import matplotlib.pyplot as plt
from matplotlib.colors import BoundaryNorm
from matplotlib.ticker import MaxNLocator
from installation_vars import *
from datetime import timedelta, datetime, tzinfo

data_dt = np.dtype([('theta', np.float_), ('phi', np.float_), ('f', np.float_)])
conv = (360*60*60)/(2*np.pi) # convert from radians to arcseconds (a more sensible unit)


def make_data(params,fname,echo=False, flags=[]):
	clargs = " "
	add = ""
	for p in params:
		clargs = clargs + " --"+p+"="+`params[p]`
	for f in flags:
		clargs = clargs + " --"+f
	command = ggadt+clargs+add+" > "+fname
	if echo: print command
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

def load_sed_data(fname):
	dt_sed = np.dtype([
			('ephot',np.float_),
			('sig_scat',np.float_),
			('sig_abs',np.float_)
			])
	return np.loadtxt(fname,dtype=dt_sed)

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

	theta_arr = np.linspace(xmin,xmax,100)
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
def get_cluster_header(filename):

    header = {
    	'MIGRATE' : None,
    	'ISEED' : None,
    	'NS' : None,
    	'VTOT' : None,
    	'alpha' : np.zeros(3),
    	'A_1' : np.zeros(3),
    	'A_2' : np.zeros(3),
    }
    Cfile = open(filename,'r')

    for i in range(0,4):
    	line = Cfile.readline()
    	words = line[:-1].split(' ')
    	while '' in words: words.remove('')
    	#print words
    	for j,w in enumerate(words):
    		
    		if w == 'MIGRATE=': header['MIGRATE'] = words[j+1]
    		elif w == 'ISEED=' : header['ISEED'] = words[j+1]
    		elif w == 'NS,':
    			header['NS'] = words[j-6]
    			header['VTOT'] = words[j-5]
    			header['alpha'][0] = words[j-4]
    			header['alpha'][1] = words[j-3]
    			header['alpha'][2] = words[j-2]
    		elif w == 'A_1':
    			header['A_1'][0] = words[j-4]
    			header['A_1'][1] = words[j-3]
    			header['A_1'][2] = words[j-2]
    		elif w == 'A_2':
    			header['A_2'][0] = words[j-4]
    			header['A_2'][1] = words[j-3]
    			header['A_2'][2] = words[j-2]
    		'''
    		if w == 'MIGRATE=': print 'MIGRATE: "',words[j+1],'"'
    		elif w == 'ISEED=' : print 'ISEED: "', words[j+1],'"'
    		elif w == 'NS,':
    			print 'NS: "', words[j-6],'"'
    			print 'VTOT: "', words[j-5],'"'
    			print 'alpha[0]: "',words[j-4],'"'
    			print 'alpha[1]: "',words[j-2],'"'
    			print 'alpha[2]: "',words[j-3],'"'
    			
    		elif w == 'A_1':
    			print 'A_1[0]: "',words[j-4],'"'
    			print 'A_1[1]: "',words[j-2],'"'
    			print 'A_1[2]: "',words[j-3],'"'
    		elif w == 'A_2':
    			print 'A_2[0]: "',words[j-4],'"'
    			print 'A_2[1]: "',words[j-2],'"'
    			print 'A_2[2]: "',words[j-3],'"'
    		'''
    Cfile.close()
    
    return header
def get_porosity(clusterfname):
	header = get_cluster_header(clusterfname)
	a = header['alpha']

	f = (a[0] + a[1] - a[2])
	f*= (a[0] + a[2] - a[1])
	f*= (a[1] + a[2] - a[0])

	f = 1.0/sqrt(f)

	return 1-f
def get_eq_ellipsoid(clusterfname):
	header = get_cluster_header(clusterfname)
	a = header['alpha']

	A = pow((a[0] + a[1] - a[2]),0.5)
	B = pow((a[0] + a[2] - a[1]),0.5)
	C = pow((a[1] + a[2] - a[0]),0.5)

	return A,B,C
def get_timestamp():
	class EST(tzinfo):
		def utcoffset(self, dt):
			return timedelta(hours=-5) + self.dst(dt)
		def dst(self, dt):
			# DST starts 2nd Sunday in March
			d = datetime(dt.year, 3, 11)   
			self.dston = d - timedelta(days=d.weekday() + 1)
			d = datetime(dt.year, 11, 4)
			self.dstoff = d - timedelta(days=d.weekday() + 1)
			if self.dston <=  dt.replace(tzinfo=None) < self.dstoff:
				return timedelta(hours=1)
			else:
				return timedelta(0)
		def tzname(self,dt):
			return "EST"

	time_now = datetime.now(tz=EST())
	timestamp = "%s (%s)"%(time_now.ctime(), time_now.tzname())
	return timestamp

def add_timestamp(f):
	f.text(0.04,0.01,get_timestamp(),va='bottom',ha='left',fontsize=10)

#def get_header_from_ggadt_output(filename):