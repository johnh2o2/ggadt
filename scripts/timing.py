#Compares timing results

from math import *
import numpy as np 
import matplotlib.pyplot as plt 
import os
import sys
import time
from installation_vars import *

REDO_TIMING = False

algorithms_to_compare = [ 'fftw3.patient', 'fftw3.measure', 'fftw3.estimate', 'gpfa' ]
as_a_function_of = [ 'ngrid' ]

fftw3_opt = " --enable fftw3"
config_opts = {		'fftw3.patient' : fftw3_opt,
				    'fftw3.measure' : fftw3_opt,
				    'fftw3.estimate' : fftw3_opt,
				    'gpfa' : ""
}


fftw3_algs = ['fftw3.patient', 'fftw3.measure', 'fftw3.estimate', 'fftw3']

max_var_val = 1024
arr = []
for i in range(1,20):
	for j in range(0,20):
		for k in range(0,20):
			n = int(pow(2,i)*pow(3,j)*pow(5,k))
			if n <= max_var_val: arr.append(n)

variables = 	{ 	'ngrid' : np.sort(arr) }
defaults = 		{	'--grid-width' : 8,
					'--aeff' : 0.2,
					'--ephot' : 1.0,
					'--ior-re': -2.079E-3,
					'--ior-im': 2.079E-3,
					'--nangle' : 50,
					'--grain-geometry' : 'spheres',
					'--euler-angle-mode' : 'random',
					'--cluster-file-name' : cluster_dir+'BAM2.256.1.targ',
					}

clargs = ""
for d in defaults:
	val = defaults[d]
	clargs = clargs+" "+d+"="+`val`

timeList = []

def timer():
	global timeList
	"""Print the time elapsed between the first
	   and second time this function is called."""
	timeList.append(time.time())
	if len(timeList)%2 == 0:
	    return timeList[-1] - timeList[-2]
	    timeList.pop()
	    timeList.pop()
	else:
		return None

times = {}
xvals = {}
scatters = {}
Nrealizations = 10

if REDO_TIMING:
	for algorithm in algorithms_to_compare:

		if algorithm == "fftw3.estimate":
			defaults['--fftw-optimization']='estimate'
		elif algorithm == "fftw3.patient":
			defaults['--fftw-optimization']='patient'
		elif algorithm == "fftw3.measure":
			defaults['--fftw-optimization']='measure'

		print "( "
		print "(   Going to time "+algorithm 
		print "( "
		#'''
		os.system("bash cleandist-custom.sh")
		os.system("aclocal -I./m4")
		os.system("autoconf")
		os.system("automake -a")
		if algorithm in fftw3_algs: 
			os.system("bash configure --enable-fftw3")
			extra = 1
		else: 
			os.system("bash configure")
			extra = 0
		os.system("make")
		#'''

		if not os.path.exists(ggadt):
			print "Cannot find ",ggadt
			sys.exit(0)
		for depvar in as_a_function_of:
			avg_times = []
			scatter = []
			print "|**"
			print "|**  Going to time "+depvar+" for "+algorithm+"."
			print "|**"
			fname = "timing."+algorithm+"."+depvar+".dat"
			os.system("rm -f "+fname)
			for val in variables[depvar]:

				total_times = []
				scatter_val = 0.0
				avg = 0.0
				for n in range(0,Nrealizations+extra):
					timer()

					command = ggadt+clargs+" --"+depvar+"="+`val`+" > /dev/null"
					print
					print `depvar`+"="+`val`+" : "+`n`#+" | "+`command`
					os.system(command)
					tt = timer()
					print "   took: "+`tt`+"s"
					if n == 0 and algorithm in fftw3_algs:
						print "   SKIPPING (this just made the plan...)"
						continue
					total_times.append(tt)
				avg = np.mean(total_times)
				for n in range(0,Nrealizations):
					scatter_val += pow(total_times[n] - avg,2)
				scatter_val = sqrt(scatter_val/(Nrealizations-1))
				avg_times.append(avg)
				scatter.append(scatter_val)
				print "|++++  "
				print "|++++  "+`depvar`+"="+`val` 
				print "|++++  time = "+`avg`+"s +/- "+`scatter_val`+"s"
				print "|++++  "
			print "|**"
			print "|**  OK -- Done with timing of "+depvar+" for "+algorithm+"."
			print "|**"

			times[fname] = np.array(avg_times)/defaults['--nangle']
			scatters[fname] = np.array(scatter)/defaults['--nangle']
			xvals[fname] = variables[depvar]
			f = open(fname,'w')
			for i in range(0,len(times[fname])):
				f.write("%e %e %e\n"%(xvals[fname][i],times[fname][i],scatters[fname][i]))
			f.close()
		print "( "
		print "(   Done with testing "+algorithm 
		print "( "
		print "----------------------------------------"

else:
	for algorithm in algorithms_to_compare:
		for depvar in as_a_function_of:
			fname = "timing."+algorithm+"."+depvar+".dat"
			data = np.loadtxt(fname)
			xvals[fname] = data[:,0]
			times[fname] = data[:,1]
			scatters[fname] = data[:,2]
			
		



print "Plotting now..."

for i,depvar in enumerate(as_a_function_of):
	f = plt.figure(i)
	ax = f.add_subplot(111)
	ax.set_xlabel(depvar)
	ax.set_ylabel("Time [s]")
	#ax.set_xlim(64, 2048)
	ax.set_yscale('log')
	for algorithm in algorithms_to_compare:
		fname = "timing."+algorithm+"."+depvar+".dat"
		xarr = xvals[fname]
		yarr = times[fname]
		sigarr = scatters[fname]

		#nlogn = np.multiply(xarr,np.log(xarr))
		#yarr = np.divide(times[fname], nlogn)
		#sigarr = np.divide(scatters[fname], nlogn)
		ax.errorbar(xarr,yarr,yerr=sigarr,label=algorithm)
	ax.legend(loc='best')


depvar = 'ngrid'
fname_gpfa = "timing.gpfa."+depvar+".dat"

f = plt.figure()
ax = f.add_subplot(111)
ax.set_xlabel(depvar)
ax.set_ylabel("Speed relative to GPFA")
#ax.set_yscale('log')
for algorithm in algorithms_to_compare:
	if algorithm == 'gpfa' : continue
	fname = "timing."+algorithm+"."+depvar+".dat"
	xarr = xvals[fname]
	#yarr = np.array([ 1.0/T for T in times[fname] ])
	#sigarr = np.array([ abs(1.0/(times[fname][i] - s) - 1.0/(times[fname][i] + s)) for i,s in enumerate(scatters[fname]) ])
	yarr = np.zeros(len(times[fname]))
	sigarr = np.zeros(len(times[fname]))
	for i in range(0,len(xvals[fname])):
		
		sa = scatters[fname_gpfa][i]
		a = times[fname_gpfa][i] 

		b = times[fname][i]
		sb = scatters[fname][i]

		yarr[i] = (a/b)
		sigarr[i] = yarr[i]*sqrt(pow(sa/a,2)+ pow(sb/b,2))
	#sigarr = scatters[fname]

	ax.errorbar(xarr,yarr,yerr=sigarr,label=algorithm)


ax.legend(loc='best')
ax.axhline(1.0,ls='--',color='k')



plt.show()
