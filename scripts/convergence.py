import os
import sys
from math import *
import numpy as np 
import matplotlib.pyplot as plt
from scipy.interpolate import RectBivariateSpline


prog = "ggadt_v0.14"
N = 6
thmin = -1000
thmax = 1000
conv = (360*60*60)/(2*np.pi)

dt = np.dtype([('thetax',np.float_), ('thetay',np.float_), ('z', np.float_)])
avgs = []
variance = []
all_data = []
length = 0
answer_file = "testdat_ns1024_ng2048_na1000_bw64.dat"
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
	os.system("./%s > %s"%(prog,dfile))
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
for i in range(0,len(avgs)):
	X = (all_data[0][i])['thetax']
	Y = (all_data[0][i])['thetay']
	error[i] = pow((avgs[i] - answer(X,Y))/answer(X,Y),2)
	avg_err+=error[i]/float(len(error))

avg_err = sqrt(avg_err)


for i in range(0,length):
	for j in range(0,N):

		variance[i] += pow(all_data[j][i]['z'] - avgs[i],2)/float(N-1)

variance = np.sqrt(variance)

fracs = np.divide(variance,avgs)
print "========================"

AVG = np.mean(fracs)
VAR = sqrt(np.mean(np.power(fracs - AVG,2)))
MAX = fracs.max()
MIN = fracs.min()

print "AVG",AVG
print "VAR",VAR
print "MAX",MAX 
print "MIN",MIN 
print "err",avg_err