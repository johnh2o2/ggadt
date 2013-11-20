from math import *
import numpy as np 
import matplotlib.pyplot as plt

fname = 'testdat.dat'
data_dt = np.dtype([('theta', np.float_), ('dscatdomega_x', np.float_), ('dscatdomega_y', np.float_)])
data = np.loadtxt(fname,dtype=data_dt)

conv = (360*60*60)/(2*np.pi)

f = plt.figure()
ax = f.add_subplot(111)
ax.plot(conv*data['theta'],data['dscatdomega_x'], label="x-axis", color='b', ls='-.')
ax.plot(conv*data['theta'], data['dscatdomega_y'], label="y-axis", color='r', ls='--')
ax.set_ylabel("$d\\sigma_{scat.}/d\\Omega$")
ax.set_yscale('log')
ax.set_ylim(0.1,100000)
ax.set_xlim(0,5000)
#ax.set_xscale('log')
ax.set_xlabel("$\\theta$ [radians]")
ax.legend(loc='best')
plt.show()