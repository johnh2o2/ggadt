from math import *
import sys
import numpy as np 
import matplotlib.pyplot as plt
from matplotlib.colors import BoundaryNorm
from matplotlib.ticker import MaxNLocator
'''
NX = 100
NY = 100

func = lambda x,y : np.exp(-(np.power(x,2) + np.power(y,2))/2)

xmin = -3.0
xmax = 3.0
ymin = -3.0
ymax = 3.0
dx = (xmax - xmin)/(NX - 1)
dy = (ymax - ymin)/(NY - 1)
x = []
y = []
z = []

for i in range(0,NX):
	x_temp = xmin + i*dx
	for j in range(0,NY):
		y_temp = ymin + j*dy 
		x.append(x_temp)
		y.append(y_temp)
		z.append(func(x_temp, y_temp))
'''

fname = 'testdat2d.dat'
data_dt = np.dtype([('x', np.float_), ('y', np.float_), ('f', np.float_)])
data = np.loadtxt(fname,dtype=data_dt)

x = data['x']
y = data['y']
z = data['f']

ALL_ZERO = True 

for Z in z:
	if Z != 0:
		print "Not all zero :)"
		ALL_ZERO = False
		break

if ALL_ZERO:
	print "ALL ZERO!"
	sys.exit()

print "Span: ",z.min()," to ",z.max()
nx = 0
ny = 0

while x[ny] == x[0]: ny+=1

nx = len(x)/ny

dx = (x.max() - x.min())/(nx-1)
dy = (y.max() - y.min())/(ny-1)

x = np.reshape(x,(nx,ny))
y = np.reshape(y,(nx,ny))
z = np.reshape(z,(nx,ny))

levels = MaxNLocator(nbins=20).tick_values(z.min(), z.max())

# pick the desired colormap, sensible levels, and define a normalization
# instance which takes data values and translates those into levels.
cmap = plt.get_cmap('coolwarm')
norm = BoundaryNorm(levels, ncolors=cmap.N, clip=True)


plt.pcolormesh(x,y,z,cmap=cmap, norm=norm)
#plt.contourf(x[:-1, :-1] + dx / 2.,y[:-1, :-1] + dy / 2.,z,levels=levels, cmap=cmap)
plt.axis([x.min(), x.max(), y.min(), y.max()])
plt.colorbar()
plt.show()
