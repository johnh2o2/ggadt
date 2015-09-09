import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator
from matplotlib.colors import BoundaryNorm
from mpl_toolkits.axes_grid1 import make_axes_locatable
import numpy as np
import argparse

parser=argparse.ArgumentParser(description="Plot 2d differential scattering cross section output from GGADT")
parser.add_argument('file_name',metavar='ggadt-output-file',type=str, nargs=1, help="a file containing the output of GGADT")

args = parser.parse_args()

file_name = args.file_name[0]

vmin, vmax = -1, 5
ytitle=1.02

dscs = np.loadtxt(file_name, dtype=np.dtype([('thetax', np.float_), ('thetay', np.float_), ('dCdOm', np.float_)]))
dscs['thetax'] *= 206264.806 # convert to arcseconds
dscs['thetay'] *= 206264.806

colormap='jet'

n = 0
while dscs['thetax'][n] == dscs['thetax'][0]: n += 1
if n == 1:
	while dscs['thetay'][n] == dscs['thetay'][0]: n += 1

X = np.reshape(dscs['thetax'], (n,n))
Y = np.reshape(dscs['thetay'], (n,n))
Z = np.reshape(dscs['dCdOm'], (n,n))

clip = 1E-9
for i in range(len(Z)):
	for j in range(len(Z[i])):
		if Z[i][j] < clip: Z[i][j] = -100
		else: Z[i][j] = np.log10(Z[i][j])


# now plot!
f = plt.figure()
ax = f.add_subplot(111)

# Set up color plot
levels = MaxNLocator(nbins=40).tick_values(vmin,vmax)
cmap = plt.get_cmap(colormap)
norm = BoundaryNorm(levels, ncolors=cmap.N, clip=True)

# Make 2d plot of dQscat/dOmega
colorplot = ax.pcolormesh(X, Y, Z,cmap=cmap, vmin=vmin, vmax=vmax)#norm=norm)
for AX in [ ax.xaxis, ax.yaxis ]:
	AX.set_major_locator(plt.MultipleLocator(2000))
	AX.set_minor_locator(plt.MultipleLocator(500))
	
ax.set_ylabel("$\\theta_Y$ [arcseconds]")
ax.set_xlabel("$\\theta_X$ [arcseconds]")
ax.set_title("$\\log_{10}{dQ_{scat}/d\\Omega}$", y=ytitle)
ax.axis([X.min(),X.max(),X.min(),X.max()])
	 
yticks = np.arange(int(vmin), int(vmax)+1)
divider = make_axes_locatable(ax)
cax = divider.append_axes('right', size="5%", pad=0.05)
cbar = f.colorbar(colorplot, cax = cax, ticks=yticks)

ax.set_aspect('equal')

plt.show(block=True)

