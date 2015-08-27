import matplotlib.pyplot as plt
import numpy as np
import argparse

parser=argparse.ArgumentParser(description="Plot 1d differential scattering cross section output from GGADT")
parser.add_argument('file_name',metavar='ggadt-output-file',type=str, nargs=1, help="a file containing the output of GGADT")

args = parser.parse_args()

file_name = args.file_name[0]

dscs = np.loadtxt(file_name, dtype=np.dtype([('theta', np.float_), ('dCdOm', np.float_)]))
dscs['theta'] *= 206264.806 # convert to arcseconds

# now plot!
f = plt.figure()
ax = f.add_subplot(111)

ax.plot(dscs['theta'], dscs['dCdOm'])
ax.set_xlabel("$\\theta$ [arcseconds]")
ax.set_ylabel("$dQ_{\\rm sca}/d\\Omega$ [sr$^{-1}$]")
ax.set_yscale('log')
plt.show(block=True)

