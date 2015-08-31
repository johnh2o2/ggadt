import matplotlib.pyplot as plt
import numpy as np
import argparse

parser=argparse.ArgumentParser(description="Plot 1d total extinction cross section output from GGADT")
parser.add_argument('file_name',metavar='ggadt-output-file',type=str, nargs=1, help="a file containing the output of GGADT")

args = parser.parse_args()

file_name = args.file_name[0]

data = np.loadtxt(file_name, dtype=np.dtype([('E', np.float_), ('qsca', np.float_), ('qabs', np.float_), ('qext', np.float_)]))

# now plot!
f = plt.figure()
ax = f.add_subplot(111)

ax.plot(data['E']*1000, data['qabs'], lw=1, color='r', label='abs')
ax.plot(data['E']*1000, data['qsca'], lw=1, color='b', label='sca')
ax.plot(data['E']*1000, data['qext'], lw=1.5, color='k', label='ext')
ax.set_yscale('log')
ax.set_xlabel("Energy [eV]")
ax.set_ylabel("$Q$")
ax.legend(loc='best')
plt.show(block=True)

