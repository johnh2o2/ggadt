import numpy as np
from math import *

max_gridsize= 10000
fname = ".allowed_ngrid_values"

n = max_gridsize/2

sizes = []
for i in range(n):
	q = 2**i
	if q > max_gridsize: break
	for j in range(n):
		p = 3**j
		if p*q > max_gridsize: break
		for k in range(n):
			r = 5**k
			if p*q*r > max_gridsize: break
			sizes.append(p*q*r)

sizes = sorted(sizes)

f = open(fname, 'w')
f.write("%d\n"%(len(sizes)))
for s in sizes:
	f.write("%d\n"%(s))

f.close()

