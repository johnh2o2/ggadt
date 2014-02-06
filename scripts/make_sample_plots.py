from math import *
import sys
import copy
import os
import numpy as np 
from  installation_vars import *
import plot_utilities as pu
import matplotlib.pyplot as plt

def make_clargs(params):
	clargs = " "
	for p in params:
		clargs = clargs + " --"+p+"="+`params[p]`
	return clargs
	
spheres_file = 'BAM2.256.1.targ'

defaults = 		{	'grid-width' : 8,
					'aeff' : 0.2,
					'ephot' : 1.0,
					'ior-re': -2.079E-3,
					'ior-im': 2.079E-3,
					#'nangle' : 1,
					'nangle' : 100,
					'grain-axis-x' : 1.0,
					'grain-axis-y' : 1.0,
					'grain-axis-z' : 1.0,
					'grain-geometry' : 'spheres',
					#'euler-angle-mode' : 'file',
					'euler-angle-mode' : 'random',
					#'euler-angle-file' : parent_dir + "/eul_angle_file.dat",
					'cluster-file-name' : cluster_dir+'/'+spheres_file,
					}



tests = ["sphere", "spheres", "ellipsoid"]

for test in tests:
	print "Testing ",test 
	ofile = data_dir+"/test-"+test+".output"
	params = copy.deepcopy(defaults)
	if test == "sphere":
		params['grain-geometry'] = 'sphere'
		plot_title = 'Single sphere' 
		footnote='$a=%.2f$,$E_{\\gamma}=%.2f$, $m=1+(%.3e)+(%.3e)i$'%(params['aeff'],params['ephot'],params['ior-re'],params['ior-im'])
	elif test == "ellipsoid":
		params['grain-geometry'] = 'ellipsoid'
		params['grain-axis-y'] = (1./sqrt(2))
		plot_title = 'Ellipsoid'
		footnote='$a=%.2f$,$E_{\\gamma}=%.2f$, $m=1+(%.3e)+(%.3e)i$'%(params['aeff'],params['ephot'],params['ior-re'],params['ior-im'])
	elif test == "spheres":
		params['grain-geometry'] = 'spheres'
		plot_title = 'Spheres: '+spheres_file
		footnote='$a=%.2f$,$E_{\\gamma}=%.2f$, $m=1+(%.3e)+(%.3e)i$'%(params['aeff'],params['ephot'],params['ior-re'],params['ior-im'])

	clargs = make_clargs(params)
	command = ggadt+clargs+" > "+ofile
	#print command
	print "  running ggadt"
	os.system(command)
	print ""
	print "  plotting..."
	data = pu.load_data(ofile)
	if pu.all_zeros(data):
		print "Data set is all zeros :("
		sys.exit()
	data = pu.convert_to_arcseconds(data)
	data = pu.filter_data(data)
	func = pu.make_data_function(data)

	f = plt.figure()
	ax = f.add_subplot(111)
	subbox = [0.05,0.65,0.3,0.3]
	ax_sub = pu.add_subplot_axes(f,ax,subbox)
	pu.add_1d_slice(ax,func,PlotMinMax=False)
	pu.add_2d_heatmap(ax_sub,func,AddLabels=False,AddColorbar=False)
	ax_sub.set_xticklabels([])
	ax_sub.set_yticklabels([])
	ax.set_title(plot_title)

	plt.show()
	print "  done."



	
