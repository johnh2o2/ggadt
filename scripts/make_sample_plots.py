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

defaults = 		{
					'aeff' : 0.2,
					'ephot' : 2.0,
					'ior-re': -1.920E-4,
					'ior-im': 2.807E-5,
					#'nangle' : 1,
					'norientations' : 100,
					'ngrain'	: 256,
					'max-angle' : 3000.,
					'nscatter' 	: 100,
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
	ofile = data_dir+"/"+test+"-test.output"
	params = copy.deepcopy(defaults)
	footnote='$a_{eff}=%.2f$ $\\mu$m\n\n$E_{\\gamma}=%.2f~$ keV\nRe[$m-1$]=%.3e\nIm[$m-1$]=%.3e'%(params['aeff'],params['ephot'],params['ior-re'],params['ior-im'])

	if test == "sphere":
		params['grain-geometry'] = 'sphere'
		plot_title = 'Single sphere' 
	elif test == "ellipsoid":
		params['grain-geometry'] = 'ellipsoid'
		params['grain-axis-y'] = (1./sqrt(2))
		plot_title = 'Ellipsoid'
		footnote += "\nSpheroid $(a,b,c) = (%.3f, %.3f, %.3f)$\nnorientations=%d"%(params['grain-axis-x'],params['grain-axis-y'],params['grain-axis-z'],params['norientations'])
	elif test == "spheres":
		params['grain-geometry'] = 'spheres'
		plot_title = 'Spheres: '+spheres_file
		footnote += "\nCluster file: %s\nnorientations=%d"%(spheres_file,params['norientations'])
	footnote+="\nngrain=%d\nnscatter=%d"%(params['ngrain'],params['nscatter'])
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
	figname = parent_dir+"/doc/sample-"+test+".png"
	f = plt.figure()
	ax = f.add_subplot(111)
	subbox = [0.05,0.65,0.3,0.3]
	ax_sub = pu.add_subplot_axes(f,ax,subbox)
	pu.add_1d_slice(ax,func,PlotMinMax=False)
	pu.add_2d_heatmap(ax_sub,func,AddLabels=False,AddColorbar=False)
	ax_sub.set_xticklabels([])
	ax_sub.set_yticklabels([])
	ax.set_title(plot_title)
	ax.text(0.4,0.25,footnote,fontsize=10,horizontalalignment='left',verticalalignment='center',transform=ax.transAxes)
	f.savefig(figname)
	plt.show()
	print "  done."



	
