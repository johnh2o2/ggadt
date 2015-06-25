GGADT -- General Geometry Anomalous Diffraction Theory
======================================================
**Version 1.1.0**
*John Hoffman and Michael Tarczon*

README last updated: June 25, 2015

GGADT is a set of Fortran 95 routines that calculate the x-ray scattering and absorption properties of dust grains using anomalous diffraction theory (valid when the grain is large compared to the wavelength of incident light).

**[Full documentation](http://johnh2o2.github.io/ggadt)** (in browsable, HTML form) is available. Open the doc/htmldoc/index.html file in a browser of your choice.

See the **ChangeLog** for the gritty details of what's been changed in GGADT over time. This is just a spit-out of the current git log, and so probably contains more information than you need.

Quick install instructions
--------------------------

**Recommended reading**: 

* The HTML documentation for GGADT, available on-line [here](http://johnh2o2.github.io/ggadt)
	* As of June 8, 2015, this is a bit out of date. Documentation will be updated ASAP.
* the INSTALL file (which is a general-use file provided by GNU and may not be 100% applicable to GGADT)

Do the following from a terminal

1. Move to the parent directory of the GGADT installation (the directory in which this README file is located)
2. Do ```./configure --enable-openmp```
3. Do ```make```
4. This should compile GGADT (without errors). The GGADT binary is in the "src" folder.


How to run GGADT
----------------

GGADT can be run with parameters set by either command line arguments, or a parameter file, or both. Mixing them isn't recommended, and may prioritize conflicting parameter values in a way that you don't like. It's better to stick with one or the other.

**Running GGADT with a parameter file**

Running ```./ggadt``` without any command line arguments will cause GGADT to look for a file in the parent directory called ```default.params```. You can peruse this file to understand the basics of the parameter file structure.

If you would like to use your own parameter file, you can specify one from the command line via 

```./ggadt --parameter-file=<path/to/parameterfile>```

**Running GGADT with command line arguments**

GGADT can also be run by setting values on the command line:

```./ggadt --parameter1=value1 --parameter2=value2 --flag1 --flag2 ...```

For a list of available parameters and flags, do

```./ggadt -h```

And for more information about what these flags and parameters mean, consult the HTML help file.

GGADT output
------------

GGADT can calculate either the integrated cross sections (absorption, scattering, extinction) as a function of energy, or it can calculate the differential scattering cross section as a function of scattering angle. 

* To compute the **orientation-averaged differential scattering cross section**, you need not do anything, as this is the default calculation that GGADT will do. 
	* By default, GGADT will average dQsca/dOmega over random orientations, the number of which is specified by the ```norientations``` parameter.
	* If you wish to set your own custom orientations, you may do so in a separate file; for more information, consult the full documentation.
* To compute the **orientation-averaged integrated cross sections**, include the ```--integrated``` flag on the command line or set ```integrated = 'T'``` in a parameter file. 
* If you wish to calculate the **full dQ/dOmega(theta, phi) function** (i.e. relax the assumption of azimuthal symmetry), then you should set the ```--do-full-2d-fft``` flag.

Some features:
--------------
* **Self contained** package that depends only upon the existence of a reasonable C and Fortran 90/95 compiler; optionally works with external libraries like OpenMP and FFTW3.
* **Intuitive command-line input** (--parameter=value) and the option to create your own parameter files.
* **GNU Configure scripts** generated by using the *GNU Autoconf* suite of software -- this should provide decent portability and relatively painless installation across all *nix systems (including Mac OSX).
* **Multiple grain geometries supported** including spherical grains, ellipsoidal grains, and grains composed of an agglomeration of spherical particles 
* **Single and multiple-composition grains** (multiple-composition grains are only available for agglomerations of particles). Materials are specified by index files which specify the complex index of refraction and dielectric constant as a function of photon energy.
* **Ability to do integrated cross sections as well as differential-scattering cross section**

A word of caution: GGADT has **not been tested on Windows**. If you own a Windows machine and get GGADT to compile and run, let us know what you did and we'll try to relay that information. If you try and *fail* to get GGADT to run, and you'd like to use it on a Windows machine, let us know and we'll try to help.

TODO list for future updates:
-----------------------------

* Parallelize the fft-firstk functions
* Update the python scripts that you bundle with ggadt
* Update documentation
	* Add timing information
	* Add new options
	* Add detailed install instructions
* Fix the annoying aclocal/automake complaints that occur when you clone the git repo and try to do ./configure; make.
	* If you do make; make clean; make dist, you get a distro that installs on metis without a problem.
* Find out if there's a compelling reason for the disagreement between the minima of dQsca/dOmega for numerical vs analytical case of spherical scatterers.
	* It's not obvious to me that the minima can be predicted analytically. I don't know an intuitive reason for the numerical offsets...They do become smaller with higher grid resolution, and you're integrating over them anyway, so who cares?
* Wrap GGADT in python
* Add --nthreads option to arguments
* Add --use-wisdom option to arguments
* Add option to define custom list of energies for which to calculate σ(E)
