GGADT -- General Geometry Anomalous Diffraction Theory
========
**Version 0.9.90**
*John Hoffman and Michael Tarczon*

README last updated: March 27, 2014

GGADT is a set of Fortran 95 routines that calculate the x-ray scattering and absorption properties of dust grains using anomalous diffraction theory (valid when the grain is large compared to the wavelength of incident light).

**Full documentation** (in browsable, HTML form) is available. Open the doc/htmldoc/index.html file in a browser of your choice.

See the **ChangeLog** for the gritty details of what's been changed in GGADT over time. This is just a spit-out of the current git log, and so probably contains more information than you need.

Quick install instructions
--------------------------

**Recommended reading**: 

* The HTML documentation for GGADT
* the INSTALL file (which is a general-use file provided by GNU and may not be 100% applicable to GGADT)

Do the following from a terminal

1. Move to the parent directory of the GGADT installation (the directory in which this README file is located)
2. Do ```./configure --enable-openmp```
3. Do ```make```
4. This should compile GGADT (without errors). The GGADT binary is in the "src" folder.


Some features:
--------------
* **Self contained** package that depends only upon the existence of a reasonable C and Fortran 90/95 compiler; optionally works with external libraries like OpenMP and FFTW3.
* **Intuitive command-line input** (--parameter=value) and the option to create your own parameter files.
* **GNU Configure scripts** generated by using the *GNU Autoconf* suite of software -- this should provide decent portability and relatively painless installation across all *nix systems (including Mac OSX).
* **Multiple grain geometries supported** including spherical grains, ellipsoidal grains, and grains composed of an agglomeration of spherical particles 
* **Single and multiple-composition grains** (multiple-composition grains are only available for agglomerations of particles). Materials are specified by index files which specify the complex index of refraction and dielectric constant as a function of photon energy.
* **Ability to do integrated cross sections as well as differential-scattering cross section**

A word of caution: GGADT has **not been tested on Windows**. If you own a Windows machine and get GGADT to compile and run, let us know what you did and we'll try to relay that information. If you try and *fail* to get GGADT to run, and you'd like to use it on a Windows machine, let us know and we'll try to help.

