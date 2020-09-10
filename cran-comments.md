
## Test environments
* Travis: Ubuntu Xenial (devel, release, and old-release)
* Travis: MacOS 10.13.6 (devel, release, and old-release)
* win-builder (devel, release, and old-release)
* R-hub: Solaris (32-bit, release), CentOS (64-bit, R 3.5.2), Fedora (gcc, R-devel)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Gertjan van den Burg <gertjanvandenburg@gmail.com>'

  Days since last update: 3

This update fixes an error that occurred on a particular Solaris build, which
unfortunately was not addressed in the last version.