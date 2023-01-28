# NEWS file for the GenSVM R package

## Version 0.1.7

* Remove any use of (v)sprintf in C code, following the new R package 
  guidelines
* Enforce strict prototypes in C code

## Version 0.1.6

* Bugfix for Fortran calls using `FC_LEN_T`

## Version 0.1.5

* Fix segmentation fault on Solaris.

## Version 0.1.4

* Fix return without () in preparation of future checks.

## Version 0.1.3

* Updated C library to support ``-fno-common`` flag for GCC.

## Version 0.1.2

* Fixed a memory leak that occurred when using a raw weight vector.
