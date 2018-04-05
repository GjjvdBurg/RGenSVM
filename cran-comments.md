## Test environments
* local Arch linux install, R 3.4.4
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Gertjan van den Burg <gertjanvandenburg@gmail.com>'

  New submission

  Possibly mis-spelled words in DESCRIPTION:
    GenSVM (8:18, 10:61, 15:2, 16:26, 19:11)
    Multiclass (4:22)
    SVMs (14:25, 15:42)
    misclassifications (11:49)
    multiclass (8:53, 14:14, 15:31)


  This is a new submission and these words are not mis-spelled.

* checking compiled code ... NOTE
  File ‘gensvm/libs/gensvm_wrapper.so’:
    Found ‘rand’, possibly from ‘rand’ (C)
      Objects: ‘gensvm/src/gensvm_cv_util.o’, ‘gensvm/src/gensvm_init.o’,
        ‘gensvm/lib/libgensvm.a’
    Found ‘srand’, possibly from ‘srand’ (C)
      Objects: ‘gensvm/src/gensvm_train.o’, ‘gensvm/lib/libgensvm.a’

  Compiled code should not call entry points which might terminate R nor
  write to stdout/stderr instead of to the console, nor use Fortran I/O
  nor system RNGs.


  rand() and srand() are kept to maintain compatibility between the GenSVM C 
  library and the Python package. They are not used for cryptographic 
  purposes.
