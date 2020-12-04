# Version 0.3.2

* Log messages adjusted.

* Using GitHub Actions instead of Travis CI for continuous integration. 


# Version 0.3.1

* The path of the SIMION workbench file can be given relative to the path of the
  configuration file.
  
* Removed the `n_ions` variable in the configuration file. The number of ions 
  can be configured in SIMION's fly2 file.


# Version 0.3.0

* Reorganized code so that SIMIONtuneR can be run without a ZeroMQ installation.


# Version 0.2.5

* Added 'Getting Started' section to README with instructions on how to setup
  SIMIONtuneR. 


# Version 0.2.4

* Added `zmq` parameter in `run_SIMIONtuneR()` which (if `FLASE`) allows to run
  SIMION without the ZeroMQ messaging library, i.e. in single-process mode.


# Version 0.2.3

* Added `write` parameter in `run_SIMIONtuneR()` which (if `FALSE`) allows to 
  run the optimization without writing output files to disk.


# Version 0.2.2

* Moved non-SIMION related functions to another (imo) package.


# Version 0.2.1

* No need to use forked rsm package any more. Using rsm from CRAN.
* Bug fixes.
* Using Travis CI.


# Version 0.2.0

* Various bug fixes.
* Added functions which allow to optimize the geometry and voltages of some ion
  mirrors directly in R.


# Version 0.1.0

* First release of SIMIONtuneR.
* Using forked rsm package https://github.com/pasturm/rsm
* SIMION parallel processing based on the ZeroMQ library.
