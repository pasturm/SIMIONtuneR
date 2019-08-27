# Version 0.2.3

* Added `write` parameter in `run_SIMIONtuner()` which (if `FALSE`) allows to 
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
