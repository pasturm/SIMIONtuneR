# SIMIONtuneR
[![Travis build status](https://travis-ci.org/pasturm/SIMIONtuneR.svg?branch=master)](https://travis-ci.org/pasturm/SIMIONtuneR)

SIMION Parameter Tuning using Response Surface Methodology.

The R package provides an interface to the ion optics simulation program 
[SIMION](http://simion.com/) for parameter optmization using design of experiments (DoE)
and response surface methodology (RSM). This is an alternative way to optimize parameters in SIMION and 
might be more efficient than SIMION's simplex optimization or genetic algorithms. 

A typical RSM optimization workflow includes:

* Generation of a Box-Behnken design (in 3 to 7 factors), 
* Running SIMION simulations in batch mode (parallelized with ZeroMQ),
* Fitting second-order response-surface models to the responses (typically sensitivity and resolution),
* Optimizing the model to find the best parameters,
* Running the simulation with the optimized parameters to validate the model,
* Generate a new Box-Behnken design with the optimized parameters as starting values, etc.

The tuning parameters are configured in a [configuration file](https://github.com/pasturm/SIMIONtuneR/blob/master/inst/SIMIONtuneR_config.toml)
and the [lua script](https://github.com/pasturm/SIMIONtuneR/blob/master/tools/example.lua)
of the SIMION simulation needs to be adjusted accordingly.

Additionally, the package provides functions to efficiently optimize the geometry and voltages of gridless planar
mirrors (directly in R, since the axial potential of such mirrors can be calculated analytically). 

## Installation
```
if (!require("devtools")) { install.packages("devtools") }
devtools::install_github("pasturm/SIMIONtuneR")
```

## Version history
See [NEWS](https://github.com/pasturm/SIMIONtuneR/blob/master/NEWS).


## Notes
* This is currently rather poorly documented. 
* See [tools](https://github.com/pasturm/SIMIONtuneR/blob/master/tools/)
for additional files, which are required in the SIMION workbench directory.
* Parallel computing is used to speed up the optimization. Parallel processing for SIMION is based on the ZeroMQ library, which is available on the SIMION
update webpage. 
* Currently this only works for voltage optimization in SIMION, but it might be extended to 
geometry optimization in the future.
* The design of experiments and response surface method closely follows the approach 
of the TOFWERK Thuner and underlying MKS MODDE-Q software. Notable differences to Thuner/MODDE are:
    * It is open source (+). 
    * The response surface model optimization works much better (due to improved optimization algorithms and desirability functions) (+).
    * The optimizaition is much easier to configure (+). 
    * It can be used to efficiently optimize SIMION simulations (+).
    * It can also be used to optimize geometries where the potential can be calculated analytically (+). 
    * It is not a self-contained program and does not have a graphical user interface (-). 
