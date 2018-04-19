# SIMIONtuneR
SIMION Parameter Tuning using Response Surface Methodology.

The R package provides an interface to the ion optics simulation program 
[SIMION](http://simion.com/) for parameter optmization using design of experiments (DoE)
and response surface methodology (RSM). This is an alternative way to optimize parameters in SIMION and 
might be more efficient than SIMION's simplex optimization or genetic algorithms. 

A typical RSM optimization workflow includes:

* Generation of a Box-Behnken design (in 3 to 7 factors), 
* Running SIMION simulations in batch mode (parallelized with ZeroMQ),
* Fitting second-order response-surface models to the responses (typically sensitivity and resolution),
* Optimizing the model to find the best point,
* Running the simulation with the best point to verify the model,
* Generate a new Box-Behnken design with the best point as starting values, etc.

The tuning parameters are configured in a [configuration file](https://github.com/pasturm/SIMIONtuneR/blob/master/inst/tuneR_config.toml)
and the [lua script](https://github.com/pasturm/SIMIONtuneR/blob/master/tools/example.lua)
of the SIMION simulation needs to be adjusted accordingly.


## Installation
```
install.packages("devtools")
devtools::install_github("pasturm/SIMIONtuneR")
```

## Version history
See [NEWS](https://github.com/pasturm/SIMIONtuneR/blob/master/NEWS).


## Notes
* This is currently very poorly documented. 
* See [tools](https://github.com/pasturm/SIMIONtuneR/blob/master/tools/)
for additional files, which are required in the SIMION workbench directory.
* Parallel processing is based on the ZeroMQ library, which is 
bundled with SIMION 8.2 Early Access Mode.
* Currently this only works for voltage optimization, but it might be extended to 
geometry optimization in the future.
* The design of experiments and response surface method closely follows the approach 
of the TOFWERK Thuner and underlying MKS MODDE-Q software. Notable differences to Thuner/MODDE are:
    * It is open source (+). 
    * The response surface model optimization works much better (due to improved optimization algorithms and desirability functions) (+). 
    * It can be used to optimize SIMION simulations (+).
    * It is not a self-contained program and does not have a graphical user interface (-). 
    * It is relatively easy to configure (provided you are somewhat familiar with SIMION and R) (+).   
