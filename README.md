# SIMIONtuneR
SIMION Parameter Tuning using Response-Surface Design.

The R package provides functions for parameter optmization in SIMION software using 
design-of-expriments methods. This is an alternative way to optimize and 
might be more efficient than SIMION's simplex or genetic algorithm methods. 
SIMION is controlled via its command line interface and a typical optimization 
workflow includes:

* Generation of a Box-Behnken design (in 3 to 7 factors), 
* Running SIMION simulations in batch mode (parallelized with ZeroMQ),
* Fitting second-order response-surface models to the responses (typically sensitivity and resolution),
* Optimizing the model to find the best point,
* Running the simulation with the best point to verify the model,
* Gernerate a new Box-Behnken design with the best point as starting values,
* etc.

The tuning parameters are configured in a configuration file (tuneR_config.toml)
and the lua script of the SIMION simulation needs to be adjusted accordingly.


## Version history
See [NEWS](https://github.com/pasturm/SIMIONtuneR/blob/master/NEWS).

## Installation
```
install.packages("devtools")
devtools::install_github("pasturm/SIMIONtuneR")
```

## Notes
* This is currently very poorly documented. 
* See [tools](https://github.com/pasturm/SIMIONtuneR/blob/master/tools/)
for additional files, which are required for the SIMION simulations.
* Parallel processing is based on the ZeroMQ (ZMQ) library, which is 
bundled with SIMION 8.2 Early Access Mode.
* Currently this only works for voltage optimization, but it might be extended to 
geometry optimization in the future.
* The design of experiments and response-surface method closely follows the approach 
of the TOFWERK Thuner and underlying MKS MODDE-Q software. Notable differences to Thuner/MODDE are:
    * (+) It is open source.
    * (+) The response-surface model optimization works much better (due to improved optimization alogorithms and desirability functions).
    * (+) It can be used to optimize SIMION simulations.
    * (-) It is not a self-contained program and does not have a graphical user interface.
