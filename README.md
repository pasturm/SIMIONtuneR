# R package: SIMIONtuneR
<!-- badges: start -->
[![R build status](https://github.com/pasturm/SIMIONtuneR/workflows/R-CMD-check/badge.svg)](https://github.com/pasturm/SIMIONtuneR/actions)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

# SIMION Parameter Tuning using Response Surface Methodology

## Overview

The R package provides an interface to the ion optics simulation program 
[SIMION](http://simion.com/) for parameter optimization using design of 
experiments (DoE) and response surface methodology (RSM). This is an alternative 
way to optimize parameters in SIMION and might be more efficient than SIMION's 
simplex optimization or genetic algorithms. 

A typical RSM optimization workflow includes:

* Generation of a Box-Behnken design (in 3 to 7 factors), 
* Running SIMION simulations in batch mode (optionally parallelized with ZeroMQ),
* Fitting second-order response-surface models to the responses (typically 
sensitivity and resolution),
* Optimizing the model to find the best parameters,
* Running the simulation with the optimized parameters to validate the model,
* Generate a new Box-Behnken design with the optimized parameters as starting 
values, etc.

The optimization closely follows the approach of the TOFWERK Thuner and 
underlying Umetrics MODDE-Q software. See the Thuner and MODDE manuals for a 
detailed overview of the tuning methods. Notable differences to Thuner/MODDE are:

* The response surface model optimization works much better (due to improved 
optimization algorithms and desirability functions) (+).
* The optimization is much easier to configure (+). 
* It is open source (+). 
* It can be used to efficiently optimize SIMION simulations (+).
* It does not have a graphical user interface (-). 

## Getting Started

### Package Installation
```r
if (!require("remotes")) { install.packages("remotes") }
remotes::install_github("pasturm/SIMIONtuneR")
```

### Required SIMION files

1.  Copy [singlelib_pst.lua](https://github.com/pasturm/SIMIONtuneR/blob/master/tools/singlelib_pst.lua) to your SIMION workbench directory.
1.  Edit your workbench user program with the code from
    [example.lua](https://github.com/pasturm/SIMIONtuneR/blob/master/tools/example.lua):
    *   Add the `-- SIMIONtuneR variables` to your workbench user program.
    *   Add the `segment.flym()` code to your workbench user program.
    *   Adjust the `runner.jobrun(i, ...)` function. 
    *   Define the response variables `resolution` and `sensitivity` 
        (the variables you want to optimize) in the `segment.terminate_run()` 
        function of your workbench user program. 
    
### Configuration file

The parameters for tuning and for communicating between SIMION and R are 
configured in a configuration file.
For example, open [SIMIONtuneR_config.toml](https://github.com/pasturm/SIMIONtuneR/blob/master/inst/SIMIONtuneR_config.toml) and adjust the parameters according to your needs. In particular, the name of the SIMION
workbench file, and the factors and controls sections needs to be adjusted.
The names of the controls need to be the same as the parameters of the
`runner.jobrun(i, ...)` function in the lua workbench user program.

### Running

```r
library(SIMIONtuneR)
run_SIMIONtuneR("SIMIONtuneR_config.toml")
```

The tuning results are plotted using [plotly](https://plotly.com/r/) and the 
experiment and result files are written to disk.

With `nogui = FALSE` SIMION log messages are shown and with `write = FALSE` no 
output files are written. This can be helpful for debugging.

Usually, the starting values are taken from the configuration file. But you can 
also start from a previous best point. To do so, set `resume = TRUE`
and the starting values from the bestpoint.txt file in the tuneR directory
will be used.

### Parallel Processing

Parallel computing can be used to speed up the optimization. This is based on 
the ZeroMQ library.

1.   Add the ZeroMQ library to your SIMION installation.
Download `simion-lib-multiplatform-20140611b.zip` from the SIMION update webpage, 
and replace the `c:\Program Files\SIMION-8.x\lib` folder with the lib folder 
corresponding to your platform in that ZIP file. Also download `vcomp140.zip` 
from the SIMION update webpage and copy `vcomp140.dll` to your lib folder. 
Restart SIMION. If successful, entering `require "zmq"` into the SIMION command 
bar should execute without error (status OK). 
1.  Copy the following files to your SIMION workbench directory:
    *   [master.iob](https://github.com/pasturm/SIMIONtuneR/blob/master/tools/master.iob)
    *   [parallellib_pst.lua](https://github.com/pasturm/SIMIONtuneR/blob/master/tools/parallellib_pst.lua)  
    *   [close_children.lua](https://github.com/pasturm/SIMIONtuneR/blob/master/tools/close_children.lua) 
1. Adjust the `np` parameter in the configuration file. It defines the 
number of SIMION processes that will be run in parallel. Note that the number of
processes which make sense to run is often limited by the available RAM rather
by the number of available cores. 
1. The script [load_remote_workers.R](https://github.com/pasturm/SIMIONtuneR/blob/master/tools/load_remote_workers.R)
shows how additional SIMION instances can be used on remote computers (optional).
1. SIMIONtuneR can then be run in parallel processing mode
using `run_SIMIONtuneR(..., zmq = TRUE)`.

Note that if only relatively few ions are flown and if small potential arrays 
are used then `zmq = FALSE` might still be faster over all.

## Release notes
See the [NEWS file](https://github.com/pasturm/SIMIONtuneR/blob/master/NEWS.md) for the latest release notes.
    
## Author
Patrick Sturm, TOFWERK

## License
GPL-3
