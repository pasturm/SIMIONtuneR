# Script to manually load workers on remote computers.
#
# - to be exectuted on the remote computer.
# - requires SIMION installation with identical copy of iob files on remote computer.
# - The IP address of the master computer needs to be configured in parallellib_pst.lua.
# - remote workers need to be started before the master process starts.

setwd("C:/Program Files/SIMION-8.1")
np = 4  # Number of processes 
n_ions = 300  # Number of ions per run
iob = "C:/SIMION/test.iob"  # SIMION iob
nogui = TRUE  # --nogui option

flyoptions = paste0("--recording-enable=0 --adjustable tuneR=1 ",
                    "--adjustable master=0 --adjustable zmq=1 --adjustable maxn=", n_ions)

for (i in seq_len(np)) {
  if (nogui) {
    system(paste("simion --nogui --quiet fly", flyoptions, iob), 
           wait = FALSE, show.output.on.console = FALSE)
  } else {
    system(paste("simion fly", flyoptions, iob), 
           wait = FALSE, show.output.on.console = FALSE)
  }
}
