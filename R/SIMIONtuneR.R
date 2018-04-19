#' Runs SIMIONtuneR simulation.
#'
#' \code{run_SIMIONtuneR} runs SIMIONtuneR simulation.
#'
#' The experiment needs to be configured in a tuneR_config file (TOML file format).
#' If you want to start from previous best point, copy bestpoint.txt to tuneR_dir
#' (delete bestpoint.txt if you want to start with starting values from tuneR_config).
#'
#' @param tuneR_config path and name of tuneR_config file (.toml)
#' @param nogui run SIMION with --nogui option (\code{TRUE} (default) or \code{FALSE})
#'
#' @examples
#' configfile = system.file("tuneR_config.toml", package = "SIMIONtuner")
#' run_SIMIONtuner(configfile)
#'
#' @export
run_SIMIONtuneR = function(tuneR_config, nogui = TRUE) {

  # configuration --------------------------------------------------------------

  # load config file
  config = RcppTOML::parseTOML(tuneR_config)

  # iob file path and name
  iob = config$iob

  # tuner result path
  tuneR_dir = file.path(dirname(iob), "tuneR")
  if (!dir.exists(tuneR_dir)) { dir.create(tuneR_dir) }

  # set working directory to SIMION executable directory
  setwd(config$simion_dir)

  # number of SIMION processes
  np = config$np
  
  # close zombie processes (if any)
  system(paste0("lua \"", dirname(iob), "/close_children.lua\""))
  
  # open worker processes
  for (i in seq_len(np)) {
    if (nogui) {
      system(paste0("simion --nogui --quiet fly --adjustable master=0 
                    --adjustable tuneR=1 \"", iob, "\""), 
             wait = FALSE, show.output.on.console = FALSE)
    } else {
      system(paste0("simion fly --adjustable master=0 --adjustable tuneR=1 \"", 
                    iob, "\""), wait = FALSE, show.output.on.console = FALSE)
    }
  }
  
  # loading and fast adjusting PAs might take some time -> make sure master runs
  # jobs only after all workers are ready.
  Sys.sleep(20)
  
  # copy the iob script to master.lua
  # this allow to run a master (with master = 1) without PAs.
  file.copy(sub("iob", "lua", iob), file.path(dirname(iob), "master.lua"), overwrite = TRUE)

  # number of ions on detector per run
  n_ions = config$n_ions

  # response variables for optimization
  responses = do.call(rbind.data.frame, c(config$responses, stringsAsFactors = FALSE))
  
  # delete previous results file if run was aborted
  if (file.exists(file.path(tuneR_dir, "results.txt"))) {
    file.remove(file.path(tuneR_dir, "results.txt"))
  }
  
  # factors
  factors = do.call(rbind.data.frame, c(config$factors, stringsAsFactors = FALSE))
  factors$Enabled = as.logical(factors$Enabled)
  nfact0 = length(factors$Name)  # number of factors
  nfact = length(factors$Name[factors$Enabled])  # number of enabled factors

  # controls with start values
  controls = do.call(rbind.data.frame, c(config$controls, stringsAsFactors = FALSE))
  ncont = length(controls$Name)  # number of factors

  # assign starting values of controls to a variable
  for (i in 1:ncont) {
    if (!is.na(controls$StartValue[i])) {
      assign(controls$Name[i], controls$StartValue[i])
    }
  }

  # evaluate factor values
  for (i in 1:nfact0) {
    factors$Value[i] = eval(parse(text = factors$Transformation[i]))
  }

  # possibly overwrite factor values with previous bestpoint.txt copied to tuneR_dir
  if (file.exists(file.path(tuneR_dir,"bestpoint.txt"))) {
    bestpoint_cvs = read.csv(file.path(tuneR_dir,"bestpoint.txt"), stringsAsFactors = FALSE)
    for (i in 1:length(bestpoint_cvs)) {
      factors$Value[factors$Name==names(bestpoint_cvs)[i]] = as.numeric(bestpoint_cvs[i])
    }
  }

  # formula names for rsm
  xnam = paste0("x", 1:nfact)

  # sequence loop --------------------------------------------------------------
  timestring = format(Sys.time(), "%Y-%m-%d-%Hh%Mm%Ss")
  for (k in 1:config$n_repeats) {

  # generate Box-Behnken design ------------------------------------------------

  if (k>1) {
    # load factors from bestpoint of last run
    for (i in 1:length(bestpoint)) {
      factors$Value[factors$Name==names(bestpoint)[i]] = as.numeric(bestpoint[i])
    }
  }

  factors_enabled = factors[factors$Enabled,]

  # coerce to limits
  for (i in (1:nfact)) {
    if (is.na(factors_enabled$LowLimit[i]) & !is.na(factors_enabled$HighLimit[i])) {
      factors_enabled$Value[i] = min(factors_enabled$Value[i],
                                     factors_enabled$HighLimit[i] - factors_enabled$Range[i])
    }
    else if (!is.na(factors_enabled$LowLimit[i]) & is.na(factors_enabled$HighLimit[i])) {
      factors_enabled$Value[i] = max(factors_enabled$LowLimit[i] + factors_enabled$Range[i],
                                     factors_enabled$Value[i])
    }
    else if (!is.na(factors_enabled$LowLimit[i]) & !is.na(factors_enabled$HighLimit[i])) {
      minValue = min(factors_enabled$LowLimit[i] + factors_enabled$Range[i],
                     factors_enabled$HighLimit[i])
      maxValue = max(factors_enabled$HighLimit[i] - factors_enabled$Range[i],
                     factors_enabled$LowLimit[i])
      factors_enabled$Value[i] = min(max(minValue, factors_enabled$Value[i]), maxValue)
    }
  }

  # bbd coding (see ?bbd)
  coding = lapply(1:nfact, function(i)
    as.formula(paste(xnam[i], "~ (", factors_enabled$Name[i], 
                     if (factors_enabled$Value[i] > 0) {"-"} else {"+"},
                     abs(factors_enabled$Value[i]), ") /", factors_enabled$Range[i])))
  design = rsm::bbd(nfact, n0 = 1, randomize = FALSE, coding = coding, block = FALSE)

  # run SIMION runs -----------------------------------------------------
  # make tuneR result directory
  resultdir = paste(timestring, formatC(k, width = 2, flag = "0"), sep="_")
  dir.create(file.path(tuneR_dir, resultdir))

  # bbd_data
  bbd_data = rsm::decode.data(design[,3:(3+nfact-1)])

  # assign factor values to variables
  for (i in 1:nfact0) {
    assign(factors$Name[i], factors$Value[i])
  }

  # overwrite enabled factors with bbd_data values
  for (i in 1:nfact) {
    assign(names(bbd_data)[i], bbd_data[i])
  }

  # make runs data.frame
  runs = data.frame(run_no = seq_len(length(bbd_data[,1])))
  for (i in 1:ncont) {
    runs[controls$Name[i]] = tryCatch(eval(parse(text = controls$Transformation[i])),
                                      error = function(e) controls$StartValue[i])
  }

  write.table(signif(runs, 12), file = file.path(tuneR_dir, "runs.txt"), sep = "|", 
              row.names = FALSE, col.names = FALSE, eol = "|\n")
  write.table(signif(runs, 12), file = file.path(tuneR_dir, resultdir, "runs.txt"), sep = "|", 
              row.names = FALSE, col.names = TRUE, eol = "|\n")

  print(paste0("Repeat ", k, ", run ", 1, " to ", length(runs[,1]),  " running..."))
  
  # run simulations
  flyoptions = paste0("--recording-enable=0 --adjustable tuneR=1 ", 
                      "--adjustable master=1 --adjustable maxn=", n_ions)
  if (nogui) {
    system(paste("simion --nogui --quiet fly", flyoptions, 
                 file.path(dirname(iob), "master.iob")), show.output.on.console = FALSE)
  } else {
    system(paste("simion fly", flyoptions, 
                 file.path(dirname(iob), "master.iob")), show.output.on.console = FALSE)
  }

  Sys.sleep(1)  # make sure all results are written before program resumes.

  # fit quadratic model and optimize -------------------------------------------

  # read in resolution and sensitivity from results.txt (generated by Lua script)
  result = read.table(file.path(tuneR_dir, "results.txt"), sep = "|",
                      col.names = c("no", "res", "sens"))
  
  result$sens[result$res==-1] = 0  # if no ions hit the detector
  result$res[result$res==-1] = NA   # if no ions hit the detector
  # result$res[result$sens<0.01] = NA   # too few ions hit the detector
  result = result[order(result$no),]  # order

  # copy results.txt to resultsdir
  file.copy(file.path(tuneR_dir, "results.txt"), file.path(tuneR_dir, resultdir, "results.txt"))
  file.remove(file.path(tuneR_dir, "results.txt"))
  
  design$res = result$res
  design$sens = result$sens
  # design = design[design$sens!=0,]

  # fit second order model
  tuner_rsm_R = rsm::rsm(as.formula(paste("res ~ + SO(", paste(xnam, collapse = ","), ")")), 
                         data = design)
  tuner_rsm_S = rsm::rsm(as.formula(paste("sens ~ + SO(", paste(xnam, collapse = ","), ")")), 
                         data = design)

  # optimize (using L-BFGS-B method)

  # Pred_min/Pred_max values (used to define desirability function)
  Pred_min_R = optim(rep(0, nfact), fn = cost_function, rsm_output = tuner_rsm_R,
                     nfact = nfact, xnam = xnam, method = "L-BFGS-B",
                     lower = rep(-1, nfact), upper = rep(1, nfact),
                     control = list(fnscale = 1))$value
  Pred_max_R = optim(rep(0, nfact), fn = cost_function, rsm_output = tuner_rsm_R,
                     nfact = nfact, xnam = xnam, method = "L-BFGS-B",
                     lower = rep(-1, nfact), upper = rep(1, nfact),
                     control = list(fnscale = -1))$value
  Pred_min_S = optim(rep(0, nfact), fn = cost_function, rsm_output = tuner_rsm_S,
                     nfact = nfact, xnam = xnam, method = "L-BFGS-B",
                     lower = rep(-1, nfact), upper = rep(1, nfact),
                     control = list(fnscale = 1))$value
  Pred_max_S = optim(rep(0, nfact), fn = cost_function, rsm_output = tuner_rsm_S,
                     nfact = nfact, xnam = xnam, method = "L-BFGS-B",
                     lower = rep(-1, nfact), upper = rep(1, nfact),
                     control = list(fnscale = -1))$value

  # to catch issue that if all sens are 1, Pred_min_S can be >1 (1+4.440892e-16) 
  # -> factors_optim does not work
  # also, if Pred_min_S = Pred_max_S = 1, sometimes factors_optim does not work
  Pred_min_S = min(Pred_min_S, 0.9999)
  Pred_max_S = min(Pred_max_S, 1)

  # run optimizer
  factors_optim = optim(rep(0, nfact), fn = desirability_overall,
                        Target = responses$Target, w = responses$Weight,
                        nfact = nfact, xnam = xnam,
                        tuner_rsm = list(tuner_rsm_R, tuner_rsm_S),
                        Pred_min = c(Pred_min_R, Pred_min_S),
                        Pred_max = c(Pred_max_R, Pred_max_S), method = "L-BFGS-B",
                        lower = rep(-1, nfact), upper = rep(1, nfact))$par

  factors_optim_coded = data.frame(t(factors_optim))
  colnames(factors_optim_coded) = xnam[1:nfact]
  bestpoint = rsm::code2val(factors_optim_coded, rsm::codings(design))
  bestpoint_predicted = data.frame(res = predict(tuner_rsm_R, factors_optim_coded),
                                   sens = predict(tuner_rsm_S, factors_optim_coded))

  # verify bestpoint -----------------------------------------------------------

  # assign bestpoint values to variables
  for (i in 1:nfact) {
    assign(names(bestpoint)[i], bestpoint[i])
  }

  # make runs data.frame
  bestpoint_run = data.frame(run_no = 1)
  for (i in 1:ncont) {
    bestpoint_run[controls$Name[i]] = tryCatch(eval(parse(text = controls$Transformation[i])),
                                               error = function(e) controls$StartValue[i])
  }

  # run experiment
  print(paste(paste0("Best point verification run:"),
              paste0(names(bestpoint_run), "=", round(bestpoint_run,2), collapse = ", ")))

  write.table(signif(bestpoint_run, 12), file = file.path(tuneR_dir, "runs.txt"), sep = "|",
                row.names = FALSE, col.names = FALSE, eol = "|\n")
  write.table(signif(bestpoint_run, 12), file = file.path(tuneR_dir, resultdir, "bestpoint_run.txt"), sep = "|",
              row.names = FALSE, col.names = TRUE, eol = "|\n")

  # run simulation
  flyoptions = paste0("--recording-enable=0 --adjustable tuneR=1 ", 
                      "--adjustable master=1 --adjustable maxn=", n_ions)
  if (nogui) {
    system(paste("simion --nogui --quiet fly", flyoptions, 
                 file.path(dirname(iob), "master.iob")), show.output.on.console = FALSE)
  } else {
    system(paste("simion fly", flyoptions, 
                 file.path(dirname(iob), "master.iob")), show.output.on.console = FALSE)
  }

  # read in resolution and sensitivity from results.txt (generated by Lua script)
  bestpoint_result = read.table(file.path(tuneR_dir, "results.txt"), sep = "|",
                       col.names = c("no", "res", "sens"))

  # copy results.txt to resultsdir
  file.copy(file.path(tuneR_dir, "results.txt"), 
                 file.path(tuneR_dir, resultdir, "bestpoint_results.txt"))
  file.remove(file.path(tuneR_dir, "results.txt"))
  file.remove(file.path(tuneR_dir, "runs.txt"))

  # plot results ---------------------------------------------------------------
  
  if (k==1) {
    # generate plotly object
    pltly = plotly::plot_ly(result[1:(length(result[,1])-1),], x = ~res)
  }

  pltly = plot_results(pltly, k, config$n_repeats, resultdir, result, runs, 
                       bestpoint_run, bestpoint_result, bestpoint_predicted, 
                       responses$Name)

  # save all data as .RData
  save(list = ls(all.names = TRUE), 
       file = file.path(tuneR_dir, resultdir, "results.RData"))

  # end of loop
  }

  # close worker processes
  system(paste0("lua \"", dirname(iob), "/close_children.lua\""))
  
}
