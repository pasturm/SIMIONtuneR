# run_SIMIONtuneR --------------------------------------------------------------
#' Runs SIMIONtuneR simulation.
#'
#' \code{run_SIMIONtuneR} runs SIMIONtuneR simulation.
#'
#' The experiment needs to be configured in a tuneR_config file (TOML file format).
#' See \code{system.file("SIMIONtuneR_config.toml", package = "SIMIONtuneR")} for 
#' a template.
#' 
#' If you want to start from a previous best point, set \code{resume = TRUE}.
#' If there is a bestpoint_run.txt file in the tuneR directory, the starting 
#' values will be taken from there. Otherwise the latest bestpoint_run.txt from
#' all subfolders of tuneR will be taken.
#'
#' @param tuneR_config Path and name of tuneR_config file (.toml).
#' @param nogui Run SIMION with --nogui option (\code{TRUE} (default) or \code{FALSE}).
#' @param write Write output files (\code{TRUE} (default) or \code{FALSE}).
#' @param zmq Use the ZeroMQ library for parallel processing (\code{TRUE} or \code{FALSE} (default)).
#' @param resume If \code{FALSE} (default) starting values are taken from the
#' tuneR_config file, if \code{TRUE} starting values are taken from the last
#' bestpoint_run.txt (see 'Details').
#' @param digits Controls the number of decimal places to print when
#' printing the best point values.
#'
#' @return A data.frame containing the optimized best points from the last run. 
#'
#' @examples
#' \dontrun{
#' tuneR_config = system.file("SIMIONtuneR_config.toml", package = "SIMIONtuneR")
#' run_SIMIONtuner(tuneR_config)
#' }
#' 
#' @export
run_SIMIONtuneR = function(tuneR_config, nogui = TRUE, write = TRUE, 
                           zmq = FALSE, resume = FALSE, digits = 2) {

  # configuration --------------------------------------------------------------

  # load config file
  config = RcppTOML::parseTOML(tuneR_config)

  # iob file
  wd = setwd(dirname(tuneR_config))
  iob = normalizePath(config$iob)
  setwd(wd)

  # tuner result path
  tuneR_dir = file.path(dirname(iob), "tuneR")
  if (!dir.exists(tuneR_dir)) { dir.create(tuneR_dir) }

  # set working directory to SIMION executable directory
  wd = setwd(config$simion_dir)
  
  if (zmq) {
    # check if zmq is installed
    if (system(paste("simion  --nogui --quiet --lua \"require 'zmq'\""), ignore.stdout = TRUE) != 0) {
      stop("ZeroMQ library not found.")
    }
    # number of SIMION processes
    np = config$np
    # open worker processes
    flyoptions = paste0("--recording-enable=0 --adjustable tuneR=1 ", 
                        "--adjustable master=0 --adjustable zmq=1")
    for (i in seq_len(np)) {
      if (nogui) {
        system(paste("simion --nogui --quiet fly", flyoptions, iob), 
               wait = FALSE, show.output.on.console = FALSE)
      } else {
        system(paste("simion fly", flyoptions, iob), 
               wait = FALSE, show.output.on.console = FALSE)
      }
    }
    # close worker processes on exit
    on.exit(system(paste0("lua \"", dirname(iob), "/close_children.lua\"")), add = TRUE)
    
    # copy the iob script to master.lua
    # this allow to run a master (with master = 1) without PAs.
    file.copy(sub("iob", "lua", iob), file.path(dirname(iob), "master.lua"), overwrite = TRUE)
    
    # loading and fast adjusting PAs might take some time -> make sure master runs
    # jobs only after all workers are ready.
    Sys.sleep(20)
  }
  
  on.exit(setwd(wd), add = TRUE)  # restore working directory
  
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
  
  # possibly overwrite start values with previous bestpoint_run.txt
  if (resume) {
    allfiles = list.files(tuneR_dir, pattern = "bestpoint_run.txt", full.names = TRUE, recursive = TRUE)
    if (length(allfiles)>0) {
      lastfile = allfiles[length(allfiles)]
      warning(paste("Start values are taken from", lastfile), immediate. = TRUE)
      bestpoint_csv = utils::read.csv(lastfile, stringsAsFactors = FALSE, sep = "|")
      for (i in 2:(length(bestpoint_csv)-1)) {
        controls$StartValue[controls$Name==names(bestpoint_csv)[i]] = as.numeric(bestpoint_csv[i])
      }
    }
  }
  
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
    stats::as.formula(paste(xnam[i], "~ (", factors_enabled$Name[i], 
                     if (factors_enabled$Value[i] > 0) {"-"} else {"+"},
                     abs(factors_enabled$Value[i]), ") /", factors_enabled$Range[i])))
  design = rsm::bbd(nfact, n0 = 1, randomize = FALSE, coding = coding, block = FALSE)

  # run SIMION runs -----------------------------------------------------
  # make tuneR result directory
  resultdir = paste(timestring, formatC(k, width = 2, flag = "0"), sep="_")
  if (write) {
    dir.create(file.path(tuneR_dir, resultdir))
  }

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

  utils::write.table(signif(runs, 12), file = file.path(tuneR_dir, "runs.txt"), 
                     sep = "|", row.names = FALSE, col.names = FALSE, eol = "|\n")
  if (write) {
    utils::write.table(signif(runs, 12), file = file.path(tuneR_dir, resultdir, "runs.txt"), 
                       sep = "|", row.names = FALSE, col.names = TRUE, eol = "|\n")
  }

  print(paste0("Repeat ", k, ", experiment ", 1, " to ", length(runs[,1]),  " running..."))
  
  # run simulations
  if (zmq) {
    flyoptions = paste0("--recording-enable=0 --adjustable tuneR=1 ",
                        "--adjustable master=1 --adjustable zmq=1")
    if (nogui) {
      system(paste("simion --nogui --quiet fly", flyoptions, 
                   file.path(dirname(iob), "master.iob")), show.output.on.console = FALSE)
    } else {
      system(paste("simion fly", flyoptions, 
                   file.path(dirname(iob), "master.iob")), show.output.on.console = FALSE)
    }
    Sys.sleep(1)  # make sure all results are written before program resumes.
  } else {
    flyoptions = paste0("--recording-enable=0 --adjustable tuneR=1 --remove-pas=0 ",
                        "--adjustable master=1 --adjustable zmq=0")
    if (nogui) {
      system(paste("simion --nogui --quiet fly", flyoptions, iob), 
             show.output.on.console = FALSE)
    } else {
      system(paste("simion fly", flyoptions, iob), 
             show.output.on.console = FALSE)
    }
  }
  
  # fit quadratic model and optimize -------------------------------------------

  # read in resolution and sensitivity from results.txt (generated by Lua script)
  result = utils::read.table(file.path(tuneR_dir, "results.txt"), sep = "|",
                             col.names = c("no", "res", "sens"))
  
  result$sens[result$res==-1] = 0  # if no ions hit the detector
  result$res[result$res==-1] = NA   # if no ions hit the detector
  # result$res[result$sens<0.01] = NA   # too few ions hit the detector
  result = result[order(result$no),]  # order

  # copy results.txt to resultsdir
  if (write) {
    file.copy(file.path(tuneR_dir, "results.txt"), file.path(tuneR_dir, resultdir, "results.txt"))
  }
  file.remove(file.path(tuneR_dir, "results.txt"))
  
  design$res = result$res
  design$sens = result$sens
  # design = design[design$sens!=0,]

  # fit second order model
  tuner_rsm_R = rsm::rsm(stats::as.formula(paste("res ~ + SO(", paste(xnam, collapse = ","), ")")), 
                         data = design)
  tuner_rsm_S = rsm::rsm(stats::as.formula(paste("sens ~ + SO(", paste(xnam, collapse = ","), ")")), 
                         data = design)

  # optimize (using L-BFGS-B method)

  # Pred_min/Pred_max values (used to define desirability function)
  Pred_min_R = stats::optim(rep(0, nfact), fn = cost_function, rsm_output = tuner_rsm_R,
                     nfact = nfact, xnam = xnam, method = "L-BFGS-B",
                     lower = rep(-1, nfact), upper = rep(1, nfact),
                     control = list(fnscale = 1))$value
  Pred_max_R = stats::optim(rep(0, nfact), fn = cost_function, rsm_output = tuner_rsm_R,
                     nfact = nfact, xnam = xnam, method = "L-BFGS-B",
                     lower = rep(-1, nfact), upper = rep(1, nfact),
                     control = list(fnscale = -1))$value
  Pred_min_S = stats::optim(rep(0, nfact), fn = cost_function, rsm_output = tuner_rsm_S,
                     nfact = nfact, xnam = xnam, method = "L-BFGS-B",
                     lower = rep(-1, nfact), upper = rep(1, nfact),
                     control = list(fnscale = 1))$value
  Pred_max_S = stats::optim(rep(0, nfact), fn = cost_function, rsm_output = tuner_rsm_S,
                     nfact = nfact, xnam = xnam, method = "L-BFGS-B",
                     lower = rep(-1, nfact), upper = rep(1, nfact),
                     control = list(fnscale = -1))$value

  # to catch issue that if all sens are 1, Pred_min_S can be >1 (1+4.440892e-16) 
  # -> factors_optim does not work
  # also, if Pred_min_S = Pred_max_S = 1, sometimes factors_optim does not work
  Pred_min_S = min(Pred_min_S, 0.9999)
  Pred_max_S = min(Pred_max_S, 1)

  # run optimizer
  factors_optim = stats::optim(rep(0, nfact), fn = desirability_overall,
                        Target = responses$Target, w = responses$Weight,
                        nfact = nfact, xnam = xnam,
                        tuner_rsm = list(tuner_rsm_R, tuner_rsm_S),
                        Pred_min = c(Pred_min_R, Pred_min_S),
                        Pred_max = c(Pred_max_R, Pred_max_S), method = "L-BFGS-B",
                        lower = rep(-1, nfact), upper = rep(1, nfact))$par

  factors_optim_coded = data.frame(t(factors_optim))
  colnames(factors_optim_coded) = xnam[1:nfact]
  bestpoint = rsm::code2val(factors_optim_coded, rsm::codings(design))
  bestpoint_predicted = data.frame(res = stats::predict(tuner_rsm_R, factors_optim_coded),
                                   sens = stats::predict(tuner_rsm_S, factors_optim_coded))

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

  result_string = bestpoint_run[2:length(bestpoint_run)]
  print(paste0("Best point: ", paste0(names(result_string), "=", 
                                      round(result_string, digits), collapse = " | ")))

  utils::write.table(signif(bestpoint_run, 12), file = file.path(tuneR_dir, "runs.txt"), 
                     sep = "|", row.names = FALSE, col.names = FALSE, eol = "|\n")
  utils::write.table(signif(bestpoint_run, 12), file = file.path(tuneR_dir, "bestpoint_run.txt"), 
                     sep = "|", row.names = FALSE, col.names = TRUE, eol = "|\n")

  # run simulation
  if (zmq) {
    flyoptions = paste0("--recording-enable=0 --adjustable tuneR=1 ", 
                        "--adjustable master=1 --adjustable zmq=1")
    if (nogui) {
      system(paste("simion --nogui --quiet fly", flyoptions, 
                   file.path(dirname(iob), "master.iob")), show.output.on.console = FALSE)
    } else {
      system(paste("simion fly", flyoptions, 
                   file.path(dirname(iob), "master.iob")), show.output.on.console = FALSE)
    }
  } else {
    flyoptions = paste0("--recording-enable=0 --adjustable tuneR=1 --remove-pas=0 ", 
                        "--adjustable master=2 --adjustable zmq=0")
    if (nogui) {
      system(paste("simion --nogui --quiet fly", flyoptions, iob), show.output.on.console = FALSE)
    } else {
      system(paste("simion fly", flyoptions, iob), show.output.on.console = FALSE)
    }
  }

  # read in resolution and sensitivity from results.txt (generated by Lua script)
  bestpoint_result = utils::read.table(file.path(tuneR_dir, "results.txt"), 
                                       sep = "|", col.names = c("no", "res", "sens"))

  # copy results.txt to resultsdir
  if (write) {
    file.copy(file.path(tuneR_dir, "results.txt"), 
              file.path(tuneR_dir, resultdir, "bestpoint_results.txt"))
    file.copy(file.path(tuneR_dir, "bestpoint_run.txt"), 
              file.path(tuneR_dir, resultdir, "bestpoint_run.txt"))
  }
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
  if (write) {
    save(list = ls(all.names = TRUE), 
         file = file.path(tuneR_dir, resultdir, "results.RData"))
  }

  # end of loop
  }
  return(bestpoint_run)

}
