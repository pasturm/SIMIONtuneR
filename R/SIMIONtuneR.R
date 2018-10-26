# run_SIMIONtuneR --------------------------------------------------------------
#' Runs SIMIONtuneR simulation.
#'
#' \code{run_SIMIONtuneR} runs SIMIONtuneR simulation.
#'
#' The experiment needs to be configured in a tuneR_config file (TOML file format).
#' See \code{system.file("SIMIONtuneR_config.toml", package = "SIMIONtuneR")} for 
#' a template.
#' 
#' If you want to start from a previous best point, copy bestpoint_run.txt to 
#' the tuneR directory (delete bestpoint_run.txt if you want to start with 
#' starting values from tuneR_config).
#'
#' @param tuneR_config path and name of tuneR_config file (.toml)
#' @param nogui run SIMION with --nogui option (\code{TRUE} (default) or \code{FALSE})
#'
#' @examples
#' tuneR_config = system.file("SIMIONtuneR_config.toml", package = "SIMIONtuneR")
#' run_SIMIONtuner(tuneR_config)
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
  wd = setwd(config$simion_dir)

  # number of SIMION processes
  np = config$np
  
  # number of ions on detector per run
  n_ions = config$n_ions
  
  # open worker processes
  flyoptions = paste0("--recording-enable=0 --adjustable tuneR=1 ", 
                      "--adjustable master=0 --adjustable maxn=", n_ions)
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
  on.exit(setwd(wd), add = TRUE)  # restore working directory
  
  # loading and fast adjusting PAs might take some time -> make sure master runs
  # jobs only after all workers are ready.
  Sys.sleep(20)
  
  # copy the iob script to master.lua
  # this allow to run a master (with master = 1) without PAs.
  file.copy(sub("iob", "lua", iob), file.path(dirname(iob), "master.lua"), overwrite = TRUE)

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

  # possibly overwrite start values with previous bestpoint_run.txt copied to tuneR_dir
  if (file.exists(file.path(tuneR_dir,"bestpoint_run.txt"))) {
    warning("Start values are taken from bestpoint_run.txt", immediate. = TRUE)
    bestpoint_csv = read.csv(file.path(tuneR_dir,"bestpoint_run.txt"), stringsAsFactors = FALSE, sep = "|")
    for (i in 2:(length(bestpoint_csv)-1)) {
      controls$StartValue[controls$Name==names(bestpoint_csv)[i]] = as.numeric(bestpoint_csv[i])
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

}

# run_GLPMtuneR ----------------------------------------------------------------
#' Runs gridless planar mirror optimization.
#'
#' \code{run_GLPMtuneR} runs gridless planar mirror optimization.
#'
#' The experiment needs to be configured in a tuneR_config file (TOML file format).
#' See \code{system.file("GLPMtuneR_config.toml", package = "SIMIONtuneR")} for 
#' a template.
#' 
#' If you want to start from a previous best point, set \code{resume = TRUE}.
#' If there is a bestpoint_run.txt file in the tuneR directory, the starting 
#' values will be taken from there. Otherwise the latest bestpoint_run.txt from
#' all subfolders of tuneR will be taken.
#'
#' @param tuneR_config path and name of tuneR_config file (.toml)
#' @param resume if \code{FALSE} (default) starting values are taken from the
#' tuneR_config file, if \code{TRUE} starting values are taken from the last
#' bestpoint_run.txt (see 'Details').
#'
#' @examples
#' tuneR_config = system.file("GLPMtuneR_config.toml", package = "SIMIONtuneR")
#' run_GLPMtuneR(tuneR_config)
#'
#' @export
run_GLPMtuneR = function(tuneR_config, resume = FALSE) {
  
  # configuration --------------------------------------------------------------
  
  # load config file
  config = RcppTOML::parseTOML(tuneR_config)
  
  # number of processes
  np = config$np
  
  # Create np copies of R running in parallel
  cl = parallel::makeCluster(np)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  # Energy range for which time-of-flight variations are minimized
  range_E = config$range_E
  
  # tuner result path
  tuneR_dir = file.path(dirname(tuneR_config), "tuneR")
  if (!dir.exists(tuneR_dir)) { dir.create(tuneR_dir) }
  
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
      bestpoint_csv = read.csv(lastfile, stringsAsFactors = FALSE, sep = "|")
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
      as.formula(paste(xnam[i], "~ (", factors_enabled$Name[i], 
                       if (factors_enabled$Value[i] > 0) {"-"} else {"+"},
                       abs(factors_enabled$Value[i]), ") /", factors_enabled$Range[i])))
    design = rsm::bbd(nfact, n0 = 1, randomize = FALSE, coding = coding, block = FALSE)
    
    # run optimization runs -----------------------------------------------------
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
    
    write.table(signif(runs, 12), file = file.path(tuneR_dir, resultdir, "runs.txt"), sep = "|",
                row.names = FALSE, col.names = TRUE, eol = "|\n")
    
    print(paste0("Repeat ", k, ", run ", 1, " to ", length(runs[,1]),  " running..."))
    
    # run experiments
    H = 20
    E = seq(1-range_E/200, 1+range_E/200, 0.005)  # energies (keV)
    
    result = foreach::foreach(i = 1:length(runs$run_no), .combine = "rbind") %dopar% {
      L = c(runs$L1[i], runs$L2[i], runs$L3[i], runs$L4[i], runs$L5[i], runs$L6[i])
      V = c(runs$V1[i], runs$V2[i], runs$V3[i], runs$V4[i], runs$V5[i], runs$V6[i])
      x1 = find_x1(L, V, H)
      tmp = tofperiod(E = E, x1 = x1, L, V, H)
      return(c(i, 1/sd(tmp), tmp))
    }
    
    result = as.data.frame(result, row.names = NA)
    names(result) = c("no", "res")
    
    # plot
    tmp = result[,!(names(result) %in% c("no", "res"))]
    plot((E-1)*100, (tmp[1,]-tmp[1,ceiling(length(E)/2)])/tmp[1,ceiling(length(E)/2)]*1e6,
         type = "l", col = rgb(0,0,0,0.3), main = paste("Repeat", k),
         ylab = expression(paste(Delta,"tof/tof") ~ "/" ~ 10^{6}),
         xlab = expression(paste(Delta,"E/E") ~ "/"~ 100))
    grid()
    for (i in 2:length(tmp[,1])) {
      lines((E-1)*100, (tmp[i,]-tmp[i,ceiling(length(E)/2)])/tmp[i,ceiling(length(E)/2)]*1e6, col = rgb(0,0,0,0.3))
    }
    
    result = result[c("no", "res")]
    
    write.table(signif(result, 12), file = file.path(tuneR_dir, resultdir, "results.txt"), sep = "|",
                row.names = FALSE, col.names = TRUE, eol = "|\n")
    
    
    
    # fit quadratic model and optimize -------------------------------------------
    
    result = result[order(result$no),]  # order
    
    design$res = result$res
    
    
    # fit second order model
    tuner_rsm_R = rsm::rsm(as.formula(paste("res ~ + SO(", paste(xnam, collapse = ","), ")")), 
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
    
    
    # run optimizer
    factors_optim = optim(rep(0, nfact), fn = desirability_overall,
                          Target = responses$Target, w = responses$Weight,
                          nfact = nfact, xnam = xnam,
                          tuner_rsm = list(tuner_rsm_R),
                          Pred_min = Pred_min_R,
                          Pred_max = Pred_max_R, method = "L-BFGS-B",
                          lower = rep(-1, nfact), upper = rep(1, nfact))$par
    
    factors_optim_coded = data.frame(t(factors_optim))
    colnames(factors_optim_coded) = xnam[1:nfact]
    bestpoint = rsm::code2val(factors_optim_coded, rsm::codings(design))
    bestpoint_predicted = data.frame(res = predict(tuner_rsm_R, factors_optim_coded))
    
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
    
    write.table(signif(bestpoint_run, 12), file = file.path(tuneR_dir, resultdir, "bestpoint_run.txt"), sep = "|",
                row.names = FALSE, col.names = TRUE, eol = "|\n")
    
    # run experiment
    bestpoint_result = data.frame(no = 1, res = NA)
    
    # ajdust to match choosen parameters
    L = c(bestpoint_run$L1, bestpoint_run$L2, bestpoint_run$L3, bestpoint_run$L4, bestpoint_run$L5, bestpoint_run$L6)
    V = c(bestpoint_run$V1, bestpoint_run$V2, bestpoint_run$V3, bestpoint_run$V4, bestpoint_run$V5, bestpoint_run$V6)
    
    x1 = find_x1(L, V, H)
    
    tmp = tofperiod(E = E, x1 = x1, L, V, H)
    bestpoint_result$res = 1/sd(tmp)
    # plot
    lines((E-1)*100, (tmp-tmp[ceiling(length(E)/2)])/tmp[ceiling(length(E)/2)]*1e6, col = rgb(1,0,0,1))
    
    
    write.table(signif(bestpoint_result, 12), file = file.path(tuneR_dir, resultdir, "bestpoint_results.txt"), sep = "|",
                row.names = FALSE, col.names = TRUE, eol = "|\n")
    
    # save all data as .RData
    save(list = ls(all.names = TRUE), 
         file = file.path(tuneR_dir, resultdir, "results.RData"))
    
    # end of loop
  }
  
}
