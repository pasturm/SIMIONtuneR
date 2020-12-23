# cost_function ----------------------------------------------------------------
#' Cost function for optimization.
#'
#' \code{cost_function}
#'
#' @param y Factors to optimize.
#' @param rsm_output Second order model fit.
#' @param nfact Number of factor values.
#' @param xnam Formula names for \code{\link{rsm}}.
#'
#' @keywords internal
#' @export
cost_function = function(y, rsm_output, nfact, xnam) {
  tmp = vector(mode = "character", length = nfact)
  for (i in 1:nfact) {
    tmp[[i]] = paste0(xnam[i],"=y[",i,"]")
  }
  factors_coded = eval(parse(text = paste("data.frame(",paste(tmp, collapse = ","), ")")))
  abs(stats::predict(rsm_output, factors_coded))
}

# desirability -----------------------------------------------------------------
#' Quadratic desirability function.
#'
#' \code{desirability} is the cost function used for optimization.
#'
#' @param factors Factors (tuning variables).
#' @param Target Target value for the response.
#' @param a Parameter of the quadratic desirability function, defaults to a = 0.
#' @param b Parameter of the quadratic desirability function, defaults to b = 0.2.
#' @param c Parameter of the quadratic desirability function, defaults to c = 0.8.
#' @param Pred_min Minimum value which can be reached by the model within the factor ranges.
#' @param Pred_max Maximum value which can be reached by the model within the factor ranges.
#' @param nfact Number of factor values.
#' @param xnam Formula names for \code{\link{rsm}}.
#' @param tuner_rsm Second order model fit.
#'
#' @keywords internal
#' @export
desirability = function(factors, Target, w, a, b, c, nfact, xnam, tuner_rsm,
                        Pred_min, Pred_max) {

  y = cost_function(factors, tuner_rsm, nfact, xnam)

  if (y < Target) {
    P = Pred_min
  } else {
    P = Pred_max
  }

  Target = min(Pred_max, max(Pred_min, Target))

  X = (Target - y)/(Target - P)

  desirability = a + b*X + c*X^2 - 1

  if (is.na(desirability)) { desirability = 0 }  # e.g. when Target = P

  return(desirability)
}

# desirability_overall ---------------------------------------------------------
#' Overall desirability function.
#'
#' \code{desirability_overall} is the cost function for optimization with more
#' than one response variable. It is the weighted mean of the individual
#' desirability functions.
#'
#' @param factors Factors (tuning variables).
#' @param Target Vector of the target values for each response.
#' @param w Vector of weights for each response.
#' @param nfact Number of factor values.
#' @param xnam Formula names for \code{\link{rsm}}.
#' @param tuner_rsm Second order model fit.
#' @param Pred_min Vector of minimum values which can be reached by the model
#' within the factor ranges for each response variable.
#' @param Pred_max Vector of maximum value which can be reached by the model
#' within the factor ranges for each response variable.
#'
#' @keywords internal
#' @export
desirability_overall = function(factors, Target, w, nfact, xnam, tuner_rsm,
                                Pred_min, Pred_max) {

  desir = numeric(length(w))
  for (i in 1:length(w)) {
    desir[i] = desirability(factors = factors, Target = Target[i],
                            w = w[i], a = 0, b = 0.2, c = 0.8, nfact = nfact,
                            xnam = xnam, tuner_rsm = tuner_rsm[[i]],
                            Pred_min = Pred_min[i], Pred_max = Pred_max[i])
  }

  desirability_overall = sum(w*desir)/sum(w)

  return(desirability_overall)
}

# plot_coeffs ------------------------------------------------------------------
#' Plots the fit coefficients of the response surface model.
#'
#' \code{plot_coeffs} generates a plot with the fit coefficients of the response
#' surface model with 95% confidence interval.
#'
#' @param tuner_rsm Second order model fit.
#' @param ylab y-axis label.
#' @param factors Factors.
#'
#' @keywords internal
#' @export
plot_coeffs = function(tuner_rsm, ylab = "", factors) {
  m = length(tuner_rsm$coeff)
  coeffs = tuner_rsm$coeff[2:m]
  stderror = summary(tuner_rsm)$coeff[2:m,2]
  confinterval = stats::confint(tuner_rsm)[2:m,]
  ylim = 1.2*range(coeffs)
  labels = sapply(strsplit(names(coeffs), ")", fixed = TRUE), "[", 2)
  nfact = length(factors$Name)
  for (i in 1:nfact) {
    labels = gsub(paste("x" ,i, sep = ""), factors$Name[i], labels)
  }
  mp = graphics::barplot(coeffs, axes=FALSE, axisnames=FALSE, ylim = ylim,
               main="Coefficients", xlab="", ylab=ylab,
               col = c(rep("blue", nfact), rep("darkgreen", sum(1:(nfact-1))), rep("red", nfact)))
  graphics::axis(1, labels = labels, at = mp, cex.axis = 0.6, las = 2)
  graphics::axis(2)
  graphics::box()
  # error bars
  graphics::segments(mp, confinterval[,1], mp, confinterval[,2])
  graphics::segments(mp - 0.2, confinterval[,1], mp + 0.2, confinterval[,1])
  graphics::segments(mp - 0.2, confinterval[,2], mp + 0.2, confinterval[,2])
}

# plot_results -----------------------------------------------------------------
#' Plots results.
#'
#' \code{plot_results} plots results.
#'
#' @param pltly Plotly object.
#' @param k Step number.
#' @param n_steps Total number of steps.
#' @param resultdir Results directory (used for legend text).
#' @param result Results (Resolving power and sensitivity).
#' @param runs Box-Behnken design data.
#' @param run_optimized Optimized bestpoint control values.
#' @param result_optimized Verified optimized bestpoint result.
#' @param bestpoint_predicted Model prediction of bestpoint result.
#' @param bestpoint_selected Selected bestpoint.
#' @param xylabel Axis labels.
#'
#' @keywords internal
#' @export
plot_results = function(pltly, k, n_steps, resultdir, result, runs, run_optimized,
                        result_optimized, bestpoint_predicted, bestpoint_selected,
                        xylabel) {

  colors = gplots::rich.colors(n_steps)

  legendtext = resultdir

  len = length(result[,1])
  
  # generate marker text strings
  tmp1 = list()
  for (i in 1:length(runs)) {
    tmp1[[i]] = paste0(names(runs)[i], ": ", signif(runs[,i], 12))
  }
  tmp2 = vector(mode = "character", length = length(runs[,1]))
  for (i in 1:length(runs[,1])) {
    tmp2[i] = paste0(lapply(tmp1, "[", i), collapse = "<br>")
  }
  tmp3 = list()
  for (i in 1:length(run_optimized)) {
    tmp3[[i]] = paste0(names(run_optimized)[i], ": ", signif(run_optimized[i], 12))
  }
  tmp4 = paste0(lapply(tmp3, "[", 1), collapse = "<br>")
  # add markers
  pltly = plotly::add_markers(pltly, data = result[1:(len-1),], x = ~res, y = ~sens,
                      marker = list(color = colors[k], symbol = "x"),
                      name = legendtext,
                      text = eval(tmp2[1:(len-1)]))
  pltly = plotly::add_markers(pltly, data = result[len,], x = ~res, y = ~sens,
                      marker = list(color = colors[k], symbol = "triangle-up"),
                      name = legendtext, showlegend = FALSE,
                      text = eval(tmp2[len]))  # center point
  pltly = plotly::add_markers(pltly, data = result_optimized, x = ~res, y = ~sens,
                      marker = list(color = colors[k], symbol = "o", size = 12),
                      name = legendtext, showlegend = FALSE,
                      text = eval(tmp4))  # best point measured
  pltly = plotly::add_markers(pltly, data = bestpoint_predicted, x = ~res, y = ~sens,
                      marker = list(color = colors[k], symbol = "circle-open", size = 12),
                      name = legendtext, showlegend = FALSE,
                      text = eval(tmp4))  # best point predicted
  pltly = plotly::add_markers(pltly, data = bestpoint_selected, x = ~res, y = ~sens,
                              marker = list(color = colors[k], symbol = "star", size = 12),
                              name = legendtext, showlegend = FALSE,
                              text = eval(tmp4))  # best point selected

  # plot
  pltly = plotly::layout(pltly, title = "", xaxis = list(title = xylabel[1]),
                 yaxis = list(title = xylabel[2]))
  print(pltly)
  return(pltly)
}

# desirability_meas ---------------------------------------------------------
#' Overall desirability function for the measured responses.
#'
#' \code{desirability_meas} is the overall desirability function for the 
#' measured responses.
#' 
#' Note that the desirability of the measured responses also depends on the
#' rsm model (via Pred_min and Pred_max).
#'
#' @param y Array of the measured responses (typically resolution or sensitivity).
#' @param Target Vector of the target values for each response.
#' @param w Vector of weights for each response.
#' @param Pred_min Vector of minimum values which can be reached by the model
#' within the factor ranges for each response variable.
#' @param Pred_max Vector of maximum value which can be reached by the model
#' within the factor ranges for each response variable.
#'
#' @keywords internal
#' @export
desirability_meas = function(y, Target, w, Pred_min, Pred_max) {
  
  desir = numeric(length(w))
  desirability = numeric(dim(y)[1])
  for (j in 1:length(desirability)) {
    
    for (i in 1:length(w)) {
      
      if (y[j,i] < Target[i]) {
        P = Pred_min[i]
      } else {
        P = Pred_max[i]
      }
      
      X = (Target[i] - y[j,i])/(Target[i] - P)
      
      desir[i] = 0.2*X + 0.8*X^2 - 1
      
      if (is.na(desir[i])) { desir[i] = 0 }  # e.g. when Target = P
    }
    
    desirability[j] = sum(w*desir)/sum(w)
  }
  
  return(desirability)
}
