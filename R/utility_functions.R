# cost_function ----------------------------------------------------------------
#' Cost function for optimization.
#'
#' \code{cost_function}
#'
#' @param y Factors to optimize.
#' @param rsm_output Second order model fit.
#' @param nfact Number of factor values.
#' @param xnam Formula names for \code{\link{rsm::rsm()}}.
#'
#' @keywords internal
#' @export
cost_function = function(y, rsm_output, nfact, xnam) {
  tmp = vector(mode = "character", length = nfact)
  for (i in 1:nfact) {
    tmp[[i]] = paste0(xnam[i],"=y[",i,"]")
  }
  factors_coded = eval(parse(text = paste("data.frame(",paste(tmp, collapse = ","), ")")))
  abs(predict(rsm_output, factors_coded))
}

# desirability -----------------------------------------------------------------
#' Quadratic desirability function.
#'
#' \code{desirability} is the cost function used for optimization.
#'
#' @param factors Factors (tuning variables).
#' @param Target Target value for the response.
#' @param a Parameter of the quadratic desirabilty function, defaults to a = 0.
#' @param b Parameter of the quadratic desirabilty function, defaults to b = 0.2.
#' @param c Parameter of the quadratic desirabilty function, defaults to c = 0.8.
#' @param Pred_min Minimum value which can be reached by the model within the factor ranges.
#' @param Pred_max Maximum value which can be reached by the model within the factor ranges.
#' @param nfact Number of factor values.
#' @param xnam Formula names for \code{\link{rsm::rsm()}}.
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
#' @param w Vector of weights for each the response.
#' @param nfact Number of factor values.
#' @param xnam Formula names for \code{\link{rsm::rsm()}}.
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
#'
#' @keywords internal
#' @export
plot_coeffs = function(tuner_rsm, ylab = "") {
  m = length(tuner_rsm$coeff)
  coeffs = tuner_rsm$coeff[2:m]
  stderror = summary(tuner_rsm)$coeff[2:m,2]
  confinterval = confint(tuner_rsm)[2:m,]
  ylim = 1.2*range(coeffs)
  labels = sapply(strsplit(names(coeffs), ")", fixed = TRUE), "[", 2)
  for (i in 1:nfact) {
    labels = gsub(paste("x" ,i, sep = ""), factors$Name[i], labels)
  }
  mp = barplot(coeffs, axes=FALSE, axisnames=FALSE, ylim = ylim,
               main="Coefficients", xlab="", ylab=ylab,
               col = c(rep("blue", nfact), rep("darkgreen", sum(1:(nfact-1))), rep("red", nfact)))
  axis(1, labels = labels, at = mp, cex.axis = 0.6, las = 2)
  axis(2)
  box()
  # error bars
  segments(mp, confinterval[,1], mp, confinterval[,2])
  segments(mp - 0.2, confinterval[,1], mp + 0.2, confinterval[,1])
  segments(mp - 0.2, confinterval[,2], mp + 0.2, confinterval[,2])
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
#' @param bestpoint_run Bestpoint control values.
#' @param bestpoint_result Verified bestpoint result.
#' @param bestpoint_predicted Model prediction of bestpoint result.
#' @param xylabel Axis labels.
#'
#' @keywords internal
#' @export
plot_results = function(pltly, k, n_steps, resultdir, result, runs, bestpoint_run,
                        bestpoint_result, bestpoint_predicted, xylabel) {

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
  for (i in 1:length(bestpoint_run)) {
    tmp3[[i]] = paste0(names(bestpoint_run)[i], ": ", signif(bestpoint_run[i], 12))
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
  pltly = plotly::add_markers(pltly, data = bestpoint_result, x = ~res, y = ~sens,
                      marker = list(color = colors[k], symbol = "o", size = 12),
                      name = legendtext, showlegend = FALSE,
                      text = eval(tmp4))  # best point measured
  pltly = plotly::add_markers(pltly, data = bestpoint_predicted, x = ~res, y = ~sens,
                      marker = list(color = colors[k], symbol = "circle-open", size = 12),
                      name = legendtext, showlegend = FALSE,
                      text = eval(tmp4))  # best point predicted

  # plot
  pltly = plotly::layout(pltly, title = "", xaxis = list(title = xylabel[1]),
                 yaxis = list(title = xylabel[2]))
  print(pltly)
  return(pltly)
}
