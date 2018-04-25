#' Regions of significance plot
#' Plots the regions of significance
#'
#' @param mod The fitted model
#' @param predictor The name of focal predictor variable
#' @param moderator The name of the moderator
#' @param lb The lower bound for the moderator
#' @param ub The upper bound for the moderator
#' @param xlab The label for the x-axis
#' @param ylab The label for the y-axis
#' @param plot.jn Logical. Shoud we plot the points when the interaction is significantly different than zero?
#' @export
rsPlot <- function(mod, predictor, moderator, lb, ub, xlab, ylab, plot.jn = FALSE){
  z1 = lb
  z2 = ub
  z <- seq(z1, z2, length = 1000)
  coefs <- coef(mod)
  int <- coefs[match(predictor, names(coefs))]
  slope <- coefs[grep(":", names(coefs))]
  tstat <- qt(.975, mod$df.residual)
  var.pred <- vcov(mod)[match(predictor, colnames(vcov(mod))),match(predictor, colnames(vcov(mod)))]
  cov.predint <- vcov(mod)[grep(":", colnames(vcov(mod))), match(predictor, colnames(vcov(mod)))]
  var.int <- vcov(mod)[grep(":", colnames(vcov(mod))),grep(":", colnames(vcov(mod)))]
  y.upper <- (int + slope * z) + (tstat * sqrt(var.pred + (2 * z * cov.predint) + ((z ^ 2) * var.int)))
  y.lower <- (int + slope * z) - (tstat * sqrt(var.pred + (2 * z * cov.predint) + ((z ^ 2) * var.int)))
  fy <- c(y.upper, y.lower)
  y.pred <- (int + slope * z)

  # Calculate JN significant areas
  a <- tstat^2 * var.int - slope^2
  b <- 2 * (tstat^2 * cov.predint - int * slope)
  c <- tstat^2 * var.pred - int^2
  jn <- c((-b - sqrt(b^2 - 4 * a * c))/(2*a),
          (-b + sqrt(b^2 - 4 * a * c))/(2*a))
  dat.tmp <- data.frame(z, low = y.lower,
                        upper = y.upper)

  if(plot.jn){
    plot(y.pred ~ z, type = "n", ylim = c(min(y.lower), max(y.upper)), xlab = xlab, ylab = ylab)
    abline(v = jn, col = rgb(0, 0, 1, 0.5))
    abline(h = 0, col = rgb(0, 0, 0, 0.5))
    lines(z, y.pred)
    lines(z, y.lower, lty = 2)
    lines(z, y.upper, lty = 2)
  } else {
    plot(y.pred ~ z, type = "n", ylim = c(min(y.lower), max(y.upper)), xlab = xlab, ylab = ylab)
    abline(h = 0, col = rgb(0, 0, 0, 0.5))
    lines(z, y.pred)
    lines(z, y.lower, lty = 2)
    lines(z, y.upper, lty = 2)
  }
}
