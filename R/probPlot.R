#' Makes univariate probability plots
#' @param mod the model
#' @param x is the independent variable
#' @param xlab is the x-axis label
#' @param ylab is the x-axis label
#' @param outcome the dependent variable
#' @export
probPlot <- function(mod, x, outcome, xlab, ylab){
  resp <- seq(min(x, na.rm = T), max(x, na.rm = T), by = .1)
  pred <- exp(coef(mod)[[1]] + coef(mod)[[2]]*resp) / (1 + exp(coef(mod)[[1]] + coef(mod)[[2]]*resp))
  obs_points <- aggregate(outcome, by = list(round(x)), mean, na.rm = T)
  plot(pred ~ resp, type = "l", xlab = xlab, ylab = ylab, ylim = c(min(obs_points$x, pred, na.rm = T), max(obs_points$x, pred, na.rm = T)+.1))
  points(x = obs_points$Group.1, y = obs_points$x)
}
