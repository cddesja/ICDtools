#' Rubin's Rules for Latent Variable Models
#'
#' This function applies Rubin's rules to models fit via lavaan
#'
#' @param model A user-specified lavaan model
#' @param imp.data A multiply-imputed data set of class mids
#' @param type Type of lavaan function to call? Either sem, cfa, growth, or lavaan.
#' @param standardize Logical. If TRUE, include standardize output. Default is FALSE.
#' @import lavaan
#' @export
#'
rrLavaan <- function(model, imp.data, type, standardize = FALSE) {
  if(class(imp.data) != "mids"){
    stop("data are not multiply-imputed data sets from the mice package. Please run mice.")
  }
  if(!(type %in% c("sem", "cfa", "growth", "lavaan"))){
    stop("type is not set to a valid lavaan function. Please specify either the sem, cfa, growth, or lavaan function.")
  }
  m <- imp.data$m

  # run the model
  if(type == "sem"){
    unpooled.mod <- list()
    for(i in 1:m){
      unpooled.mod[[i]] <- sem(model, complete(imp.data, i))
    }
  }

  # extract parameter estimates
  params.mod <- lapply(unpooled.mod, parameterestimates, ci = FALSE, standardized = TRUE)
  npar <- nrow(params.mod[[i]])[1]
  
  # extract the variances
  vcov.var <- matrix(nrow = npar, ncol = m)
  for(i in 1:m){
    vcov.var[,i] <- params.mod[[i]][, "se"]^2
  }
  u.bar <- rowMeans(vcov.var)
  
  unstd.params <- matrix(nrow = npar, ncol = m)
  for(i in 1:m){
    unstd.params[,i] <- params.mod[[i]][, "est"]
  }
  q.bar <-  rowMeans(unstd.params)

  b <- (1 / (m - 1)) * rowSums((unstd.params - q.bar)^2)
  T  <-  (1 + 1 / m) * b + u.bar
  v <- (m - 1) * (1 + (u.bar / ((1 + 1 / m) * b)))^2
  t <- q.bar/sqrt(T)
  p <- round(pt(q = abs(t), df = v, lower.tail = F) * 2, 3)

  if(standardize){
    std.params.lv <- matrix(nrow = npar, ncol = m)
    for(i in 1:m){
      std.params.lv[,i] <- params.mod[[i]][, "std.lv"]
    }
    std.params.all <- matrix(nrow = npar, ncol = m)
    for(i in 1:m){
      std.params.all[,i] <- params.mod[[i]][, "std.all"]
    }
    q.lv.bar <- rowMeans(std.params.lv)
    q.all.bar <- rowMeans(std.params.all)
  }

  if(standardize){
    pooled.params <- cbind(params.mod[[1]][,1:4], q.bar, t, p, q.lv.bar, q.all.bar)
    colnames(pooled.params)[5:9] <- c("Q.bar", "t", "p", "Q.lv.bar", "Q.all.bar")
  } else {
    pooled.params <- cbind(params.mod[[1]][,1:4], q.bar, t, p)
    colnames(pooled.params)[5:7] <- c("Q.bar", "t", "p")
  }
  return(pooled.params)
}

