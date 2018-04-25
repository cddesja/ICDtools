#' Calculate dext scores 
#'
#' @param file Name of assessment scores CSV file
#' @param output Name of output file. This is optional.
#' @param time Is time the first character in the ID variable? Defaults to TRUE.   
#' @export
#'
dextScore <- function(file, output = "dext_score.csv", time = TRUE){
  file <- read.csv(file)
  flanker <- grep("Flanker", file$Inst)
  dccs <- grep("Dimensional", file$Inst)
  
  # Calculate Flanker Score 
  if(!(is.null(file$age))){
    flanker_data <- file[flanker, c("PIN", "Inst","RawScore", "Total.Dext.Raw.score", "Computed.Score", "age")]
  } else {
    flanker_data <- file[flanker, c("PIN", "Inst","RawScore", "Total.Dext.Raw.score", "Computed.Score")]
  }
  fdata <- data.frame()
  for(i in unique(flanker_data$PIN)) {
    tmp <- subset(flanker_data, PIN == i)
    summed <- colSums(tmp[,3:4], na.rm = T)
    # cat(i,"\n")
    if(!(is.null(tmp$age)) & any(tmp$age >=8)){
      summed[[1]] <- tmp$RawScore + 20
    }
    if(summed[[1]] >= 32){        
      score <- tmp$Computed.Score
    } else if(summed[[2]] == 0 & summed[[1]] > 0) {
      score <- summed[[1]] * (1 / 8)
    } else {
      score <- -5 + summed[[2]] * (1 / 6) + summed[[1]] * (1 / 8)
    }
    dat.tmp <- data.frame(id = substring(tmp$PIN[1], 2),
                          time = substring(tmp$PIN[1], 1, 1),
                          instrument = "Flanker",
                          dext = ifelse(summed[[2]] == 0 & summed[[1]] > 0, 0, 1),
                          score = score)
    if(time == FALSE){
      dat.tmp$id <- tmp$PIN[1] 
      dat.tmp$time <- NULL
    }
    fdata <- rbind(fdata, dat.tmp)
  }
  
  
  # Calcluate DCCS
  if(!(is.null(file$age))){
    dccs_data <- file[dccs, c("PIN", "Inst","RawScore", "Total.Dext.Raw.score", "Total.Dext.Raw..", "Computed.Score", "age")]
  } else {
    dccs_data <- file[dccs, c("PIN", "Inst","RawScore", "Total.Dext.Raw.score", "Total.Dext.Raw..", "Computed.Score")]
  }
  ddata <- data.frame()
  for(i in unique(dccs_data$PIN)) {
    tmp <- subset(dccs_data, PIN == i)
    summed <- colSums(tmp[,3:4], na.rm = T)
    # cat(i, "\n")
    if(!(is.null(tmp$age)) & any(tmp$age >=8)){
      summed[[1]] <- tmp$RawScore + 20
    }
    if(summed[[1]] >= 32){        
      score <- tmp$Computed.Score
    } else if(summed[[2]] == 0 & summed[[1]] > 0) {
      score <- summed[[1]] * (1 / 8)
    } else {
      score <- -5 + (summed[[2]] + summed[[1]]) * (1 / 8)
    }
    dat.tmp <- data.frame(id = substring(tmp$PIN[1], 2),
                          time = substring(tmp$PIN[1], 1, 1),
                          instrument = "DCCS",
                          dext = ifelse(summed[[2]] == 0 & summed[[1]] > 0, 0, 1),
                          score = score)
    
    if(time == FALSE){
      dat.tmp$id <- tmp$PIN[1]
      dat.tmp$time <- NULL
    }
    ddata <- rbind(ddata, dat.tmp)
  }
  tmp_data <- rbind(fdata, ddata)
  cat("Writing data to", paste(getwd(), output, sep = "/"))
  write.csv(tmp_data, file = output, row.names = FALSE)
}

