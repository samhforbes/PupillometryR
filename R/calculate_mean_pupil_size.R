#' Calculate a mean of two pupil sizes
#'
#' This function is useful when you have left and right eye eyetracking data, and a mean of the two would be useful.
#'
#' @param data a PupillometryR dataframe
#' @param rpupil column name indicating right pupil size
#' @param lpupil column name indicating left pupil size
#'
#' @examples
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#'                                subject = ID,
#'                                trial = Trial,
#'                                time = Time,
#'                                condition = Type)
#' regressed_data <- regress_data(data = Sdata, pupil1 = RPupil, pupil2 = LPupil)
#' mean_data <- calculate_mean_pupil_size(data = regressed_data, rpupil = RPupil, lpupil = LPupil)
#'
#' @export
#'
#' @return A PupillometryR dataframe with a mean pupil column

calculate_mean_pupil_size <- function(data, rpupil, lpupil){
lpupil <- deparse(substitute(lpupil))
rpupil <- deparse(substitute(rpupil))

if('PupillometryR' %in% class(data) == FALSE){
  stop('Dataframe is not of class PupillometryR. Did you forget to run make_pupillometryr_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
}

options <- attr(data, 'PupillometryR')
subject <- options$Subject
trial <- options$Trial
time <- options$Time
condition <- options$Condition
other <- options$Other

ind <- which(is.na(lpupil))
lpupil[ind] <- rpupil[ind]
ind <- which(is.na(rpupil))
rpupil[ind] <- lpupil[ind]

mpupil <- (data[[rpupil]] + data[[lpupil]])/2
newdata <- cbind(data,mpupil)

class(newdata) <- c(class(data))
attr(newdata, 'PupillometryR') <- options
return(newdata)
}
