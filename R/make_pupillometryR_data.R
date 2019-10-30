#' Prepare data for pre-processing in PupillometryR
#'
#' This should be the first function you run as part of using PupillometryR.
#' This will make sure your data is in the right format for processing.
#' This package is designed to deal with data at it comes out of the eyetracker
#' in a long-form csv style format. Thus data input here would be a long
#' dataframe, wherein each row is a single frame collected by the eyetracker.
#'
#' @param data a raw, long form dataframe organised by subject, trial, and time.
#'    if your data is not long form, look at tidyr for examples of conversion.
#' @param subject column name indicating subject ID
#' @param trial column name indicating trial ID. This should be unique for participants
#' @param time column name indicating time column (should be numeric)
#' @param condition column name indicating experimental condition
#' @param other any other column you may wish to keep in the data frame for processing
#'
#' @examples
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#' subject = ID,
#' trial = Trial,
#' time = Time,
#' condition = Type)
#' @import dplyr
#' @import rlang
#'
#' @export
#'
#' @return A dataframe ready to use in PupillometryR


make_pupillometryr_data <- function(data, subject, trial, time, condition, other){

  subject <- deparse(substitute(subject))
  trial <- deparse(substitute(trial))
  time <- deparse(substitute(time))
  condition <- deparse(substitute(condition))
  other <- deparse(substitute(other))

  class(data) <- c('PupillometryR', class(data))

  attr(data, 'PupillometryR') <- list(Subject = subject,
                                            Trial = trial,
                                            Time = time,
                                            Condition = condition,
                                            Other = other)

  return(data)
}
