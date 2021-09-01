#' Calculate a mean size across two pupils over time
#'
#' This function is useful when you have left and right eye eyetracking data, and a mean of the two would be useful.
#'
#' @param data a PupillometryR dataframe
#' @param pupil1 column name indicating pupil size
#' @param pupil2 column name indicating pupil size
#'
#' @examples
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#'                                subject = ID,
#'                                trial = Trial,
#'                                time = Time,
#'                                condition = Type)
#' regressed_data <- regress_data(data = Sdata, pupil1 = RPupil, pupil2 = LPupil)
#' mean_data <- calculate_mean_pupil_size(data = regressed_data, pupil1 = RPupil, pupil2 = LPupil)
#' @import dplyr
#' @import rlang
#' @export
#'
#' @return A PupillometryR dataframe with a mean pupil column

calculate_mean_pupil_size <- function(data, pupil1, pupil2){
pupil1 <- deparse(substitute(pupil1))
pupil2 <- deparse(substitute(pupil2))

if('PupillometryR' %in% class(data) == FALSE){
  stop('Dataframe is not of class PupillometryR. Did you forget to run make_pupillometryr_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
}

options <- attr(data, 'PupillometryR')
subject <- options$Subject
trial <- options$Trial
time <- options$Time
condition <- options$Condition
other <- options$Other

# ind <- which(is.na(data[[pupil1]]))
# data[[pupil2]][ind] <- data[[pupil1]][ind]
# ind <- which(is.na(data[[pupil2]]))
# data[[pupil1]][ind] <- data[[pupil2]][ind]
#
# newdata <- data
# newdata[['mean_pupil']] <- (data[[pupil1]] + data[[pupil2]])/2

newdata <- data %>%
  rowwise() %>%
  mutate(mean_pupil = mean(c(!!sym(pupil1), !!sym(pupil2)), na.rm = T)) %>%
  ungroup()


class(newdata) <- c(class(data))
attr(newdata, 'PupillometryR') <- options
return(newdata)
}
