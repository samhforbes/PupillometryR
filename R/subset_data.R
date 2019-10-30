#' Helper function mean2
#'
#' Somewhat useful function for ignoring NAs
#' @param x the object
#'

mean2 <- function(x){
  mean(x, na.rm = T)
}

#' Subset data to provide start and finish time windows
#'
#' subset_data can be used on a PupillometryR dataframe to subset the time into relevant chunks.
#' This, ideally should be one of the first runctions run, before anything analytical.
#' Use this to indicate a start and stop time to create a new resized dataframe.
#'
#' @param data a PupillometryR dataframe
#' @param start a single number indicating start time of new dataframe
#' @param stop a single number indicating end time of new dataframe
#' @param rezero logical, whether time should start from zero
#' @param remove logical, remove observations outside of start and stop
#'
#' @examples
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#'                                subject = ID,
#'                                trial = Trial,
#'                                time = Time,
#'                                condition = Type)
#' subset_data(Sdata, start = 100, stop = 10000, rezero = T, remove = T)
#'
#' @export
#'
#' @return a subsetted PupillometryR dataframe

subset_data <- function(data, start, stop, rezero = T, remove = T){

  if('PupillometryR' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometryR. Did you forget to run make_pupillometryr_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }

  options <- attr(data, 'PupillometryR')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition
  other <- options$Other
  start <- as.numeric(start)
  stop <- as.numeric(stop)

  data2 <- data

  if(start < min(data[[time]])){
    stop("Cannot rezero to less than the minimum value in time column")
  }
  if(stop > max(data[[time]])){
    stop("Cannot rezero to great than the maximum value in time column")
  }

  if(remove == T){
  data <- data[data[[time]] <= stop,]
  data <- data[data[[time]] >= start,]
  }else{
    data <- data
  }

  if(rezero == T){
    data[[time]] <- data[[time]] - min(data[[time]])
  }else{
    data = data
  }
  class(data) <- c(class(data2))
  attr(data, 'PupillometryR') <- options
  return(data)
}

#' Make PupillometryR dataframe into a single collapsed window for easy analysis
#'
#' This function creates a single collapsed data frame for easy analysis with a t-test or anova,
#' per condition.
#' By comparison create_time_windows allows dividing it into multiple windows per time.
#'
#' @param data a PupillometryR dataframe
#' @param pupil column name denoting pupil data to be used
#'
#' @examples
#' window <- create_window_data(data = base_data, pupil = mpupil)
#' p <- plot(data = window, pupil = mpupil, windows = F, geom = 'boxplot')
#' p
#'
#' @export
#'
#' @return a Pupil_window_data dataframe

create_window_data <- function(data, pupil){
  pupil <- deparse(substitute(pupil))

  if('PupillometryR' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometryR. Did you forget to run make_pupillometryr_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }

  options <- attr(data, 'PupillometryR')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition
  other <- options$Other

  var <- paste('mean2(', pupil, ')', sep ='')
  window <- group_by_(data, subject, condition) %>%
    summarize_(Pupil = var) %>%
    ungroup()

  colnames(window)[colnames(window) == 'Pupil'] <- pupil
 #update class

  class(window) <- c('Pupil_window_data', class(data))
  attr(window, 'Pupil_window_data') <- list(Subject = subject,
                                       Trial = trial,
                                       Condition = condition,
                                       Window = c(),
                                       Other = other)
  return(window)
}

#' Make PupillometryR dataframe into multiple time windows for easy analysis
#'
#' This function creates a single collapsed data frame for easy analysis with an anova or model,
#' per condition.
#' By comparison create_window_data allows collapsing all into a single time window.
#'
#' @param data a PupillometryR dataframe
#' @param pupil column name denoting pupil data to be used
#' @param breaks a vector or numbers indicating start times for each window
#'
#' @examples
#' window <- create_window_data(data = base_data, pupil = mpupil)
#' p <- plot(data = window, pupil = mpupil, windows = F, geom = 'boxplot')
#' p
#'
#' @export
#'
#' @return a Pupil_window_data dataframe

create_time_windows <- function(data, pupil, breaks){
  pupil <- deparse(substitute(pupil))

  if('PupillometryR' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometryR. Did you forget to run make_pupillometryr_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }

  options <- attr(data, 'PupillometryR')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition
  other <- options$Other

  if(length(breaks) < 1){
    stop('No breaks specified. Do you mean to use create_window_data() instead?')
  }else{
    Window <- cut(data[[time]], breaks, labels = F)
    data <- cbind(data, Window)
    data[['Window']] <- as.character(data[['Window']])
    var <- paste('mean2(', pupil, ')', sep ='')

    aggdata <- group_by_(data, subject, condition, 'Window') %>%
      summarize_(Pupil = var) %>%
      ungroup()

    colnames(aggdata)[colnames(aggdata) == 'Pupil'] <- pupil

    window <- 'Window'
    class(aggdata) <- c('Pupil_window_data', class(data))
    attr(aggdata, 'Pupil_window_data') <- list(Subject = subject,
                                         Trial = trial,
                                         Condition = condition,
                                         Window = window,
                                         Other = other)
    return(aggdata)
    }

}
