#' replaces missing observations if you have some degree of incomplete observations
#'
#' This is a useful function if you have a dataset where certain timepoints have been removed for whatever reason,
#' but you want continuous time data. This will make assumptions about trials being the same length though,
#' so may not be appropriate for all data types.
#' This should only be run after running make_pupillometry_data.
#'
#' @param data your data of class pupillometryR
#'
#' @examples
#' data(pupil_data)
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#' subject = ID,
#' trial = Trial,
#' time = Time,
#' condition = Type)
#' new_data <- replace_missing_data(data = Sdata)
#' @import dplyr
#' @import rlang
#'
#' @export
#' @return A time-stepped data frame

replace_missing_data <- function(data){

  message('replace_missing_data will only help if you have missing timepoints, and a reliable time column.')

  if('PupillometryR' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometryR. Did you forget to run make_pupillometryr_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }

  options <- attr(data, 'PupillometryR')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition
  other <- options$Other

  #add missing values
  framesize = data[[time]][2] - data[[time]][1]
  min <- min(data[[time]])
  max <- max(data[[time]])

  nid <- length(unique(data[[subject]]))
  ntr <- length(unique(data[[trial]]))
  nti <- length(unique(data[[time]]))
  tot <- nid*ntr

  Timestampkk <- rep(seq(min, max, framesize), times = tot)
  Subjectkk <- rep(unique(data[[subject]]), each = nti * ntr)

  Trialkk <- rep(unique(data[[trial]]), each = nti, times = nid)

  subdata <- data.frame(Subjectkk, Trialkk, Timestampkk)

  names(subdata)[c(1,2,3)] <- c(subject, trial, time)
  data3 <- merge(data, subdata, by = c(subject, trial, time), all.y = T, sort = F)

  #rearrange
  vars = c(subject, trial, time)
  data3 <- dplyr::arrange(data3, UQS(syms(vars)))

  class(data3) <- c(class(data))
  attr(data3, 'PupillometryR') <- options
  return(data3)
}

#' Run a filter on the data to smooth it out.
#'
#' filter_data allows three different options for filtering, a butterworth lowpass filter, a hanning filter, or
#' a median filter. You can also set the degree of this filter; we recommend a default of 11.
#' This filters on one pupil, it can be re-run on a second pupil if needed. Lowpass makes use of the
#' butterworth filter and filtfilt from package signal, median makes use of runmed.
#'
#' @param data a PupillometryR dataframe
#' @param pupil column name for pupil data
#' @param filter option for filtering the data
#' @param degree filter degree
#'
#' @examples
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#' subject = ID,
#' trial = Trial,
#' time = Time,
#' condition = Type)
#' mean_data <- calculate_mean_pupil_size(data = Sdata,
#' pupil1 = RPupil, pupil2 = LPupil)
#' filtered_data <- filter_data(data = mean_data,
#' pupil = mean_pupil,
#' filter = 'hanning',
#' degree = 11)
#' @import dplyr
#' @import rlang
#'
#' @export
#' @return filtered pupil data

filter_data <- function(data, pupil, filter = c('median', 'hanning', 'lowpass'), degree = 11){

  if('PupillometryR' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometryR. Did you forget to run make_pupillometryr_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }

  pupil <- deparse(substitute(pupil))
  options <- attr(data, 'PupillometryR')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition
  other <- options$Other

  #remove strange values
  rdata <- data
  rdata[[pupil]][rdata[[pupil]] == 0] <- NA
  rdata[[pupil]][rdata[[pupil]] < 0] <- NA
  rdata[[pupil]][rdata[[pupil]] == '.'] <- NA

  #make filtering functions
  #interpolate
  .int_data <- function(x){
    x <- zoo::na.approx(x, na.rm = F, yleft = min(x, na.rm = T) + 1, yright = max(x, na.rm = T) - 1)
    return(x)
  }
  #filtfilt2 (based on danielson's solution from stackoverflow)
  bf <- signal::butter(degree, 0.1)
  .filtfilt2 <- function(filters, x)  {
    filt <- filters$b
    a <- filters$a
    y <- signal::filter(filt, a, c(x, numeric(2 * max(length(a), length(filt)))),
                        init = rep(mean(x, na.rm = T) + 1, degree))
    y <- rev(signal::filter(filt, a, rev(y)))[seq_along(x)]
    return(y)
  }
  #lowpass

  .lowpass <- function(x){
    x <- .filtfilt2(bf, x)
    return(x)
  }
  #hanning
  hanning_filter <- function(x, n){
    i <- 0:(n - 1)
    w <- 0.5 - 0.5 * cos((2 * pi * i)/(n - 1))
    w <- w/sum(w)
    # class(w) <- 'Ma'
    y <- as.vector(stats::filter(x, w))
    y
  }
  .hanning <- function(x){
    x <- hanning_filter(x, n = degree)
    return(x)
  }
  #median
  .median <- function(x){
    x <- stats::runmed(x, degree, endrule = 'keep')
    return(x)
  }

  #reorder
  vars = c(subject, trial, time)
  rdata <- dplyr::arrange(rdata, UQS(syms(vars)))

  if(filter == 'lowpass'){
    message('Performing lowpass filter \n')
    warning('Lowpass filter is still under development - it can do some strange things at the start and end of trials. Check your data to see that it works before proceeding.')
    rdata2 <- rdata %>%
      group_by(!!sym(subject), !!sym(trial), !!sym(condition), !!sym(other)) %>%
      mutate(!!sym(pupil) := .int_data(!!sym(pupil))) %>%
      mutate(!!sym(pupil) := .lowpass(!!sym(pupil))) %>%
      ungroup()
  }

  else{
    if(filter == 'hanning'){
      message('Performing hanning filter \n')
      rdata2 <- rdata %>%
        group_by(!!sym(subject), !!sym(trial), !!sym(condition), !!sym(other)) %>%
        mutate(!!sym(pupil) := .int_data(!!sym(pupil))) %>%
        mutate(!!sym(pupil) := .hanning(!!sym(pupil))) %>%
        ungroup()
    }
    else{
      message('Performing median filter \n')
      rdata2 <- rdata %>%
        group_by(!!sym(subject), !!sym(trial), !!sym(condition), !!sym(other)) %>%
        mutate(!!sym(pupil) := .int_data(!!sym(pupil))) %>%
        mutate(!!sym(pupil) := .median(!!sym(pupil))) %>%
        ungroup()
    }
  }

  #remove the badly interpolated NAs
  rdata2[[pupil]][is.na(rdata[[pupil]])] <- NA

  #update class
  class(rdata2) <- c(class(data))
  attr(rdata2, 'PupillometryR') <- options
  return(rdata2)
}

#' Regress one pupil against another for extra smoothing
#'
#' regress_data runs a simple linear regression of pupil1 against pupil2 and the reverse.
#' This can help to account for small amount of bumpiness in the data.
#' The regression runs over each participant and each trial, per time.
#'
#' @param data a PupillometryR dataframe
#' @param pupil1 Column name for first pupil data
#' @param pupil2 Column name for second pupil data
#'
#' @examples
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#' subject = ID,
#' trial = Trial,
#' time = Time,
#' condition = Type)
#' regressed_data <- regress_data(data = Sdata,
#' pupil1 = RPupil,
#' pupil2 = LPupil)
#' mean_data <- calculate_mean_pupil_size(data = regressed_data,
#' pupil1 = RPupil, pupil2 = LPupil)
#'
#' @import dplyr
#' @import rlang
#' @importFrom stats lm median na.exclude predict qt sd
#' @importFrom utils tail
#'
#' @export
#' @return a PupillometryR dataframe with smoothed pupil values

#regress one pupil against another
regress_data <- function(data, pupil1, pupil2) {

  if('PupillometryR' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometryR. Did you forget to run make_pupillometryr_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }

  options <- attr(data, 'PupillometryR')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition
  other <- options$Other

  pupil1 <- deparse(substitute(pupil1))
  pupil2 <- deparse(substitute(pupil2))

  regdata <- data

  #make functions
  .predict_right = function(x, y){
    pupilz <- predict(lm(x ~ y, na.action = na.exclude))
    return(pupilz)
  }
  #run
  regdata2 <- regdata %>%
    group_by(!!sym(subject), !!sym(trial), !!sym(condition), !!sym(other)) %>%
    mutate(!!sym(pupil1) := ifelse(is.na(!!sym(pupil1)), !!sym(pupil2), !!sym(pupil1)),
           !!sym(pupil2) := ifelse(is.na(!!sym(pupil2)), !!sym(pupil1), !!sym(pupil2))) %>%
    mutate(pupil1newkk = .predict_right(!!sym(pupil1), !!sym(pupil2))) %>%
    mutate(pupil2newkk = .predict_right(!!sym(pupil2), !!sym(pupil1))) %>%
    mutate(!!sym(pupil1) := pupil1newkk,
           !!sym(pupil2) := pupil2newkk) %>%
    select(-pupil1newkk, -pupil2newkk) %>%
    ungroup()

  #update class
  class(regdata2) <- c(class(data))
  attr(regdata2, 'PupillometryR') <- options
  return(regdata2)
}

#' Interpolate across the gaps in data
#'
#' Once data is smoothed, it is important to deal with missing observations, such as blinks.
#' This allows simple interpolation over missing values, either linear, or cubic.
#' Depending on the analysis planed, this may not be a necessary option, but it is
#' strongly recommended for the functional analyses planned in this package.
#'
#' @param data a PupillometryR dataframe
#' @param pupil Column name for pupil data to be interpolated
#' @param type string indicating linear or cubic interpolation to be performed.
#'
#' @examples
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#' subject = ID,
#' trial = Trial,
#' time = Time,
#' condition = Type)
#' mean_data <- calculate_mean_pupil_size(data = Sdata,
#' pupil1 = RPupil, pupil2 = LPupil)
#' filtered_data <- filter_data(data = mean_data,
#' pupil = mean_pupil,
#' filter = 'hanning',
#' degree = 11)
#' int_data <- interpolate_data(data = filtered_data,
#' pupil = mean_pupil,
#' type = 'linear')
#' @import dplyr
#' @import rlang
#'
#' @export
#' @return interpolated pupillometry data

interpolate_data <- function(data, pupil, type = c('linear', 'cubic')){

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

  intdata <- data

  #make functions
  .spline <- function(x){
    x <- zoo::na.spline(x, na.rm = F)
    return(x)
  }
  .approx <- function(x){
    x <- zoo::na.approx(x, na.rm = F, rule = 2)
    return(x)
  }
  #interpolate
  if(type == 'cubic'){
    message('Performing cubic interpolation \n')
    warning('If start and end values are missing, cubic interpolation can return extreme values. Check data before continuing. If in doubt you should opt for linear interpolation.')

    intdata2 <- intdata %>%
      group_by(!!sym(subject), !!sym(trial), !!sym(condition), !!sym(other)) %>%
      mutate(!!sym(pupil) := .spline(!!sym(pupil))) %>%
      ungroup()
  }else{
    if(type =='linear'){
      message('Performing linear interpolation \n')
      intdata2 <- intdata %>%
        group_by(!!sym(subject), !!sym(trial), !!sym(condition), !!sym(other)) %>%
        mutate(!!sym(pupil) := .approx(!!sym(pupil))) %>%
        ungroup()
    }
  }
  #cleanup
  class(intdata2) <- c(class(data))
  attr(intdata2, 'PupillometryR') <- options
  return(intdata2)
}
