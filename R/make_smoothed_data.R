#' replaces missing observations if you have some degree of incomplete observations
#'
#' This is a useful function if you have a dataset where certain timepoints have been removed for whatever reason,
#' but you want continuous time data. This will make assumptions about trials being the same length though,
#' so may not be appropriate for all data types.
#' This should only be run after running make_pupillometry_data.
#'
#' @param data your data of class pupillometryR
#' @param rpupil column name for right pupil data
#' @param lpupil column name for left pupil data
#'
#' @examples
#' data(pupil_data)
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#' subject = ID,
#' trial = Trial,
#' time = Time,
#' condition = Type)
#' new_data <- replace_missing_data(data = Sdata,
#' rpupil = RPupil,
#' lpupil = LPupil)
#'
#' @export
#' @return A time-stepped data frame

replace_missing_data <- function(data, rpupil, lpupil){

  warning('replace_missing_data will only help if you have missing timepoints, and a reliable time column.')
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

  data[[lpupil]] <- as.numeric(data[[lpupil]])
  data[[rpupil]] <- as.numeric(data[[rpupil]])

  #add missing values
  framesize = data[[time]][2] - data[[time]][1]
  min <- min(data[[time]])
  max <- max(data[[time]])

  nid <- length(unique(data[[subject]]))
  ntr <- length(unique(data[[trial]]))
  nti <- length(unique(data[[time]]))
  tot <- nid*ntr

  Timestamp <- rep(seq(min, max, framesize), times = tot)
  Subject <- rep(unique(data[[subject]]), each = nti * ntr)

  Trial <- rep(unique(data[[trial]]), each = nti, times = nid)

  subdata <- data.frame(Subject, Trial, Timestamp)

  names(subdata)[c(1,2,3)] <- c(subject, trial, time)
  data3 <- merge(data, subdata, by = c(subject, trial, time), all.y = T)

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
#' filtered_data <- filter_data(data = mean_data,
#' pupil = mpupil,
#' filter = 'hanning',
#' degree = 11)
#'
#' @export
#' @return filtered pupil data

filter_data <- function(data, pupil, filter = c('lowpass', 'hanning', 'median'), degree = 11){

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

  #remove strange values
  rdata <- data
  rdata[[pupil]][rdata[[pupil]] == 0] <- NA
  rdata[[pupil]][rdata[[pupil]] < 0] <- NA

  # The old way of doing things - assumes trials are equal length, which we can't do
  # names(rdata)[c(1:4)] <- c('ID', 'Tr', 'Tim', 'Pup')
  # rdata <- data.table::dcast(rdata, Tim ~ ID + Tr, value.var = 'Pup')
  #
  # rdata2 <- rdata
  # rdata2[1] <- NULL

  rdata1 <- rdata %>% tidyr::unite(utrial, subject, trial, remove = F)
  rdata2 <- split(rdata1, rdata1$utrial)

  .int_data <- function(x){
    x[[pupil]] = zoo::na.approx(x[[pupil]], na.rm = F, rule = 2)
    return(x)
  }

  rdata3 <- lapply(rdata2, .int_data)

  if(filter == 'lowpass'){
    warning('lowpass filter is still under development - it can do some strange things at the start and end of trials. Check your data to see that it works before proceeding.')
    #make function
    .lowpass <- function(x){
      x[[pupil]] = signal::filtfilt(bf, x[[pupil]])
      return(x)
    }
    bf <- signal::butter(degree, 0.1)
    rdata4 <- lapply(rdata3, .lowpass)
  }

  else{
    if(filter == 'hanning'){
      #make filter
      hanning_filter <- function(x, n){
        i <- 0:(n - 1)
        w <- 0.5 - 0.5 * cos((2 * pi * i)/(n - 1))
        w <- w/sum(w)
        # class(w) <- 'Ma'
        y <- as.vector(stats::filter(x, w))
        y
      }
      #make function
      .hanning <- function(x){
        x[[pupil]] <- hanning_filter(x[[pupil]], n = degree)
        return(x)
      }
      rdata4 <- lapply(rdata3, .hanning)

    }
    else{#median
      #function
      .median <- function(x){
        x[[pupil]] <- stats::runmed(x[[pupil]], degree, endrule = 'keep')
        return(x)
      }
      rdata4 <- lapply(rdata3, .median)
    }
  }

  rdatasub <- rdata

  rdata33 <- unsplit(rdata4, rdata1$utrial)
  rdata33$utrial <- NULL

  #remove the badly interpolated NAs
  rdata33[[pupil]][is.na(rdatasub[[pupil]])] <- NA

  #reorder
  vars = c(subject, trial, time)
  smoothdata <- dplyr::arrange(rdata33, UQS(syms(vars)))

  #update class
  class(smoothdata) <- c(class(data))
  attr(smoothdata, 'PupillometryR') <- options
  return(smoothdata)
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
#' regressed_data <- regress_data(data = new_data,
#' pupil1 = RPupil,
#' pupil2 = LPupil)
#'
#' @export
#' @return smoothed pupil values

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

  #split for usability
  regdata1 <- regdata %>% tidyr::unite(utrial, subject, trial, remove = F)
  regdata11 <- split(regdata1, regdata1$utrial)

  .predict_right = function(x){
    x$rpmodel <- predict(lm(paste(pupil1, '~',pupil2), na.action = na.exclude, data = x))
    return(x)
  }
  .predict_left = function(x){
    x$lpmodel <- predict(lm(paste(pupil2, '~' ,pupil1), na.action = na.exclude, data = x))
    return(x)
  }

  regdata11 <- lapply(regdata11, .predict_right)
  regdata11 <- lapply(regdata11, .predict_left)

  regdata111 <- unsplit(regdata11, regdata1$utrial)
  #rearrange
  regdata111$utrial <- NULL

  regdata111[[pupil1]] <- regdata111[['rpmodel']]
  regdata111[[pupil2]] <- regdata111[['lpmodel']]

  regdata111[['rpmodel']] <- NULL
  regdata111[['lpmodel']] <- NULL

  #check arrangement
  vars = c(subject, trial, time)
  regdata2 <- dplyr::arrange(regdata111, UQS(syms(vars)))

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
#' int_data <- interpolate_data(data = filtered_data,
#' pupil = mpupil,
#' type = 'linear')
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
  #split
  intdata1 <- intdata %>% tidyr::unite(utrial, subject, trial, remove = F)
  intdata2 <- split(intdata1, intdata1$utrial)

  #interpolate
  if(type == 'linear'){
    .approx <- function(x){
      x[[pupil]] <- zoo::na.approx(x[[pupil]], na.rm = F, rule = 2)
      return(x)
    }
    pupils <- lapply(intdata2, .approx)
  }else{
    if(type =='cubic'){  #cubic
      warning('If start and end values are missing, cubic interpolation can return extreme values. Check data before continuing.')
      .spline <- function(x){
        x[[pupil]] <- zoo::na.spline(x[[pupil]], na.rm = F)
        return(x)
      }
      pupils <- lapply(intdata2, .spline)
    }
  }
  #cleanup
  pupils2 <- unsplit(pupils, intdata1$utrial)
  pupils2$utrial <- NULL

  vars = c(subject, trial, time)
  pupils3 <- dplyr::arrange(pupils2, UQS(syms(vars)))

  class(pupils3) <- c(class(data))
  attr(pupils3, 'PupillometryR') <- options
  return(pupils3)
}
