#' Downsample frequency to reduce number of samples and data size
#'
#' This function is useful if you were sampling at a very high frequency (eg 500Hz)
#' causing the data size to be hard to manage, and high autocorrelation.
#' Careful decisions should be made about the time bin size and appropriateness
#' of this function, with respect to the data type.
#'
#' @param data your data of class PupillometryR
#' @param pupil a column name denoting pupil size
#' @param timebin_size the size of the new timebin you wish to use
#' @param option what should be calculated in each timebin - mean or median. Defaults to mean.
#' @examples
#' data(pupil_data)
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#' subject = ID,
#' trial = Trial,
#' time = Time,
#' condition = Type)
#' new_data <- downsample_time_data(data = Sdata,
#' pupil = LPupil,
#' timebin_size = 50,
#' option = 'mean')
#' @import dplyr
#' @import rlang
#'
#' @export
#' @return A downsampled dataframe of class PupillometryR

downsample_time_data <- function(data, pupil, timebin_size, option = c('mean', 'median')){

  if('PupillometryR' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometryR. Did you forget to run make_pupillometryr_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }

  if(is.null(option)) option = 'mean'

  options <- attr(data, 'PupillometryR')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition
  other <- options$Other

  pupil <- deparse(substitute(pupil))
  data[["Timebin"]] <- floor(data[[time]] / timebin_size)

  if(option == 'median'){
    message('Calculating median pupil size in each timebin \n')
  data2 <- data %>%
    group_by(!!sym(subject), !!sym(trial), !!sym(trial), !!sym(condition), !!sym(other),
             Timebin) %>%
    summarise(!!sym(pupil) := median(!!sym(pupil))) %>%
    ungroup() %>%
    mutate(!!sym(time) := Timebin * timebin_size)

  }else{
    message('Calculating mean pupil size in each timebin \n')
    data2 <- data %>%
      group_by(!!sym(subject), !!sym(trial), !!sym(trial), !!sym(condition), !!sym(other),
               Timebin) %>%
      summarise(!!sym(pupil) := mean(!!sym(pupil))) %>%
      ungroup() %>%
      mutate(!!sym(time) := Timebin * timebin_size)
  }

  class(data2) <- c(class(data))
  attr(data2, 'PupillometryR') <- options

  return(data2)
}

#' Calculate the missing data amount
#'
#' This function can be used to assess the amount of samples that have problematic
#' data from each trial, which helps assess cleaning parameters
#'
#' @param data your data of class PupillometryR
#' @param pupil a column name denoting pupil size
#' @examples
#' data(pupil_data)
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#' subject = ID,
#' trial = Trial,
#' time = Time,
#' condition = Type)
#' new_data <- downsample_time_data(data = Sdata,
#' pupil = LPupil,
#' timebin_size = 50,
#' option = 'mean')
#' calculate_missing_data(data = new_data, pupil = LPupil)
#' @import dplyr
#' @import rlang
#'
#' @export
#' @return A summary table with number of missing samples in each trial

# Calculate problematic data
calculate_missing_data <- function(data, pupil){

  if('PupillometryR' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometryR.
         Did you forget to run make_pupillometryr_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }

  options <- attr(data, 'PupillometryR')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition
  other <- options$Other

  pupil <- deparse(substitute(pupil))

  data_trial <- data %>%
    group_by(!!sym(subject), !!sym(trial)) %>%
    mutate(Missing = ifelse(is.na(!!sym(pupil)), 1, 0)) %>%
    summarise(Missing = sum(Missing) /length(Missing)) %>%
    ungroup()

  return(data_trial)
}

#' Clean missing data above an acceptable threshold
#'
#' This function can be used to remove trials and participants
#' who do not meet the threshold for a study. Note that there are two parameters for
#' cleaning, one to remove trials above a threshold,
#' the second to remove participants who drop more than a certain amount of trials.
#'
#' @param data your data of class PupillometryR
#' @param pupil a column name denoting pupil size
#' @param trial_threshold a proportion of missing data over which a trial can be considered lost
#' @param subject_trial_threshold a proportion of missing trials over which a participant can be considered lost.
#' @examples
#' data(pupil_data)
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#' subject = ID,
#' trial = Trial,
#' time = Time,
#' condition = Type)
#' new_data <- downsample_time_data(data = Sdata,
#' pupil = LPupil,
#' timebin_size = 50,
#' option = 'mean')
#' calculate_missing_data(data = new_data, pupil = LPupil)
#' @import dplyr
#' @import rlang
#'
#' @export
#' @return A cleaned PupillometryR dataframe

clean_missing_data <- function(data, pupil, trial_threshold = 1, subject_trial_threshold = 1){

  if('PupillometryR' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometryR.
         Did you forget to run make_pupillometryr_data?
         Some tidyverse functions associated with dplyr and tidyr
         can also interfere with this functionality.')
  }

  options <- attr(data, 'PupillometryR')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition
  other <- options$Other

  pupil <- deparse(substitute(pupil))

  if(subject_trial_threshold == 1){
    if(trial_threshold > 1){
      stop('Please input trial threshold as a proportion')
    }
    data_trial <- data %>%
      group_by(!!sym(subject), !!sym(trial)) %>%
      mutate(Missing = ifelse(is.na(!!sym(pupil)), 1, 0)) %>%
      summarise(SumMissing = sum(Missing),
                PropMissing = sum(Missing)/length(Missing)) %>%
      ungroup()

    data_trial2 <- data_trial[data_trial[['PropMissing']] < trial_threshold,]
    data_bad <- data_trial[data_trial[['PropMissing']] > trial_threshold,]

    data_trial <- data_trial %>%
      mutate(Remove = ifelse(PropMissing > trial_threshold, 1, 0))

    bad_num <- length(data_bad[['PropMissing']])

    message(paste('Removing trials with a proportion missing >', trial_threshold,
                  '\n ...removed', bad_num, 'trials \n'))

    data_out <- left_join(data_trial2, data, by = c(subject, trial))
    data_out$SubjProp <- 1

    }else{ # both put in
    if(subject_trial_threshold > 1){
      stop('Please input subject trial threshold as a proportion')
    }
    if(trial_threshold > 1){
      stop('Please input trial threshold as a proportion')
    }
      data_trial <- data %>%
        group_by(!!sym(subject), !!sym(trial)) %>%
        mutate(Missing = ifelse(is.na(!!sym(pupil)), 1, 0)) %>%
        summarise(SumMissing = sum(Missing),
                  PropMissing = sum(Missing)/length(Missing)) %>%
        ungroup()

      data_trial2 <- data_trial[data_trial$PropMissing < trial_threshold,]
      data_bad <- data_trial[data_trial$PropMissing > trial_threshold,]

      bad_num <- length(data_bad[['PropMissing']])

      data_trial <- data_trial %>%
        mutate(Remove = ifelse(PropMissing > trial_threshold, 1, 0))

      message(paste('Removing trials with a proportion missing >', trial_threshold,
                    '\n ...removed', bad_num, 'trials \n'))

      # by participant
     data_part <- data_trial %>%
        group_by(!!sym(subject)) %>%
        summarise(SubjProp = sum(Remove)/length(Remove)) %>%
       ungroup()

     data_part2 <- data_part[data_part[['SubjProp']] < subject_trial_threshold,]
     data_bad2 <- data_part[data_part[['SubjProp']] > subject_trial_threshold,]

     part_num <- length(data_bad2[['SubjProp']])

     message(paste('Removing subjects with a proportion of missing trials >',
                   subject_trial_threshold,
                   '\n ...removed', part_num, 'subjects \n'))

     data_out2 <- left_join(data_trial2, data, by = c(subject, trial))
     data_out <- left_join(data_part2, data_out2, by = subject)

  }
  data_out <- data_out %>%
    select(-SubjProp, -PropMissing, -SumMissing)

  #update class
  class(data_out) <- c(class(data))
  attr(data_out, 'PupillometryR') <- options
  return(data_out)
}
