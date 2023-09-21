#' detect blinks by a change in velocity
#'
#' This allows the user to set a threshold for velocity and remove anything classed as a blink as a result
#'
#' @param data dataset of class PupillometryR
#' @param pupil column name for pupil data
#' @param threshold velocity threshold for blink detection
#' @param extend_forward number of observations to remove forward of blink
#' @param extend_back number of obervations to remove behind blink
#'
#' @examples
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#' subject = ID,
#' trial = Trial,
#' time = Time,
#' condition = Type)
#'
#' Sdata2 <- detect_blinks_by_velocity(data = Sdata,
#' pupil = LPupil,
#' threshold = 0.1,
#' extend_forward = 0,
#' extend_back = 0)
#'
#' @importFrom tidyr fill
#' @import dplyr
#'
#' @export
#' @return returns dataframe with blinks removed including forward and back, and data in blink column.

detect_blinks_by_velocity <- function(data, pupil, threshold = 0.1, extend_forward = 0, extend_back = 0){

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

  message('Creating column pupil_in_blink')

  data1 <- data %>%
    group_by(!!sym(subject), !!sym(trial)) %>%
    mutate(.diff1 = c(diff(!!sym(pupil)), NA),
           .diff2 = c(diff(!!sym(time)), NA),
           .vel = .diff1/.diff2,
           pupil_in_blink = ifelse(.vel > threshold | .vel < -threshold, 1, 0),
           pupil_in_blink = ifelse(is.na(!!sym(pupil)), 1, pupil_in_blink),
           pupil_in_blink = ifelse(is.na(pupil_in_blink), lag(pupil_in_blink), pupil_in_blink)) %>%
    ungroup()

  data2 <- data1 %>%
    mutate(!!sym(pupil) := ifelse(pupil_in_blink == 1, NA, !!sym(pupil))) %>%
    group_by(!!sym(subject), !!sym(trial)) %>%
    #mutate(.mark = ifelse(pupil_in_blink == 1, !!sym(time), NA)) %>%
    mutate(.mark = ifelse(pupil_in_blink == 1, !!sym(time), NA),
           .foward = .mark + extend_forward,
           .back = .mark - extend_back) %>%
    fill(.foward, .direction = 'down') %>%
    fill(.back, .direction = 'up') %>%
    mutate(.blink = ifelse(!!sym(time) >= .back | !!sym(time) <= .foward, 1, 0),
           .blink = ifelse(is.na(.blink), 0, .blink)) %>%
    mutate(pupil_in_blink := ifelse(.blink == 1, 1, pupil_in_blink),
           !!sym(pupil) := ifelse(pupil_in_blink == 1, NA, !!sym(pupil))) %>%
    select(-.blink, -.foward, -.back, -.mark, -.vel)

  class(data2) <- class(data)
  attr(data2, 'PupillometryR') <- options
  return(data2)
}

#' detect blinks by a change in pupil size
#'
#' This allows the user to set a threshold for pupil size and remove anything classed as a blink as a result
#'
#' @param data dataset of class PupillometryR
#' @param pupil column name for pupil data
#' @param threshold velocity threshold for blink detection
#' @param extend_forward number of observations to remove forward of blink
#' @param extend_back number of obervations to remove behind blink
#'
#' @examples
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#' subject = ID,
#' trial = Trial,
#' time = Time,
#' condition = Type)
#'
#' Sdata2 <- detect_blinks_by_size(data = Sdata,
#' pupil = LPupil,
#' threshold = 2.5,
#' extend_forward = 0,
#' extend_back = 0)
#'
#' @importFrom tidyr fill
#' @import dplyr
#'
#' @export
#' @return returns dataframe with blinks removed including forward and back, and data in blink column.

detect_blinks_by_size <- function(data, pupil, threshold = 2.5, extend_forward = 0, extend_back = 0){

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

  data1 <- data %>%
    group_by(!!sym(subject), !!sym(trial)) %>%
    mutate(.min = mean(!!sym(pupil), na.rm = T) - (threshold * sd(!!sym(pupil), na.rm = T)),
           .max = mean(!!sym(pupil), na.rm = T) + (threshold * sd(!!sym(pupil), na.rm = T))) %>%
    ungroup() %>%
    mutate(pupil_in_blink = ifelse(!!sym(pupil) > .max | !!sym(pupil) < .min, 1, 0),
           pupil_in_blink = ifelse(is.na(!!sym(pupil)), 1, pupil_in_blink))

  message('Creating column pupil_in_blink')

  data2 <- data1 %>%
    mutate(!!sym(pupil) := ifelse(pupil_in_blink == 1, NA, !!sym(pupil))) %>%
    group_by(!!sym(subject), !!sym(trial)) %>%
    #mutate(.mark = ifelse(pupil_in_blink == 1, !!sym(time), NA)) %>%
    mutate(.mark = ifelse(pupil_in_blink == 1, !!sym(time), NA),
           .foward = .mark + extend_forward,
           .back = .mark - extend_back) %>%
    fill(.foward, .direction = 'down') %>%
    fill(.back, .direction = 'up') %>%
    mutate(.blink = ifelse(!!sym(time) >= .back | !!sym(time) <= .foward, 1, 0),
           .blink = ifelse(is.na(.blink), 0, .blink)) %>%
    mutate(pupil_in_blink := ifelse(.blink == 1, 1, pupil_in_blink),
           !!sym(pupil) := ifelse(pupil_in_blink == 1, NA, !!sym(pupil))) %>%
    select(-.blink, -.foward, -.back, -.mark, -.min, -.max)

  class(data2) <- class(data)
  attr(data2, 'PupillometryR') <- options
  return(data2)
}

#' detect blinks by a pre-existing labelled blink column that comes from the eyetracker
#'
#' This allows the user to remove anything classed as a blink as a result of eyetracker output.
#'
#' @param data dataset of class PupillometryR
#' @param pupil column name for pupil data
#' @param column column that refers to blinks
#' @param extend_forward number of observations to remove forward of blink
#' @param extend_back number of obervations to remove behind blink
#' @param .tag the variable in the blink column that represents a blink
#'
#' @examples \dontrun{
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#' subject = ID,
#' trial = Trial,
#' time = Time,
#' condition = Type)
#'
#' Sdata2 <- detect_blinks_by_column(data = Sdata,
#' pupil = LPupil,
#' column = data_in_blink,
#' extend_forward = 0,
#' extend_back = 0)
#'}
#'
#' @importFrom tidyr fill
#' @import dplyr
#'
#' @export
#' @return returns dataframe with blinks removed including forward and back, and data in blink column.

detect_blinks_by_column <- function(data, pupil, column, extend_forward = 0, extend_back = 0, .tag = 1){

  if('PupillometryR' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometryR. Did you forget to run make_pupillometryr_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }

  pupil <- deparse(substitute(pupil))
  column <- deparse(substitute(column))
  options <- attr(data, 'PupillometryR')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  condition <- options$Condition
  other <- options$Other
  # data2 <- data %>%
  #   mutate(!!sym(pupil) := ifelse(!!sym(column) == 1, NA, !!sym(pupil))) %>%
  #   nest_by(!!sym(subject), !!sym(trial)) %>%
  #   mutate(.mark = ifelse(!!sym(column) == 1, !!sym(time), NA))
  #
  #
  #  k <- data2[1032,]
  #  j =k[[3]][[1]]
  #  which(j[[column]] == 1)
  #
  #  j2 <- j %>%
  #    mutate(.mark = ifelse(!!sym(column) == 1, !!sym(time), NA),
  #           .foward = .mark + extend_forward,
  #           .back = .mark - extend_back) %>%
  #    fill(.foward, .direction = 'down') %>%
  #    fill(.back, .direction = 'up') %>%
  #   # ungroup() %>%
  #    mutate(.blink = ifelse(!!sym(time) >= .back | !!sym(time) <= .foward, 1, 0))
  #
  #  which(j2[['.blink']] == 1)

   data2 <- data %>%
     mutate(!!sym(pupil) := ifelse(!!sym(column) == .tag, NA, !!sym(pupil))) %>%
     group_by(!!sym(subject), !!sym(trial)) %>%
     mutate(.mark = ifelse(!!sym(column) == .tag, !!sym(time), NA)) %>%
     mutate(.mark = ifelse(!!sym(column) == .tag, !!sym(time), NA),
            .foward = .mark + extend_forward,
            .back = .mark - extend_back) %>%
     fill(.foward, .direction = 'down') %>%
     fill(.back, .direction = 'up') %>%
     mutate(.blink = ifelse(!!sym(time) >= .back | !!sym(time) <= .foward, 1, 0),
            .blink = ifelse(is.na(.blink), 0, .blink)) %>%
     mutate(!!sym(column) := ifelse(.blink == 1, 1, 0),
            !!sym(pupil) := ifelse(!!sym(column) == .tag, NA, !!sym(pupil))) %>%
     select(-.blink, -.foward, -.back, -.mark)

   class(data2) <- class(data)
   attr(data2, 'PupillometryR') <- options
   return(data2)
}
