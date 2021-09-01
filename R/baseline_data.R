#' Baseline pupil data to the average pupil size within a window
#'
#' This function is for use with the PupillometryR package to baseline each participant's pupil size to the
#' mean pupil size within a window.
#' This may not be necessary if you are doing purely within-subject analyses, but it is convenient for
#' comparison across subjects, and makes results more uniform.
#'
#' @param data a PupillometryR dataframe
#' @param pupil a column name denoting pupil data
#' @param start start time of baseline window
#' @param stop stop time of baseline window
#'
#' @examples
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#'                                subject = ID,
#'                                trial = Trial,
#'                                time = Time,
#'                                condition = Type)
#' regressed_data <- regress_data(data = Sdata, pupil1 = RPupil, pupil2 = LPupil)
#' mean_data <- calculate_mean_pupil_size(data = regressed_data,
#' pupil1 = RPupil, pupil2 = LPupil)
#' base_data <- baseline_data(data = mean_data, pupil = mean_pupil, start = 0, stop = 100)
#' @import dplyr
#' @import rlang
#'
#' @export
#'
#' @return A PupillometryR dataframe, with baselined pupil


baseline_data <- function(data, pupil, start, stop){

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

  data[[time]] <- as.numeric(data[[time]])

  basedata <- data

  baseline1 <- dplyr::filter(basedata, !!rlang::sym(time) >= start)
  baseline2 <- dplyr::filter(baseline1, !!rlang::sym(time) <= stop)

  values <- paste('mean(',pupil,')', sep = '')

  baseline3 <- baseline2 %>%
    group_by(!!sym(subject), !!sym(trial)) %>%
    summarise(Base = mean(!!sym(pupil), na.rm = T)) %>%
    ungroup()


  baselined <- merge(basedata, baseline3, by = c(subject, trial), all.x = T, sort = F)
  baselined[[pupil]] <- baselined[[pupil]] - baselined[['Base']]

  baselined[['Base']] <- NULL

  vars = c(subject, trial, time)
  baselined <- dplyr::arrange(baselined, !!!syms(vars))

  class(baselined) <- c(class(data))
  attr(baselined, 'PupillometryR') <- options

  return(baselined)

}
