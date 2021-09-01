#' Create a difference data frame when dealing with a condition column with 2 levels
#'
#' The difference data frame is used when creating a dataframe to do the functional t-test analysis.
#' This function would be the first step in that analysis, after doing the pre-processing.
#' It creates a frame where it treats the condition data as level2 - level1.
#' It will throw an error if there are more than two conditions.
#'
#' @param data a PupillometryR dataframe
#' @param pupil column name for pupil data
#'
#' @examples
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#'                                subject = ID,
#'                                trial = Trial,
#'                                time = Time,
#'                                condition = Type)
#' mean_data <- calculate_mean_pupil_size(data = Sdata,
#' pupil1 = RPupil, pupil2 = LPupil)
#' base_data <- baseline_data(data = mean_data, pupil = mean_pupil, start = 0, stop = 100)
#' differences <- create_difference_data(data = base_data, pupil = mean_pupil)
#' plot(differences, pupil = mean_pupil, geom = 'line')
#' @import dplyr
#' @import rlang
#' @export
#'
#' @return A Pupil_difference_data data frame

create_difference_data <- function(data, pupil){

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

  #unique <- as.vector(unique(data[[condition]]))
  unique <- levels(as.factor(data[[condition]]))

  if(length(unique) > 2){
    stop('More than 2 conditions exist, unable to create meaningful differences between more than 2 objects.')
  }

  message(paste(unique[2], 'minus', unique[1], ' -- relevel condition if this is not the intended outcome '), sep = ' ')

  # var <- paste('mean2(', pupil, ')', sep ='')
  #
  # criteria <- lazyeval::interp(~column == value, .values = list(column = as.name(condition), value = unique[1]))
  #
  # data1 <- data %>%
  #   filter_(criteria) %>%
  #   group_by_(subject, time) %>%
  #   summarize_(Pupil = var) %>%
  #   ungroup()
  #
  # criteria <- lazyeval::interp(~column == value, .values = list(column = as.name(condition), value = unique[2]))
  # data2 <- data %>%
  #   filter_(criteria) %>%
  #   group_by_(subject, time) %>%
  #   summarize_(Pupil = var) %>%
  #   ungroup()
  #
  # data2[['Pupil']] <- data2[['Pupil']] - data1[['Pupil']]

  lev2 <- unique[2]
  lev1 <- unique[1]

  data2 <- data %>%
    group_by(!!sym(subject), !!sym(time), !!sym(condition)) %>%
    summarise(!!sym(pupil) := mean(!!sym(pupil), na.rm = T)) %>%
    tidyr::pivot_wider(names_from = !!sym(condition), values_from = !!sym(pupil)) %>%
    mutate(!!sym(pupil) := !!sym(lev2) - !!sym(lev1)) %>%
    select(-!!sym(lev2), -!!sym(lev1)) %>%
    ungroup()

 # colnames(data2)[colnames(data2) == 'Pupil'] <- pupil

  vars = c(subject,time)
  data2 <- dplyr::arrange(data2, !!!syms(vars))

  #class
  class(data2) <- c('Pupil_difference_data', class(data))

  attr(data2, 'Pupil_difference_data') <- list(Subject = subject,
                                      Trial = trial,
                                      Time = time,
                                      Other = other)

  return(data2)
}

#' Makes a functional data with splines from a Pupil_difference_data dataframe.
#'
#' This function turns difference data into fitted splines in order to carry out functional data analysis.
#' Under the hood this passes basis and order to fda::Data2fd, and fda::create.bspline.basis, and is
#' mandatory before running run_functional_t_test. It is recommended to read the documentation for
#' package fda for further information.
#'
#' @param data a Pupil_difference_data dataframe
#' @param pupil Column name indicating pupil data to fit
#' @param basis Integer specifying number of basis functions to create a b-spline basis
#' @param order Integer specifying order of b-splines (one higher than the degree)
#'
#' @examples
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#'                                subject = ID,
#'                                trial = Trial,
#'                                time = Time,
#'                                condition = Type)
#' regressed_data <- regress_data(data = Sdata, pupil1 = RPupil, pupil2 = LPupil)
#' mean_data <- calculate_mean_pupil_size(data = regressed_data, pupil1 = RPupil, pupil2 = LPupil)
#' base_data <- baseline_data(data = mean_data, pupil = mean_pupil, start = 0, stop = 100)
#' differences <- create_difference_data(data = base_data, pupil = mean_pupil)
#' spline_data <- create_functional_data(data = differences, pupil = mean_pupil, basis = 10, order = 4)
#'
#' @seealso fda package
#'
#' @import dplyr
#' @import rlang
#' @export
#'
#' @return A Pupil_difference_data dataframe fitted with b-splines.

create_functional_data <- function(data, pupil, basis, order){

  if('Pupil_difference_data' %in% class(data) == FALSE){
    stop('Dataframe is not of class Pupil_difference_data. Did you forget to run create_difference_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }

  pupil <- deparse(substitute(pupil))
  options <- attr(data, 'Pupil_difference_data')
  subject <- options$Subject
  trial <- options$Trial
  other <- options$Other
  time <- options$Time


  data1 <- dplyr::select(data, !!sym(subject), !!sym(time), !!sym(pupil))
  data2 <- tidyr::spread(data1, key = subject, value = pupil)
  Times <- data2[,1]
  data2[1] <- NULL

  is.nan.data.frame <- function(x){
    do.call(cbind, lapply(x, is.nan))}

  if(length(which(is.nan(data2))) > 0){
    message('Dataframe contains NaNs; replacing with 0')
  }

  data2[is.nan(data2)] <- 0
  length <- nrow(data2)
  data3 <- as.matrix(data2)

  basis <- fda::create.bspline.basis(c(1,length), basis, order)

  fd_data_1_1 <- fda::Data2fd(1:length, data3, basis)

  coefs <- data.frame(fd_data_1_1[["coefs"]])
  x <- fd_data_1_1$coefs
  y <- fd_data_1_1[["fdnames"]][["time"]]

  fdobj <- fd_data_1_1
  Lfdobj <- 0

  fdmat <- fda::eval.fd(y, fdobj, Lfdobj)

  fdmat <- data.frame(fdmat)

  fdmat2 <- tidyr::gather(fdmat, .)

  fdobj <- cbind(Times, fdmat2)

  names(fdobj)[3] <- pupil
  data[[pupil]] <- fdobj[[pupil]]

  #class
  class(data) <- c(class(data), 'Pupil_function_data')

  attr(data, 'Pupil_difference_data') <- options
  return(data)
}

#' Run a functional t-test on a dataframe previously fitted with b-splines.
#'
#' This allows running of a functional t-test for a given alpha on pupil data that has been fitted with b-splines.
#' This is only appropriate for functional difference data, as it assumes we are dealing with condition A - condition B.
#'
#' @param data a Pupil_difference_data fitted with b-splines
#' @param pupil column name indicating pupil data to test
#' @param alpha an alpha level to be set for the t-test
#'
#' @examples
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#'                                subject = ID,
#'                                trial = Trial,
#'                                time = Time,
#'                                condition = Type)
#' regressed_data <- regress_data(data = Sdata, pupil1 = RPupil, pupil2 = LPupil)
#' mean_data <- calculate_mean_pupil_size(data = regressed_data, pupil1 = RPupil, pupil2 = LPupil)
#' base_data <- baseline_data(data = mean_data, pupil = mean_pupil, start = 0, stop = 100)
#' differences <- create_difference_data(data = base_data, pupil = mean_pupil)
#' spline_data <- create_functional_data(data = differences, pupil = mean_pupil, basis = 10, order = 4)
#' ft_data <- run_functional_t_test(data = spline_data, pupil = mean_pupil, alpha = 0.05)
#'
#' @import dplyr
#' @import rlang
#' @export
#'
#' @return A Pupil_test_data dataframe

run_functional_t_test <- function(data, pupil, alpha = 0.05){

  if('Pupil_function_data' %in% class(data) == FALSE){
    stop('Dataframe has not been assessed using create_functional_data. Make sure to run this step first, then proceed.')
  }

  pupil <- deparse(substitute(pupil))
  options <- attr(data, 'Pupil_difference_data')
  subject <- options$Subject
  time <- options$Time
  other <- options$Other

  num_part <- length(unique(data[[subject]]))
  var <- paste('mean2(', pupil, ')', sep = '')
  var2 <- paste('sd(', pupil, ')', sep ='')

  data_summary <- data %>%
    group_by(!!sym(time)) %>%
    summarize(Mean = mean(!!sym(pupil), na.rm = T),
              SD = sd(!!sym(pupil), na.rm = T))

  data_summary[['SE']] <- data_summary[['SD']] / sqrt(num_part)

  data_summary[['t']] <- data_summary[['Mean']] / data_summary[['SE']]

  df <- num_part - 1

  critical_value <- qt(1-alpha/2, df)
  message(paste('critical value for n =', num_part, 'is', critical_value), sep = ' ')

  data_summary[['Divergence']] <- ifelse(abs(data_summary[['t']]) >= critical_value, TRUE, FALSE)

  #class
  test = 't'
  divergence = 'Divergence'
  class(data_summary) <- c('Pupil_test_data', class(data))
  attr(data_summary, 'Pupil_test_data') <- list(Subject = subject,
                                           Time = time,
                                           Other = other,
                                           Critical = critical_value,
                                           Test = test,
                                           Divergence = divergence)
  return(data_summary)

}
