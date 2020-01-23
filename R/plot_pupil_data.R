#' Pre-prepared plots of PupillometryR data
#'
#' The plot functions are designed to run with just data and pupil selections,
#' with some additional options for fun with plotting. This allows to see
#' raw data as points, grouped by either subject or condition.
#'
#' @param x A PupillometryR dataframe
#' @param pupil Column name of pupil data to be plotted
#' @param group What to group the data by (none, condition, or subject)
#' @param model Optional argument to plot agains a fitted model
#' @param ... Ignored
#'
#' @examples
#' Sdata <- make_pupillometryr_data(data = pupil_data,
#' subject = ID,
#' trial = Trial,
#' time = Time,
#' condition = Type)
#' Sdata2 <- downsample_time_data(data = Sdata,
#' pupil = LPupil,
#' timebin_size = 100,
#' option = 'median')
#' p <- plot(Sdata2, pupil = LPupil, group = 'subject')
#' p
#'
#' @import ggplot2
#' @import dplyr
#' @import rlang
#' @export
#'
#' @return A ggplot object


plot.PupillometryR <- function(x, pupil, group = c('none', 'condition', 'subject'), model = NULL, ...){

  data <- x
  if('PupillometryR' %in% class(data) == FALSE){
    stop('Dataframe is not of class PupillometryR. Did you forget to run make_pupillometryr_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }
  options <- attr(data, 'PupillometryR')
  trial <- options$Trial
  condition <- options$Condition
  subject <- options$Subject
  time <- options$Time
  pupil <- deparse(substitute(pupil))
  if(!is.null(model)){fit <- model$fitted.values}

  if(is.null(model)){
  if(group == 'condition'){
    p <- data %>% ggplot2::ggplot(
                         ggplot2::aes_string(x = time, y = pupil, colour = condition, shape = condition))
  }
  else{
    if(group == 'subject'){
      p <- data %>% ggplot2::ggplot(
                         ggplot2::aes_string(x = time, y = pupil, group = subject))
    }
    else{
      p <- data %>% ggplot2::ggplot(
                           ggplot2::aes_string(x = time, y = pupil))
    }
  }

  #Add plot layers
    q <- p + ggplot2::stat_summary(geom = 'point', fun.y = 'mean', size = 1, inherit.aes = T) +
      ggplot2::ylab('Pupil Size') +
      ggplot2::xlab('Time') +
      ggplot2::theme(legend.position = c(0.85, 0.85))

  q
  }else{
    if('gam' %in% class(model)){
      data$fit <- fit
    if(group == 'condition'){
      p <- data %>%
        ggplot2::ggplot(
        ggplot2::aes_string(x = time, y = pupil, colour = condition, shape = condition))
    }
    else{
      if(group == 'subject'){
        p <- data %>%
          ggplot2::ggplot(
          ggplot2::aes_string(x = time, y = pupil, group = subject))
      }
      else{
        p <- data %>%
          ggplot2::ggplot(
          ggplot2::aes_string(x = time, y = pupil))
      }
    }

    #Add plot layers
    q <- p + ggplot2::stat_summary(geom = 'pointrange', fun.data = 'mean_se', size = 1, inherit.aes = T, alpha = 0.1) +
      ggplot2::stat_summary(inherit.aes = T, ggplot2::aes(y = fit), geom = 'line', fun.y = 'mean', size = 2) +
      ggplot2::ylab('Pupil Size') +
      ggplot2::xlab('Time') +
      ggplot2::theme(legend.position = c(0.85, 0.85))

    q
    }
  }
}

#' Pre-prepared plots of PupillometryR data
#'
#' The plot functions are designed to run with just data and pupil selections,
#' with some additional options for fun with plotting. To see these plots,
#' you must first use create_window_data.
#'
#' @param x A Pupil_window_data dataframe
#' @param pupil Column name of pupil data to be plotted
#' @param windows Whether you want to include time windows in the plot - logical
#' @param geom violin plots or boxplots. The newest version adds raincloud plots using Ben Marwick's flat violin plot.
#' @param ... Ignored
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
#' window <- create_window_data(data = base_data,pupil = mean_pupil)
#' p <-plot(window, pupil = mean_pupil, windows = FALSE, geom = 'boxplot')
#' p
#'
#' @import dplyr
#' @import rlang
#' @import ggplot2
#' @export
#'
#' @return A ggplot object

plot.Pupil_window_data <- function(x, pupil, windows = c(FALSE, TRUE), geom = c('raincloud', 'violin', 'boxplot'), ...){

  data <- x
  if('Pupil_window_data' %in% class(data) == FALSE){
    stop('Dataframe is not of class Pupil_window_data. Did you forget to run create_window_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }

  pupil <- deparse(substitute(pupil))
  options <- attr(data, 'Pupil_window_data')
  subject <- options$Subject
  trial <- options$Trial
  window <- options$Window
  condition <- options$Condition
  other <- options$Other

  if(windows == TRUE){
    if(is.null(window)){
      stop('This data frame has no time windows included. Try running create_time_windows and rerun.')
    }
    p <- ggplot2::ggplot(data = data,
                        ggplot2::aes_string(x = window, y = pupil, colour = condition, fill = condition))
    if(geom == 'boxplot'){
      q <- p + ggplot2::geom_boxplot(alpha = 0.2)
    }else{
      if(geom == 'violin'){
      q <- p + ggplot2::geom_violin(alpha = 0.2) +
        ggplot2::stat_summary(geom = 'pointrange', fun.data = 'mean_se', position = ggplot2::position_dodge(1))
      }else{ #raincloud
        q <- p + geom_flat_violin(position = ggplot2::position_nudge(x = .2, y = 0), alpha = .5, colour = NA) +
          ggplot2::geom_boxplot(width = .2,  outlier.shape = NA, alpha = 0.2) +
          ggplot2::geom_point(position = ggplot2::position_jitter(width = .15), size = .8, alpha = 0.8)

      }
    }
  }else{
    p <- ggplot2::ggplot(data = data,
                         ggplot2::aes_string(x = condition, y = pupil, colour = condition, fill = condition))
    if(geom == 'boxplot'){
      q <- p + ggplot2::geom_boxplot(alpha = 0.2)
    }
    else{
      if(geom == 'violin'){
      q <- p + ggplot2::geom_violin(alpha = 0.2) +
        ggplot2::stat_summary(geom = 'pointrange', fun.data = 'mean_se')
      }else{
        q <- p + geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .5, colour = NA) +
          ggplot2::geom_boxplot(width = .2,  outlier.shape = NA, alpha = 0.2) +
          ggplot2::geom_point(position = position_jitter(width = .15), size = .8, alpha = 0.8)
      }
    }
  }
  q + ggplot2::ylab('Change in Pupil Size')
}

#' Pre-prepared plots of PupillometryR data
#'
#' The plot functions are designed to run with just data and pupil selections,
#' with some additional options for fun with plotting. To see these plots,
#' you must first use create_difference_data.
#'
#' @param x A Pupil_difference_data dataframe
#' @param pupil Column name of pupil data to be plotted
#' @param geom string indicating whether made of connected points or a line
#' @param colour string indicating colour of geom, passed to ggplot2
#' @param ... Ignored
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
#' differences <- create_difference_data(data = base_data,
#' pupil = mean_pupil)
#' p <- plot(differences, pupil = mean_pupil, geom = 'line')
#' p
#' @import dplyr
#' @import rlang
#' @import ggplot2
#' @export
#'
#' @return A ggplot object

plot.Pupil_difference_data <- function(x, pupil, geom = c('point', 'line'), colour = 'black', ...){

  data <- x
  if('Pupil_difference_data' %in% class(data) == FALSE){
    stop('Dataframe is not of class Pupil_difference_data. Did you forget to run create_difference_data? Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }
  pupil <- deparse(substitute(pupil))
  options <- attr(data, 'Pupil_difference_data')
  subject <- options$Subject
  trial <- options$Trial
  time <- options$Time
  other <- options$Other

  p <- ggplot2::ggplot(data = data,
                       ggplot2::aes_string(x = time, y = pupil))

  if(geom == 'line'){
    q <- p + ggplot2::stat_summary(geom = 'line', fun.y = 'mean', size = 1, color = colour)
  }
  else{
    q <- p + ggplot2::stat_summary(geom = 'point', fun.y = 'mean', size = 1, color = colour)
  }
  q
}

#' Pre-prepared plots of PupillometryR data
#'
#' The plot functions are designed to run with just data and pupil selections,
#' with some additional options for fun with plotting. To see these plots,
#' you must first use one of the run_functional tests.
#'
#' @param x A Pupil_test_data dataframe
#' @param show_divergence logical indicating whether divergences are to be highlighted
#' @param colour string indicating colour of geom_line, passed to ggplot2
#' @param fill string indicating fill hue of divergence highlights, passed to ggplot2
#' @param ... Ignored
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
#' differences <- create_difference_data(data = base_data,
#' pupil = mean_pupil)
#' spline_data <- create_functional_data(data = differences, pupil = mean_pupil, basis = 10, order = 4)
#' ft_data <- run_functional_t_test(data = spline_data,
#' pupil = mean_pupil)
#' p <- plot(ft_data, show_divergence = TRUE, colour = 'red', fill = 'orange')
#' p
#' @import dplyr
#' @import rlang
#' @import ggplot2
#' @export
#'
#' @return A ggplot object

plot.Pupil_test_data <- function(x, show_divergence = TRUE, colour = 'black', fill = 'grey', ...){

  data <- x
  if('Pupil_test_data' %in% class(data) == FALSE){
    stop('Dataframe is not of class Pupil_test_data. Make the functional data before proceeding. Some tidyverse functions associated with dplyr and tidyr can also interfere with this functionality.')
  }

  options <- attr(data, 'Pupil_test_data')
  subject <- options$Subject
  time <- options$Time
  other <- options$Other
  t <- options$Test
  critical <- options$Critical
  divergence = options$Divergence

  if(show_divergence == T & !(TRUE %in% data[[divergence]])){
    show_divergence <- FALSE
    message('No divergence to show')
  }

  if(show_divergence == F){
    p <- ggplot2::ggplot(data = data,
                         ggplot2::aes_string(x = time, y = t))
    q <- p + ggplot2::stat_summary(geom = 'line', fun.y = 'mean', size = 1, color = colour)
  }else{

    #set up divergences
    data2 <- data

    data2[['div']] <- ifelse(data2[[divergence]] == T, 1, 0)
    data2[['inds']] <- diff(c(0, data2$div))
    start <- data2[[time]][data2$inds == 1]
    end <- data2[[time]][data2$inds == -1]
    if (length(start) > length(end)) end <- c(end, tail(data2[[time]], 1))
    rects <- data.frame(start=start, end=end, group=seq_along(start))

    min = min(data2[[t]])
    max = max(data2[[t]])

      p <- ggplot2::ggplot(data = data2,
                           ggplot2::aes_string(x = time, y = t))
      q <- p + ggplot2::stat_summary(geom = 'line', fun.y = 'mean', size = 1, colour = colour) +
        ggplot2::geom_rect(data = rects, inherit.aes = F,
                           ggplot2::aes(xmin = start, xmax = end,
                                        ymin = -100*max, ymax = 100*max,
                                        group = group),
                           colour = 'transparent', fill = fill, alpha = 0.3) +
        ggplot2::geom_hline(yintercept = critical, linetype = 'dotted') +
        ggplot2::geom_hline(yintercept = -critical, linetype = 'dotted') +
        ggplot2::coord_cartesian(ylim= c(min, max))

  }
  q

}
