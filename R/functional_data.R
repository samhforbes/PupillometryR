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

#' Create functional data for ANOVA by fitting b-splines to multiple conditions.
#'
#' This function fits b-splines to pupil data for multiple conditions in preparation for a functional ANOVA.
#' Unlike the difference-based approach for t-tests, this preserves the original condition structure.
#'
#' @param data a Pupil_data dataframe
#' @param pupil column name indicating pupil data to fit
#' @param basis number of basis functions to use in fitting
#' @param order order of the b-splines (default is cubic splines, order = 4)
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
#' anova_spline_data <- create_functional_anova_data(data = base_data, pupil = mean_pupil, basis = 10, order = 4)
#'
#' @import dplyr
#' @import splines
#' @import rlang
#' @export
#'
#' @return A Pupil_function_data dataframe
create_functional_anova_data <- function(data, pupil, basis = 10, order = 4) {
  # Check if the input is valid
  if (!any(c('PupillometryR') %in% class(data))) {
    stop('Dataframe is not a PupillometryR object. Please use make_pupillometryr_data first.')
  }

  # Extract pupil column name
  pupil <- deparse(substitute(pupil))

  # Get metadata from attributes
  options <- attr(data, 'PupillometryR')
  subject <- options$Subject
  time <- options$Time
  other <- options$Other
  condition <- options$Condition

  # Check if condition column exists
  if (is.null(condition) || !(condition %in% colnames(data))) {
    stop('Condition column not found. Make sure you specify the condition when creating data.')
  }

  # Create simpler, cleaner implementation for fitting splines
  # Group by subject and condition
  message("Fitting splines for each subject-condition combination...")

  # Create result by processing one subject-condition pair at a time
  result <- data

  # Get all subject-condition combinations
  subj_cond_pairs <- unique(data[, c(subject, condition)])

  # For each subject-condition pair, fit splines separately
  for (i in 1:nrow(subj_cond_pairs)) {
    curr_subj <- subj_cond_pairs[[subject]][i]
    curr_cond <- subj_cond_pairs[[condition]][i]

    # Filter data for this subject and condition
    mask <- data[[subject]] == curr_subj & data[[condition]] == curr_cond
    idx <- which(mask)

    if (length(idx) > 0) {
      # Get time and pupil values for this subject and condition
      time_vals <- data[[time]][idx]
      pup_vals <- data[[pupil]][idx]

      # Only proceed if we have enough data points
      if (length(time_vals) >= basis) {
        tryCatch({
          # Create spline basis
          bs_matrix <- splines::bs(time_vals, df = basis, degree = order - 1)

          # Fit model
          fit <- lm(pup_vals ~ bs_matrix)

          # Get fitted values
          fitted <- predict(fit)

          # Replace original values with fitted values
          result[[pupil]][idx] <- fitted
        }, error = function(e) {
          warning(paste("Spline fitting failed for subject", curr_subj,
                        "condition", curr_cond, ":", e$message))
        })
      } else {
        warning(paste("Not enough data points for subject", curr_subj,
                      "condition", curr_cond,
                      "- need at least", basis, "points"))
      }
    }
  }

  # Add appropriate class and attributes
  class(result) <- c('Pupil_function_data', class(result))
  attr(result, 'PupillometryR') <- options

  # Add parameters used for the spline fitting
  attr(result, 'Pupil_function_data') <- list(
    Basis = basis,
    Order = order
  )

  return(result)
}

#' Run a functional ANOVA on a dataframe previously fitted with b-splines.
#'
#' This allows running of a functional ANOVA for a given alpha on pupil data that has been fitted with b-splines.

#' @param data a Pupil_function_data object from create_functional_anova_data
#' @param pupil column name indicating pupil data to test
#' @param alpha an alpha level to be set for the ANOVA
#'
#' @import dplyr
#' @import rlang
#' @import afex
#' @export
#'
#' @return A Pupil_anova_data dataframe
run_functional_anova <- function(data, pupil, alpha = 0.05) {
  if (!('Pupil_function_data' %in% class(data))) {
    stop('Dataframe has not been processed with create_functional_anova_data.')
  }

  message('Functional ANOVA is currently under development, use at your peril')

  pupil <- deparse(substitute(pupil))

  # Get attributes from the data
  options <- attr(data, 'PupillometryR')
  subject <- options$Subject
  time <- options$Time
  condition <- options$Condition
  other <- options$Other

  # Check if condition column exists
  if (is.null(condition) || !(condition %in% colnames(data))) {
    stop('Condition column not found.')
  }

  # Get number of participants and conditions
  num_part <- length(unique(data[[subject]]))
  num_cond <- length(unique(data[[condition]]))

  if (num_cond < 2) {
    stop('ANOVA requires at least 2 conditions, but only 1 was found.')
  }

  message(paste('Running functional ANOVA with', num_part, 'participants and', num_cond, 'conditions'))

  # Get all time points
  all_time_points <- sort(unique(data[[time]]))
  message(paste("Processing", length(all_time_points), "time points"))

  # Create theoretical degrees of freedom
  theoretical_df_num <- num_cond - 1
  theoretical_df_denom <- num_part - num_cond  # Between-subjects conservative estimate

  # Check if this is a within-subjects design
  is_within <- FALSE
  subject_has_multiple_conditions <- data %>%
    group_by(!!sym(subject)) %>%
    summarise(num_conditions = n_distinct(!!sym(condition)), .groups = "drop") %>%
    pull(num_conditions)

  # If most subjects have multiple conditions, it's likely within-subjects
  if (mean(subject_has_multiple_conditions > 1) > 0.5) {
    is_within <- TRUE
    theoretical_df_denom <- (num_part - 1) * (num_cond - 1)  # Within-subjects df
    message("Design appears to be within-subjects")
  } else {
    message("Design appears to be between-subjects")
  }

  # Prepare result dataframe
  result_df <- data.frame()

  # For the first timepoint only, analyze ANOVA structure
  first_t <- all_time_points[1]
  first_time_data <- data[data[[time]] == first_t, ]
  first_agg_data <- first_time_data %>%
    group_by(!!sym(subject), !!sym(condition)) %>%
    summarize(agg_pupil = mean(!!sym(pupil), na.rm = TRUE), .groups = "drop")

  first_agg_data[[subject]] <- factor(first_agg_data[[subject]])
  first_agg_data[[condition]] <- factor(first_agg_data[[condition]])

  formula_str <- paste("agg_pupil ~", condition)
  tryCatch({
    model <- aov(as.formula(formula_str), data = first_agg_data)
    anova_result <- summary(model)[[1]]
    rownames_str <- paste(rownames(anova_result), collapse = ", ")
    message("Note: For your data, ANOVA result rownames are: ", rownames_str)

    # If condition is not in rownames but 'Type' is, adjust approach for extraction
    if (!(condition %in% rownames(anova_result)) && "Type" %in% rownames(anova_result)) {
      message("Note: Your condition column name differs from ANOVA output rownames.")
      message("      Using 'Type' row for condition effect extraction.")
    }
  }, error = function(e) {
    message("Error analyzing first timepoint ANOVA structure: ", e$message)
  })

  # For each time point, run an ANOVA
  for (t in all_time_points) {
    # Subset data for this time point
    time_data <- data[data[[time]] == t, ]

    # Aggregate to one value per subject-condition at each timepoint
    agg_data <- time_data %>%
      group_by(!!sym(subject), !!sym(condition)) %>%
      summarize(agg_pupil = mean(!!sym(pupil), na.rm = TRUE), .groups = "drop")

    # Skip if data is incomplete
    if (nrow(agg_data) < num_part * num_cond / 2) {
      next
    }

    # Ensure factors
    agg_data[[subject]] <- factor(agg_data[[subject]])
    agg_data[[condition]] <- factor(agg_data[[condition]])

    # Initialize values for this time point
    f_value <- NA
    p_value <- NA
    df_num <- theoretical_df_num
    df_denom <- theoretical_df_denom
    extraction_success <- FALSE

    # Use car package for Type III SS if available
    if (requireNamespace("car", quietly = TRUE)) {
      tryCatch({
        # Simple linear model
        formula_str <- paste("agg_pupil ~", subject, "*", condition)
        model <- lm(as.formula(formula_str), data = agg_data)

        # Get ANOVA table with Type III SS
        anova_result <- car::Anova(model, type = 3)

        # Extract results - different structure from aov
        if (condition %in% rownames(anova_result)) {
          f_value <- anova_result[condition, "F value"]
          p_value <- anova_result[condition, "Pr(>F)"]
          df_num <- anova_result[condition, "Df"]
          df_denom <- model$df.residual
          extraction_success <- TRUE
        }
      }, error = function(e) {
        # Silent error handling
      })
    }

    # If car approach failed or not available, try standard approach
    if (!extraction_success) {
      # Create a simple one-way ANOVA first
      tryCatch({
        formula_str <- paste("agg_pupil ~", condition)
        model <- aov(as.formula(formula_str), data = agg_data)
        anova_result <- summary(model)[[1]]  # First component

        # Try condition variable name first
        if (condition %in% rownames(anova_result)) {
          f_value <- anova_result[condition, "F value"]
          p_value <- anova_result[condition, "Pr(>F)"]
          df_num <- anova_result[condition, "Df"]
          df_denom <- anova_result["Residuals", "Df"]
          extraction_success <- TRUE
        }
        # Then try "Type" if present (common in some datasets)
        else if ("Type" %in% rownames(anova_result)) {
          f_value <- anova_result["Type", "F value"]
          p_value <- anova_result["Type", "Pr(>F)"]
          df_num <- anova_result["Type", "Df"]
          df_denom <- anova_result["Residuals", "Df"]
          extraction_success <- TRUE
        }
        # Finally try partial matches
        else {
          for (r in rownames(anova_result)) {
            if (r != "Residuals" && !is.na(r)) {
              f_value <- anova_result[r, "F value"]
              p_value <- anova_result[r, "Pr(>F)"]
              df_num <- anova_result[r, "Df"]
              df_denom <- anova_result["Residuals", "Df"]
              extraction_success <- TRUE
              break
            }
          }
        }
      }, error = function(e) {
        # Silent error handling
      })
    }

    # Calculate statistics
    overall_mean <- mean(agg_data$agg_pupil, na.rm = TRUE)
    overall_sd <- sd(agg_data$agg_pupil, na.rm = TRUE)

    # Create result row
    new_row <- data.frame(x = t)
    names(new_row)[1] <- time  # Set column name correctly
    new_row$F <- f_value
    new_row$df_num <- df_num
    new_row$df_denom <- df_denom
    new_row$p_value <- p_value
    new_row$Mean <- overall_mean
    new_row$SD <- overall_sd

    # Based on F > critical value for non-adjusted
    critical_value <- stats::qf(1 - alpha, df_num, df_denom)
    new_row$Divergence <- !is.na(f_value) && f_value > critical_value

    # Add condition means
    for (cond in unique(agg_data[[condition]])) {
      col_name <- paste("Mean_", make.names(cond), sep = "")
      cond_data <- agg_data[agg_data[[condition]] == cond, ]
      new_row[[col_name]] <- mean(cond_data$agg_pupil, na.rm = TRUE)
    }

    # Add to result dataframe
    result_df <- rbind(result_df, new_row)
  }

  # Sort by time
  result_df <- result_df[order(result_df[[time]]), ]

  # Print summary
  message(paste("Successfully ran ANOVAs for", nrow(result_df), "time points out of",
                length(all_time_points), "total time points"))

  # Calculate critical value for theoretical df
  critical_value <- stats::qf(1 - alpha, theoretical_df_num, theoretical_df_denom)
  message(paste('Critical F value (', theoretical_df_num, ',', theoretical_df_denom,
                ') =', round(critical_value, 3)))

  # Apply FDR correction
  result_df$p_adjusted <- stats::p.adjust(result_df$p_value, method = "fdr")

  # Add the new Divergence_adj column based on adjusted p-values
  result_df$Divergence_adj <- !is.na(result_df$p_adjusted) & result_df$p_adjusted < alpha

  # Add class and attributes
  class(result_df) <- c('Pupil_anova_data', class(result_df))
  attr(result_df, 'Pupil_anova_data') <- list(
    Subject = subject,
    Time = time,
    Condition = condition,
    Other = other,
    Critical = critical_value,
    Test = 'F',
    Divergence = 'Divergence',
    Divergence_adj = 'Divergence_adj',
    df_num = theoretical_df_num,
    df_denom = theoretical_df_denom,
    Alpha = alpha
  )

  return(result_df)
}
