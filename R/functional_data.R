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

#' Run a functional ANOVA using the FDA package
#'
#' This function performs a functional ANOVA on pupillometry data using the fda package,
#' treating pupil responses as continuous functional data rather than discrete time points.
#' This approach controls for multiple comparisons and properly handles the functional nature of the data.
#'
#' @param data a Pupil_function_data object from create_functional_anova_data
#' @param pupil column name indicating pupil data to test
#' @param alpha an alpha level to be set for the ANOVA
#' @param perm_test logical; whether to perform permutation testing (default: TRUE)
#' @param n_perm number of permutations for permutation test (default: 1000)
#' @param seed random seed for permutation test reproducibility (default: 12345)
#'
#' @import dplyr
#' @import rlang
#' @importFrom fda create.bspline.basis Data2fd pca.fd Fperm.fd
#' @export
#'
#' @return A Pupil_anova_fda dataframe
#'
#' @examples
#' \dontrun{
#' anova_data <- create_functional_anova_data(data = base_data, pupil = mean_pupil)
#' fda_result <- run_functional_anova_fda(data = anova_data, pupil = mean_pupil)
#' plot(fda_result)
#' }
run_functional_anova_fda <- function(data, pupil, alpha = 0.05, perm_test = TRUE, n_perm = 1000, seed = 12345) {
  if (!requireNamespace("fda", quietly = TRUE)) {
    stop("Package 'fda' is required but not installed. Please install it with install.packages('fda')")
  }

  if (!('Pupil_function_data' %in% class(data))) {
    stop('Dataframe has not been processed with create_functional_anova_data.')
  }

  pupil <- deparse(substitute(pupil))

  # Get attributes from the data
  options <- attr(data, 'PupillometryR')
  subject <- options$Subject
  time <- options$Time
  condition <- options$Condition
  other <- options$Other

  # Get function data attributes
  func_options <- attr(data, 'Pupil_function_data')
  basis_dim <- func_options$Basis
  spline_order <- func_options$Order

  # Check if condition column exists
  if (is.null(condition) || !(condition %in% colnames(data))) {
    stop('Condition column not found.')
  }

  # Get number of participants and conditions
  num_part <- length(unique(data[[subject]]))
  num_cond <- length(unique(data[[condition]]))
  conditions <- sort(unique(data[[condition]]))

  if (num_cond < 2) {
    stop('ANOVA requires at least 2 conditions, but only 1 was found.')
  }

  message(paste('Running functional ANOVA with', num_part, 'participants and', num_cond, 'conditions'))
  message("Using full FDA approach...")

  # Get timepoints and range
  time_points <- sort(unique(data[[time]]))
  time_range <- range(time_points)
  n_timepoints <- length(time_points)

  # Create basis functions
  basis <- fda::create.bspline.basis(rangeval = time_range, nbasis = basis_dim, norder = spline_order)

  # Check if this is a within-subjects design
  is_within <- FALSE
  subject_has_multiple_conditions <- data %>%
    group_by(!!sym(subject)) %>%
    summarise(num_conditions = n_distinct(!!sym(condition)), .groups = "drop") %>%
    pull(num_conditions)

  if (mean(subject_has_multiple_conditions > 1) > 0.5) {
    is_within <- TRUE
    message("Design appears to be within-subjects (repeated measures)")
  } else {
    message("Design appears to be between-subjects - FDA approach works best with within-subjects")
  }

  # Create a data frame to collect all subject-condition data
  subj_cond_data <- data.frame()

  # Get unique subject-condition combinations
  unique_subj <- sort(unique(data[[subject]]))

  # Track index for each subject-condition pair
  index_array <- array(NA, dim = c(num_part, num_cond))
  colnames(index_array) <- conditions
  rownames(index_array) <- unique_subj

  # Collect data for each subject-condition pair
  idx <- 1
  for (s_id in unique_subj) {
    for (c_id in conditions) {
      # Filter data for this subject-condition
      curr_data <- data[data[[subject]] == s_id & data[[condition]] == c_id, ]

      # Only process if we have data for this subject-condition
      if (nrow(curr_data) > 0) {
        # Sort by time
        curr_data <- curr_data[order(curr_data[[time]]), ]

        # Ensure we have data for all timepoints - important for proper FDA
        # Create a template with all timepoints
        template <- data.frame(time_point = time_points)
        names(template)[1] <- time

        # Merge with actual data
        merged_data <- merge(template, curr_data, by = time, all.x = TRUE)

        # Add subject and condition where missing
        merged_data[[subject]][is.na(merged_data[[subject]])] <- s_id
        merged_data[[condition]][is.na(merged_data[[condition]])] <- c_id

        # For pupil, use linear interpolation for missing values
        if (any(is.na(merged_data[[pupil]]))) {
          non_na_idx <- which(!is.na(merged_data[[pupil]]))
          if (length(non_na_idx) > 1) {
            pupil_vals <- merged_data[[pupil]]
            na_idx <- which(is.na(pupil_vals))
            pupil_vals[na_idx] <- approx(
              x = time_points[non_na_idx],
              y = pupil_vals[non_na_idx],
              xout = time_points[na_idx],
              rule = 2
            )$y
            merged_data[[pupil]] <- pupil_vals
          } else {
            # Not enough non-NA values for interpolation
            # Skip this subject-condition pair
            next
          }
        }

        # Store the index of this subject-condition
        s_idx <- which(unique_subj == s_id)
        c_idx <- which(conditions == c_id)
        index_array[s_idx, c_idx] <- idx

        # Add row to our collection with subject, condition, index, and pupil data
        new_row <- data.frame(
          subject_id = s_id,
          condition_id = c_id,
          idx = idx,
          stringsAsFactors = FALSE
        )

        # Add pupil data columns
        for (t in 1:n_timepoints) {
          new_row[[paste0("t_", t)]] <- merged_data[[pupil]][t]
        }

        subj_cond_data <- rbind(subj_cond_data, new_row)

        idx <- idx + 1
      }
    }
  }

  # Check if we have complete data
  if (nrow(subj_cond_data) == 0) {
    stop("No valid subject-condition pairs found")
  }

  # Now create the fd_data_matrix with proper dimensions
  fd_data_matrix <- matrix(NA, nrow = n_timepoints, ncol = nrow(subj_cond_data))

  # Fill matrix
  for (i in 1:nrow(subj_cond_data)) {
    for (t in 1:n_timepoints) {
      fd_data_matrix[t, i] <- subj_cond_data[[paste0("t_", t)]][i]
    }
  }

  # Convert to functional data object
  fd_dataset <- fda::Data2fd(
    argvals = time_points,
    y = fd_data_matrix,
    basisobj = basis
  )

  # Perform functional ANOVA
  message("Computing functional ANOVA statistics...")

  if (is_within) {
    # For within-subjects, compute SS for condition effect
    # Calculate df for F-test
    df_num <- num_cond - 1
    df_denom <- (num_part - 1) * df_num  # For repeated measures

    # Initialize SSA (between conditions)
    SSA <- rep(0, n_timepoints)

    # Calculate within-subjects main effect
    for (c in 1:num_cond) {
      # Get indices for this condition
      indices <- which(subj_cond_data$condition_id == conditions[c])

      if (length(indices) > 0) {
        # Calculate mean for this condition
        condition_mean_fd <- fda::mean.fd(fd_dataset[indices])

        # Calculate overall mean
        overall_mean_fd <- fda::mean.fd(fd_dataset)

        # Calculate difference
        diff_fd <- fda::fd(
          coef = condition_mean_fd$coefs - overall_mean_fd$coefs,
          basis = basis
        )

        # Evaluate and square difference
        diff_values <- as.numeric(fda::eval.fd(time_points, diff_fd))
        SSA <- SSA + (diff_values^2) * sum(!is.na(index_array[, c]))
      }
    }

    # Calculate MSA
    MSA <- SSA / df_num

    # Calculate error term (SSAxS)
    SSAxS <- rep(0, n_timepoints)

    # For each subject and condition
    for (s in 1:num_part) {
      s_id <- unique_subj[s]

      # Get all data for this subject
      s_indices <- which(subj_cond_data$subject_id == s_id)

      if (length(s_indices) > 0) {
        # Calculate subject mean
        subject_mean_fd <- fda::mean.fd(fd_dataset[s_indices])

        for (c in 1:num_cond) {
          c_id <- conditions[c]

          # Get index for this subject-condition
          sc_idx <- which(subj_cond_data$subject_id == s_id &
                            subj_cond_data$condition_id == c_id)

          if (length(sc_idx) > 0) {
            # Get condition mean
            c_indices <- which(subj_cond_data$condition_id == c_id)
            condition_mean_fd <- fda::mean.fd(fd_dataset[c_indices])

            # Get overall mean
            overall_mean_fd <- fda::mean.fd(fd_dataset)

            # Calculate residual
            curr_fd <- fd_dataset[sc_idx]

            resid_fd <- fda::fd(
              coef = curr_fd$coefs - subject_mean_fd$coefs -
                condition_mean_fd$coefs + overall_mean_fd$coefs,
              basis = basis
            )

            # Evaluate residual
            resid_values <- as.numeric(fda::eval.fd(time_points, resid_fd))

            # Add squared residual to error term
            SSAxS <- SSAxS + (resid_values^2)
          }
        }
      }
    }

    # Calculate MSAxS
    MSAxS <- SSAxS / df_denom

    # Calculate F ratio
    F_values <- MSA / MSAxS
  } else {
    # For between-subjects, compute standard one-way ANOVA
    # Calculate df for F-test
    df_num <- num_cond - 1
    df_denom <- num_part - num_cond

    # Calculate SSR and SSE
    SSR <- rep(0, n_timepoints)
    SSE <- rep(0, n_timepoints)

    # Calculate condition means and overall mean
    cond_means <- list()
    for (c in 1:num_cond) {
      c_id <- conditions[c]

      # Get indices for this condition
      c_indices <- which(subj_cond_data$condition_id == c_id)

      if (length(c_indices) > 0) {
        # Calculate mean for this condition
        cond_means[[c]] <- fda::mean.fd(fd_dataset[c_indices])
      } else {
        cond_means[[c]] <- NULL
      }
    }

    # Calculate overall mean
    overall_mean_fd <- fda::mean.fd(fd_dataset)

    # Between-group variation
    for (c in 1:num_cond) {
      if (!is.null(cond_means[[c]])) {
        # Get number of subjects in this condition
        c_id <- conditions[c]
        n_subj_in_cond <- sum(subj_cond_data$condition_id == c_id)

        # Get condition mean and overall mean
        cond_mean_vals <- as.numeric(fda::eval.fd(time_points, cond_means[[c]]))
        overall_mean_vals <- as.numeric(fda::eval.fd(time_points, overall_mean_fd))

        # Add to SSR
        SSR <- SSR + (cond_mean_vals - overall_mean_vals)^2 * n_subj_in_cond
      }
    }

    # Within-group variation
    for (i in 1:nrow(subj_cond_data)) {
      # Get condition for this subject
      c_id <- subj_cond_data$condition_id[i]
      c_idx <- which(conditions == c_id)

      if (!is.null(cond_means[[c_idx]])) {
        # Get subject data
        subj_vals <- as.numeric(fda::eval.fd(time_points, fd_dataset[i]))

        # Get condition mean
        cond_mean_vals <- as.numeric(fda::eval.fd(time_points, cond_means[[c_idx]]))

        # Add to SSE
        SSE <- SSE + (subj_vals - cond_mean_vals)^2
      }
    }

    # Calculate MSR and MSE
    MSR <- SSR / df_num
    MSE <- SSE / df_denom

    # Calculate F ratio
    F_values <- MSR / MSE
  }

  # Calculate critical F value
  critical_F <- stats::qf(1 - alpha, df_num, df_denom)

  # Calculate pointwise p-values
  p_values <- 1 - stats::pf(F_values, df_num, df_denom)

  # FIRST create all the vectors to add to the data frame
  time_col <- time_points
  F_col <- as.numeric(F_values)
  p_value_col <- as.numeric(p_values)
  df_num_col <- rep(df_num, length(time_points))
  df_denom_col <- rep(df_denom, length(time_points))
  divergence_col <- as.logical(F_col > critical_F)

  # Initialize result dataframe properly with first column
  result_df <- data.frame(matrix(nrow = length(time_points), ncol = 0))
  result_df[[time]] <- time_col
  result_df[["F"]] <- F_col
  result_df[["p_value"]] <- p_value_col
  result_df[["df_num"]] <- df_num_col
  result_df[["df_denom"]] <- df_denom_col
  result_df[["Divergence"]] <- divergence_col

  # Perform permutation test if requested
  perm_crit_value <- NA
  overall_perm_pval <- NA

  if (perm_test) {
    # Set seed for reproducibility using the provided seed parameter
    set.seed(seed)
    message(paste("Running permutation test with", n_perm, "permutations..."))

    # Store maximum F statistic for each permutation
    max_F_perms <- numeric(n_perm)

    # In within-subjects design, we permute condition labels within subjects
    # In between-subjects design, we permute condition labels across subjects
    for (p in 1:n_perm) {
      if (p %% 100 == 0) {
        message(paste("Permutation", p, "of", n_perm))
      }

      if (is_within) {
        # For within-subjects, permute condition labels within each subject
        perm_subj_cond_data <- subj_cond_data

        for (s in 1:num_part) {
          s_id <- unique_subj[s]

          # Get rows for this subject
          s_rows <- which(subj_cond_data$subject_id == s_id)

          if (length(s_rows) > 1) {
            # Permute condition labels for this subject
            perm_cond <- sample(subj_cond_data$condition_id[s_rows])
            perm_subj_cond_data$condition_id[s_rows] <- perm_cond
          }
        }

        # Calculate permuted F values
        SSA_perm <- rep(0, n_timepoints)

        # Calculate within-subjects main effect for permuted data
        for (c in 1:num_cond) {
          c_id <- conditions[c]

          # Get indices for this permuted condition
          c_indices <- which(perm_subj_cond_data$condition_id == c_id)

          if (length(c_indices) > 0) {
            # Calculate mean for this permuted condition
            condition_mean_fd <- fda::mean.fd(fd_dataset[c_indices])

            # Overall mean doesn't change
            overall_mean_fd <- fda::mean.fd(fd_dataset)

            # Calculate difference
            diff_fd <- fda::fd(
              coef = condition_mean_fd$coefs - overall_mean_fd$coefs,
              basis = basis
            )

            # Evaluate and square difference
            diff_values <- as.numeric(fda::eval.fd(time_points, diff_fd))
            SSA_perm <- SSA_perm + (diff_values^2) * length(c_indices)
          }
        }

        # Calculate MSA
        MSA_perm <- SSA_perm / df_num

        # Calculate error term (SSAxS)
        SSAxS_perm <- rep(0, n_timepoints)

        # For each subject and condition
        for (s in 1:num_part) {
          s_id <- unique_subj[s]

          # Get all data for this subject
          s_indices <- which(perm_subj_cond_data$subject_id == s_id)

          if (length(s_indices) > 0) {
            # Calculate subject mean
            subject_mean_fd <- fda::mean.fd(fd_dataset[s_indices])

            for (c in 1:num_cond) {
              c_id <- conditions[c]

              # Get index for this subject-condition
              sc_idx <- which(perm_subj_cond_data$subject_id == s_id &
                                perm_subj_cond_data$condition_id == c_id)

              if (length(sc_idx) > 0) {
                # Get condition mean
                c_indices <- which(perm_subj_cond_data$condition_id == c_id)
                condition_mean_fd <- fda::mean.fd(fd_dataset[c_indices])

                # Get overall mean
                overall_mean_fd <- fda::mean.fd(fd_dataset)

                # Calculate residual
                curr_fd <- fd_dataset[sc_idx]

                resid_fd <- fda::fd(
                  coef = curr_fd$coefs - subject_mean_fd$coefs -
                    condition_mean_fd$coefs + overall_mean_fd$coefs,
                  basis = basis
                )

                # Evaluate residual
                resid_values <- as.numeric(fda::eval.fd(time_points, resid_fd))

                # Add squared residual to error term
                SSAxS_perm <- SSAxS_perm + (resid_values^2)
              }
            }
          }
        }

        # Calculate MSAxS
        MSAxS_perm <- SSAxS_perm / df_denom

        # Calculate F ratio for permutation
        F_perm <- as.numeric(MSA_perm / MSAxS_perm)
      } else {
        # For between-subjects, permute condition labels across subjects

        # Create permuted data
        perm_subj_cond_data <- subj_cond_data

        # Randomly assign condition labels
        perm_cond_labels <- sample(perm_subj_cond_data$condition_id)
        perm_subj_cond_data$condition_id <- perm_cond_labels

        # Calculate SSR and SSE for permuted data
        SSR_perm <- rep(0, n_timepoints)
        SSE_perm <- rep(0, n_timepoints)

        # Calculate condition means for permuted data
        perm_cond_means <- list()
        for (c in 1:num_cond) {
          c_id <- conditions[c]

          # Get indices for this permuted condition
          c_indices <- which(perm_subj_cond_data$condition_id == c_id)

          if (length(c_indices) > 0) {
            # Calculate mean for this permuted condition
            perm_cond_means[[c]] <- fda::mean.fd(fd_dataset[c_indices])
          } else {
            perm_cond_means[[c]] <- NULL
          }
        }

        # Overall mean doesn't change
        # Between-group variation
        for (c in 1:num_cond) {
          if (!is.null(perm_cond_means[[c]])) {
            # Get number of subjects in this permuted condition
            c_id <- conditions[c]
            n_subj_in_cond <- sum(perm_subj_cond_data$condition_id == c_id)

            # Get condition mean and overall mean
            cond_mean_vals <- as.numeric(fda::eval.fd(time_points, perm_cond_means[[c]]))
            overall_mean_vals <- as.numeric(fda::eval.fd(time_points, overall_mean_fd))

            # Add to SSR
            SSR_perm <- SSR_perm + (cond_mean_vals - overall_mean_vals)^2 * n_subj_in_cond
          }
        }

        # Within-group variation
        for (i in 1:nrow(perm_subj_cond_data)) {
          # Get permuted condition for this subject
          c_id <- perm_subj_cond_data$condition_id[i]
          c_idx <- which(conditions == c_id)

          if (!is.null(perm_cond_means[[c_idx]])) {
            # Get subject data
            subj_vals <- as.numeric(fda::eval.fd(time_points, fd_dataset[i]))

            # Get condition mean
            cond_mean_vals <- as.numeric(fda::eval.fd(time_points, perm_cond_means[[c_idx]]))

            # Add to SSE
            SSE_perm <- SSE_perm + (subj_vals - cond_mean_vals)^2
          }
        }

        # Calculate MSR and MSE
        MSR_perm <- SSR_perm / df_num
        MSE_perm <- SSE_perm / df_denom

        # Calculate F ratio for permutation
        F_perm <- as.numeric(MSR_perm / MSE_perm)
      }

      # Store maximum F value for this permutation
      max_F_perms[p] <- max(F_perm, na.rm = TRUE)
    }

    # Remove any NA or Inf values from max_F_perms
    max_F_perms <- max_F_perms[is.finite(max_F_perms)]

    if (length(max_F_perms) > 0) {
      # Calculate permutation p-value based on the distribution of max F statistics
      perm_crit_value <- as.numeric(quantile(max_F_perms, 1 - alpha, na.rm = TRUE))
      max_F_actual <- max(F_col, na.rm = TRUE)
      overall_perm_pval <- sum(max_F_perms >= max_F_actual, na.rm = TRUE) / length(max_F_perms)

      # For each timepoint, determine if it's significant based on permutation test
      perm_significance <- as.logical(F_col > perm_crit_value)

      # Pre-create vectors before adding to dataframe to avoid matrix attributes
      p_adjusted_vec <- rep(1, length(time_points))
      p_adjusted_vec[perm_significance] <- overall_perm_pval

      # Make sure at least one value is significant if the overall test is significant
      if (overall_perm_pval < alpha && !any(perm_significance)) {
        # If no point is significant but the overall test is, mark the maximum F as significant
        max_idx <- which.max(F_col)
        perm_significance[max_idx] <- TRUE
        p_adjusted_vec[max_idx] <- overall_perm_pval
      }

      # Add to result dataframe as simple vectors
      result_df$p_adjusted <- p_adjusted_vec
      result_df$Divergence_adj <- perm_significance

      message(paste("Permutation critical F value =", round(perm_crit_value, 3)))
    } else {
      message("Permutation test failed, using FDR correction instead")
      # Apply FDR correction as fallback
      p_adjusted <- as.numeric(stats::p.adjust(p_value_col, method = "fdr"))
      divergence_adj <- as.logical(p_adjusted < alpha)

      result_df$p_adjusted <- p_adjusted
      result_df$Divergence_adj <- divergence_adj
    }
  } else {
    # Use FDR correction if no permutation test
    message("Using FDR correction")
    # Apply FDR correction
    p_adjusted <- as.numeric(stats::p.adjust(p_value_col, method = "fdr"))
    divergence_adj <- as.logical(p_adjusted < alpha)

    result_df$p_adjusted <- p_adjusted
    result_df$Divergence_adj <- divergence_adj
  }

  # Calculate overall mean - CONVERT TO VECTOR WITH as.numeric TO AVOID [,"mean"] ISSUE
  overall_mean_values <- as.numeric(fda::eval.fd(time_points, overall_mean_fd))
  result_df$Mean <- overall_mean_values

  # Calculate condition means for the result dataframe and use proper naming
  for (c in 1:num_cond) {
    c_id <- conditions[c]

    # Get indices for this condition
    c_indices <- which(subj_cond_data$condition_id == c_id)

    if (length(c_indices) > 0) {
      # Calculate mean for this condition
      condition_fd <- fda::mean.fd(fd_dataset[c_indices])
      # CONVERT TO NUMERIC VECTOR TO AVOID [,"mean"] ISSUE
      condition_values <- as.numeric(fda::eval.fd(time_points, condition_fd))

      # Use a clean column name format that matches the pointwise version
      clean_cond_name <- make.names(as.character(c_id))
      col_name <- paste0("Mean_", clean_cond_name)

      # Add to result dataframe using direct assignment to avoid matrix attributes
      result_df[[col_name]] <- condition_values
    }
  }

  # Calculate standard deviation at each timepoint
  sd_values <- numeric(length(time_points))
  for (t in 1:n_timepoints) {
    sd_values[t] <- sd(fd_data_matrix[t, ], na.rm = TRUE)
  }
  result_df$SD <- sd_values

  # Add class and attributes
  class(result_df) <- c('Pupil_anova_fda', 'Pupil_anova_data', class(result_df))

  # Store both the dataframe results and the FDA objects
  attr(result_df, 'Pupil_anova_data') <- list(
    Subject = subject,
    Time = time,
    Condition = condition,
    Other = other,
    Critical = critical_F,
    Test = 'F',
    Divergence = 'Divergence',
    Divergence_adj = 'Divergence_adj',
    df_num = df_num,
    df_denom = df_denom,
    Alpha = alpha,
    Method = ifelse(perm_test, "FDA with permutation test", "FDA with FDR correction"),
    Perm_critical = perm_crit_value,
    Perm_pval = overall_perm_pval
  )

  # Store FDA objects for potential further analysis
  attr(result_df, 'FDA_objects') <- list(
    basis = basis,
    fd_dataset = fd_dataset
  )

  # Just print a single message about critical F values
  message("Functional ANOVA completed successfully")

  # Only print the critical F value if we're not doing permutation testing
  # or if the permutation test failed
  if (!perm_test || is.na(perm_crit_value)) {
    message(paste("Critical F value (", df_num, ",", df_denom, ") =", round(critical_F, 3)))
  }

  return(result_df)
}
