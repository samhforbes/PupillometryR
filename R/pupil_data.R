
#' Data collected in a pupillometry study by Sylvain Sirois
#'
#' Data from a simple study measuring pupil dilation as participants answer hard or easy maths problems.
#' Original data sourced and reformatted from Sylvain Sirois' Pupillometry tutorial available at https://oraprdnt.uqtr.uquebec.ca/pls/public/gscw031?owa_no_site=314&owa_no_fiche=3&owa_bottin=)
#'
#' @format A data frame with 28800 rows and 7 variables: \describe{
#' \item{ID}{Uniaue participant ID}
#'   \item{Trial}{Unique trial code (also unique for each participant)}
#'   \item{RPupil}{Right pupil size}
#'   \item{LPupil}{Left Pupil Size}
#'   \item{Timebin}{Ordered timebin within each trial}
#'   \item{Time}{Elapsed time within trial}
#'   \item{Type}{Hard or easy trial?} ... }
#' @source (https://oraprdnt.uqtr.uquebec.ca/pls/public/gscw031?owa_no_site=314&owa_no_fiche=3&owa_bottin=)
"pupil_data"
