#' Demographic Data from the National Longitudinal Survey of Youth (NLSY79)
#'
#' A data set contains demographic information of the NLSY79 cohort.
#' The cohort included 12,686 respondents ages 14-22 when first interviewed in 1979.
#'
#' @source The U.S. Bureau of Labor Statistics. (2021, January 6). *National Longitudinal Survey of Youth 1979*. https://www.nlsinfo.org/content/cohorts/nlsy79/get-data
#'
#' @format A data frame contains 12,686 rows and 7 variables:
#' \describe{
#'  \item{id}{A unique individual's ID number.}
#'  \item{age_1979}{The age of the subject in 1979.}
#'  \item{gender}{Gender of the subject, FEMALE and MALE.}
#'  \item{race}{Race of the subject, NON-BLACK,NON-HISPANIC; HISPANIC; BLACK.}
#'  \item{hgc}{Highest grade completed.}
#'  \item{hgc_i}{Integer of highest grade completed.}
#'  \item{yr_hgc}{The year when the highest grade completed.}
#' }
#'
"demog_nlsy79"
