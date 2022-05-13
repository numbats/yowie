#' Demographic Data from the National Longitudinal Survey of Youth (NLSY79)
#'
#' A data set contains demographic information of the NLSY79 cohort.
#' The cohort included 12,686 respondents ages 14-22 when first interviewed in 1979.
#'
#' @source The U.S. Bureau of Labor Statistics. (2021, January 6). *National Longitudinal Survey of Youth 1979*. https://www.nlsinfo.org/content/cohorts/nlsy79/get-data
#'
#' @format A data frame contains 12,686 rows and 8 variables:
#' \describe{
#'  \item{id}{A unique individual's ID number.}
#'  \item{age_1979}{The age of the subject in 1979.}
#'  \item{sex}{Sex of the subject: f = Female and m = Male.}
#'  \item{race}{Race of the subject: NBH = Non-Black,Non-Hispanic; H = Hispanic; B = Black.}
#'  \item{hgc}{Highest grade completed.}
#'  \item{hgc_i}{Integer of highest grade completed.}
#'  \item{hgc_1979}{The highest grade completed in 1979 (integer value).}
#'  \item{ged}{Wether the respondent had a high school diploma or Graduate Equivalency Degree (GED). 1: High school diploma; 2: GED; 3: Both}
#' }
#'
#' @docType data
#' @name demog_nlsy79
NULL
