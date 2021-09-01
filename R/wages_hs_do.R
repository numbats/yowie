#' Wages Data of High School Dropout from the National Longitudinal Survey of Youth (NLSY79)
#'
#' A data set contains longitudinal data of mean hourly wages along with
#' several demographic variables from the National Longitudinal Survey of Youth (NLSY79)
#' held by the U.S. Bureau of Labor Statistics from Round 1 (1979 survey year)
#' to Round 28 (2018 survey year). The cohort provided in this data set is high school
#' dropouts, i.e., the male aged between 14 and 17 years and only completed either
#' 9th, 10th, 11th grade, or aged at least 19 years old when completed high school and participated
#' in at least five rounds of surveys.
#'
#' @source The U.S. Bureau of Labor Statistics. (2021, January 6). *National Longitudinal Survey of Youth 1979*. https://www.nlsinfo.org/content/cohorts/nlsy79/get-data
#'
#' @format A tsibble contains 97,087 rows and 15 variables:
#'
#' #' \describe{
#'   \item{id}{A unique individual's ID number. This is the `key` of the data.}
#'   \item{year}{The year the observation was taken. This could be the `index` of the data.}
#'   \item{wage}{The mean of the hourly wages the individual gets at
#'                           each of their different jobs. The value could be a
#'                           weighted or an arithmetic mean. The weighted mean is used
#'                           when the information of hours of work as the weight
#'                           is available. The mean hourly wage could also be a predicted
#'                           value if the original value is considered influential
#'                           by the robust linear regression as part of data cleaning.}
#'   \item{age_1979}{The age of the subject in 1979.}
#'   \item{gender}{Gender of the subject, FEMALE and MALE.}
#'   \item{race}{Race of the subject, NON-BLACK,NON-HISPANIC; HISPANIC; BLACK.}
#'   \item{hgc}{Highest grade completed.}
#'   \item{hgc_i}{Integer of highest grade completed.}
#'   \item{yr_hgc}{The year when the highest grade completed.}
#'   \item{njobs}{Number of jobs that an individual has.}
#'   \item{hours}{The total number of hours the individual usually works per week.}
#'   \item{is_wm}{Whether the mean hourly wage is weighted mean, using the hour work
#'                as the weight, or regular/arithmetic mean. TRUE = is weighted mean.
#'                FALSE = is regular mean.}
#'   \item{is_pred}{Whether the mean hourly wage is a predicted value or not.}
#' }
#' @examples
#' # saving the data into a new object
#' library(yowie)
#' library(tsibble)
#' wages_hs_do %>% key_data()
#' @docType data
#' @name wages_hs_do
#' @import tsibble
NULL
