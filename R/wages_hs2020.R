#' Wages Data from National Longitudinal Survey of Youth
#'
#' A data set contains longitudinal data of mean hourly wages along
#' with several demographic variables from National Longitudinal Survey of Youth (NLSY)
#' from Round 1 (1979 survey year) to Round 28 (2018 survey year).
#' The cohort provided in this data set are those with maximum highest grade completed is
#' high school and participated at least 5 years in the survey.
#'
#' @source [The NLSY79 Database](https://www.nlsinfo.org/content/cohorts/nlsy79/get-data)
#'
#' @format A data frame with 103,994 rows and 15 variables:
#' \describe{
#'   \item{id}{A unique individual's ID number. This is the `key` of the data.}
#'   \item{year}{The year the observation was taken. This could be the `index` of the data.}
#'   \item{mean_hourly_wage}{The mean of the hourly wages the individual gets at each of their different jobs.}
#'   \item{age_1979}{The age of the subject in 1979.}
#'   \item{gender}{Gender of the subject, FEMALE and MALE.}
#'   \item{race}{Race of the subject, NON-BLACK,NON-HISPANIC; HISPANIC; BLACK.}
#'   \item{hgc}{Highest grade completed.}
#'   \item{hgc_i}{Integer of highest grade completed.}
#'   \item{yr_hgc}{The year when the highest grade completed.}
#'   \item{number_of_jobs}{Number of jobs that an individual has.}
#'   \item{total_hours}{The number of hours the individual usually works per week.}
#'   \item{is_wm}{Whether the mean hourly wage is calculated by weighted
#'                using the hour work as the weightor regular/arithmetic mean.
#'                TRUE = is weighted mean. FALSE = is regular mean.}
#'   \item{is_locf}{Whether the mean hourly wage is derived from the previous's year value
#'                (last observation carried forward) because mean hourly wage in
#'                that year is extreme (Q3 + 3xIQR or below Q1 - 3xIQR).
#'                TRUE = is locf. FALSE = is not locf.}
#'   \item{is_ext_id}{Whether the id's average of mean hourly wage is much
#'                higher that other ids (extreme). TRUE = The id is extreme.
#'                FALSE = The id is not extreme.}
#'   \item{is_ext_obs}{Whether the mean hourly wage is extreme. TRUE = The id is extreme.
#'                FALSE = The id is not extreme.}
#' }
#'
#' @examples
#' # show the data
#' wages_hs2020
#' # turn the data into a tsibble object
#' library(brolgar)
#' wages <- as_tsibble(x = wages_hs2020,
#'                     key = id,
#'                     index = year,
#'                     regular = FALSE)
#' # create the spaghetti plot of the data
#' library(ggplot2)
#' ggplot(wages) +
#'     geom_line(aes(x = year,
#'                   y = mean_hourly_wage,
#'                   group = id), alpha = 0.1)
#'

"wages_hs2020"