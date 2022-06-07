globalVariables("series_id")
globalVariables("period")
globalVariables("value")
globalVariables("year")
globalVariables("adj_value")
globalVariables("wage")

#' A Function to get inflation-adjusted wages.
#'
#' This function aims to adjust the `wage` column in `wages` and  `wages_hs_do` with the
#' inflation rate of the base year of your choice.
#' The inflation rate is calculated from the CPI data published by the US Bureau of Labor Statistics (BLS).
#' This function needs internet connection to download the data from the BLS website.
#' The code is adapted from Kris Eberwein in R Bloggers website. https://www.r-bloggers.com/2015/12/calculate-inflation-with-r/
#' @param base_year The base year of your choice (4 digits-year without quotation mark).
#'
#' @return wages and wages_hs_do data with a new column of inflation-adjusted wages value.
#'
#' @examples
#' \dontrun{
#'    inflation_adjust(1990)
#' }
#'
#' @export
adj_inflation <- function(base_year){
  filter <- dplyr::filter
  select <- dplyr::select
  summarise <- dplyr:: summarise
  as_tsibble <- tsibble::as_tsibble
  group_by_key <- tsibble::group_by_key
  index_by <- tsibble::index_by

  if (nchar(base_year) == 4){
    # Download file from BLS
    temp <- tempfile()
    download.file("http://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems",temp)
    cpi <- read.table(temp,
                        header=FALSE,
                        sep="\t",
                        skip=1,
                        stringsAsFactors=FALSE,
                        strip.white=TRUE)
    colnames(cpi)<-c("series_id", "year", "period", "value", "footnote_codes")
    unlink(temp)
    #Get rid of annual time periods and add real dates.
    cpi <- filter(cpi,series_id == "CUSR0000SA0" &
                    period !="M13" &
                    period !="S01" &
                    period!="S02" &
                    period!="S03")

    cpi$date <-as.Date(paste(cpi$year, cpi$period,"01",sep="-"),"%Y-M%m-%d")

    cpi <- select(cpi, c(series_id, date, value)) %>%
      as_tsibble(key = series_id, index = date)

    # Get average yearly CPI.
    average_cpi <- cpi %>%
      group_by_key() %>%
      index_by(year = ~ year(.)) %>%
      summarise(avg_cpi = mean(value, na.rm = TRUE))

    # Get the adjusted-inflation value
    average_cpi$adj_value <- average_cpi$avg_cpi/
      average_cpi$avg_cpi[average_cpi$year == base_year]

    # get only year and adjusted-inflation value in the result
    res <- average_cpi %>%
      select(year, adj_value)

    # join the adjusted value with wages data and calculate the inflation-adjusted wages

    wages <- dplyr::left_join(yowie::wages, res, by = "year") %>%
      dplyr::mutate(wages_inf_adj = wage*adj_value) %>%
      select(-adj_value)

    wages_hs_do <- dplyr::left_join(yowie::wages_hs_do, res, by = "year") %>%
      dplyr::mutate(wages_inf_adj = wage*adj_value) %>%
      select(-adj_value)

    return(list(wages, wages_hs_do))
  }
  else {(message(
    "The input is four digit year without quotes, e.g. 1990", appendLF = TRUE))
  }
}
