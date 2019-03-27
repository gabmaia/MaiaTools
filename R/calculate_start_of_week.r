
#' Caucula o primeiro dia da semana de qualquer data
#'
#'
#'
#' @param date Data, no formato "yyyy-mm-dd"
#'
#' @param start Dia em que a semana começar "sun" ou "mon". Defaults to "sun"
#'
#' @return Primeiro dia da semana a qual a data entrada pertece
#'
#' @author Gabriel Maia
#'
#' @import tidyverse
#'
#' @import lubridate
#'
#' @export
#'


calculate_start_of_week <-
function(date, start="sun") {
  lubridate::as_date(as.Date(date))
  week<-lubridate::week(date)
  year<-lubridate::year(date)
  date <- lubridate::ymd(paste(year, 1, 1, sep="-"))
  date2<-date
  if(weekdays(date)!="Sunday"){
  while (weekdays(date)!="Sunday"){
    date<-date+lubridate::days(1) }
  date<-date-lubridate::days(7)
  }

  date <- date+lubridate::days(7*(week-1))
  if(start=="mon") {
  date<-date+lubridate::days(1)
  }
  return(date)
}
