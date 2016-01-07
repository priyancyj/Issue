#' Gives average of delivery time per week
#'
#'
#'
#'
#'
#'
Delivery_time_weekly <- function(df) {
  x <- filter(df, delivery_time < 600)
  deliver <- select(x, week, delivery_time) %>%
    group_by(week) %>%
    summarise(mean(delivery_time))
  return(deliver)
}
#' Reads table delivery
#'
#'
#'
#'
#' @examples
#' delivery(sales())
delivery <- function(df) {
  drv <- DBI::dbDriver("PostgreSQL")
  con <- RPostgreSQL::dbConnect(drv, dbname="mom", user = "metar", host="localhost", password = "123$")
  on.exit(RPostgreSQL::dbDisconnect(con))
  locations <- RPostgreSQL::dbReadTable(con, c("mom", "locations"))
  vehicle_location <- RPostgreSQL::dbReadTable(con, c("mom", "vehicle_locations"))
  start_latitude <- filter(vehicle_location, id == 1)$latitude
  start_longitude <- filter(vehicle_location, id == 1)$longitude

  select(sales(), id, delivery_location_id, delivery_time


#' Gives the dustance in kilometres of two lat/long points
#'
#' @param
#' @return
#'
#' @example
#'
dist_km <- function (long1, lat1, long2, lat2) {
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}








#' Gives the analysis of delivery time over Distance
#'
#' @param
#' @return
#'
#' @examples
#'
Delivery_over_distance
#'
