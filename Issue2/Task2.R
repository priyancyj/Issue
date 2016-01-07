#' Gives average of delivery time per week
#'
#' @param df: dataframe which contains all sales information
#' @return plot of average delivery time over weeks
#'
#' @examples
#' Delivery_time_weekly(sales())
Delivery_time_weekly <- function(df) {
  x <- filter(df, delivery_time < 600)
  deliver <- select(x, week, delivery_time) %>%
    group_by(week) %>%
    summarise(mean(delivery_time))
  plot(deliver, type = "o", col = "blue")
}

#' Euclidean distance
#'
#' @param long1: longitude of location 1
#' @param long2: longitude of location 2
#' @param lat1: latitude of location 1
#' @param lat2: latitude of location 2
#' @return Euclidean distance in kilometres between two points
#'
#' @example
#' dist_km(77, 12, 78, 13)
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

#' delivery
#'
#' @param
#' @return Returns a dataframe which contains all information regarding Delivery
#'
#' @examples
#' delivery()
delivery <- function() {
  drv <- DBI::dbDriver("PostgreSQL")
  con <- RPostgreSQL::dbConnect(drv, dbname="mom", user = "metar", host="localhost", password = "123$")
  on.exit(RPostgreSQL::dbDisconnect(con))
  locations <- RPostgreSQL::dbReadTable(con, c("mom", "locations"))
  vehicle_location <- RPostgreSQL::dbReadTable(con, c("mom", "vehicle_locations"))
  lat1 <- filter(vehicle_location, id == 1)$latitude
  long1 <- filter(vehicle_location, id == 1)$longitude
  delivery <- select(sales(), id, delivery_location_id, delivery_time) %>%
    filter(delivery_time < 600) %>%
    left_join(locations, by = c("delivery_location_id" = "id")) %>%
    rename(order_id = id, user_id = person_id) %>%
    mutate(distance = dist_km(long1, lat1, longitude, latitude))
  return(delivery)
}

#' Gives the analysis of delivery time over Eucledian Distance
#'
#' @param df: dataframe which contains all information regarding delivery
#' @return plot of average delivery time over Eucledian distance
#'
#' @examples
#' delivery_over_distance(delivery())
delivery_over_distance <- function(df) {
  a <- select(df, distance, delivery_time) %>%
    group_by(distance) %>%
    summarise(Delivery_time = mean(delivery_time))
  plot(a, type = "o", col = "blue")
}


