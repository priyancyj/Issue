#' Returns a list of no. of users activated on a particular month
#'
#' @param month: integer value
#' @return user_id's of users that got activated in that month
#'
#' @example
#' activated_users(sales(), 3)
activated_users <- function(df, mont) {
  activation <- group_by( df, user_id) %>%
    summarise( month = min(month))
  activation$month <- as.numeric(as.factor(activation$month))
  activation <- filter(activation, month == mont) %>%
    select(user_id)
  return(activation)
}
#' Find retention of active users
#'
#' @param i: the month in which users got activated
#' @param j: the month for which retention is required
#' @return retention of active users in later month that got activated in the former month
#'
#' @example
#' retention(sales(), 1, 3)
retention <- function(df, i, j) {
  f <- rmyra::user_frequency(df, 'month')
  users <- activated_users(df, i)
  if ((i - j) > 0) return(NA)
  activity <- subset(f, f$user_id %in% users$user_id, j + 1)
  reten <- sum(is.na(activity) == FALSE)
  c <- as.integer((reten/count(users))*100)
  return(c)
}
#' Gives monthly cohort analysis of active users
#'
#' @param
#' @return
#'
#' @example
#' cohort (sales())
cohort <- function(df) {
  f <- user_frequency(df, 'month')
  n <- (ncol(f) - 1)
  x <- matrix(NA, nrow = n, ncol = n)
  #x <- apply(as.array(list(row(x), col(x))), MARGIN = c(1,2), function(x) retention( df, x[1], x[2]))
  for ( i in 1:n) {
    for (j in 1:n) {
      x[i,j] <- retention(df, i, j)
    }
  }
  cohort.table <- data.frame(cohort = c('c0', 'c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9'), M0 = x[,1], M1 = x[,2], M2 = x[,3], M3 = x[,4], M4 = x[,5], M5 = x[,6], M6 = x[,7], M7 = x[,8], M8 = x[,9], M9 = x[,10])
  cohort.chart <- melt(cohort.table, id.vars = "cohort")
  colnames(cohort.chart) <- c('cohort', 'month', 'active_users')
  blues <- colorRampPalette(c('lightblue','darkblue'))
  p <- ggplot(cohort.chart, aes(x = month, y = active_users, group = cohort, colour = cohort))
  p + geom_line(size=2, alpha=1/2) +
    geom_point(size=3, alpha=1) +
    ggtitle('Monthly Active users by Cohort')
}
