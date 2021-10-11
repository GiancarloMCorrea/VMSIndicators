#' Efficiency indicators
#'
#' Calculate nine efficiency indicators.
#'
#' @param data Dataset with required quantities obtained from the
#' preprocessing function.
#' @return A data.frame with the efficiency indicators per trip.
#' @export
get_indicators = function(data) {

  if(!(class(data$TIME)[1] == "POSIXct")) {
    data$TIME = parse_date_time(data$TIME, c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M"))
  }
  data$MONTH = format(data$TIME, format = '%m')
  data$YEAR = format(data$TIME, format = '%Y')
  data$TIME_IND = paste0(data$YEAR, data$MONTH)

  indicatorData = data %>%
                    group_by(EMB_NOMBRE, TRIP_IND, MONTH, YEAR, TIME_IND) %>%
                    summarise(IND_1 = mean(TRIP_DISTANCE), 
                              IND_2 = mean(DISTANCE_RECT_LINE),
                              IND_3 = mean(DISTANCE_RECT_LINE)/mean(TRIP_DISTANCE),
                              IND_4 = mean(LANDING), # only landings
                              IND_5 = mean(LANDING)/mean(CAPBODEGA),
                              IND_6 = mean(TRIP_TIME),
                              IND_7 = 24/mean(TRIP_TIME),
                              IND_8 = mean(LANDING)/mean(TRIP_TIME), .groups = 'drop')

  indicatorData$IND_9 = log(indicatorData$IND_3*indicatorData$IND_5*indicatorData$IND_7*indicatorData$IND_8 + 1)

  return(indicatorData)

}
