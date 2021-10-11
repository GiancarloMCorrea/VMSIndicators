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
    data$TIME = lubridate::parse_date_time(data$TIME, c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M"))
  }
  data$MONTH = format(data$TIME, format = '%m')
  data$YEAR = format(data$TIME, format = '%Y')
  data$TIME_IND = paste0(data$YEAR, data$MONTH)

  indicatorData = data %>%
                    dplyr::group_by(EMB_NOMBRE, TRIP_IND, MONTH, YEAR, TIME_IND) %>%
                    dplyr::summarise(DIST = mean(TRIP_DISTANCE), 
                              DIST_RECT = mean(DISTANCE_RECT_LINE),
                              EFF_DIST = mean(DISTANCE_RECT_LINE)/mean(TRIP_DISTANCE),
                              CAPTURA = mean(LANDING), # only landings
                              EFF_CAPTURA = mean(LANDING)/mean(CAPBODEGA),
                              DURACION = mean(TRIP_TIME),
                              IND_TEMP = 24/mean(TRIP_TIME),
                              CPUE = mean(LANDING)/mean(TRIP_TIME), .groups = 'drop')

  indicatorData$EFF_VIAJE = log(indicatorData$EFF_DIST*indicatorData$EFF_CAPTURA*indicatorData$IND_TEMP*indicatorData$CPUE + 1)

  return(indicatorData)

}
