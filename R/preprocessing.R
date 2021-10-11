#' Preprocessing VMS data
#'
#' Calculates required variables to calculate the efficiency indicators and make plots.
#'
#' @param vmsdata VMS dataset.
#' @param catchdata Dataset with landings.
#' @param vesseldata Vessel information.
#' @param cutoff_dc Minimum distance to the shoreline (in nautical miles) 
#' to identify a fishing trip.
#' @param cutoff_time Maximum time interval (in hours) allowed between 
#' arrival time and landing time.
#' @param min_trip_time Minimum trip time allowed (in hours). 
#' @return The VMS database with quantities calculated.
#' @export
preprocessing = function(vmsdata, catchdata, vesseldata, cutoff_dc = 5, cutoff_time = 8, min_trip_time = 5) {

  # Create time column:
  vmsdata$TIME = paste(vmsdata$FECHA, vmsdata$HORA)
  vmsdata$TIME = parse_date_time(vmsdata$TIME, c("%d/%m/%Y %H:%M:%S", "%d/%m/%y %H:%M"))
  vmsdata$EMB_NOMBRE = gsub(pattern = '[[:space:]]', replacement = '_', x = vmsdata$NAVE)

  catchdata$TIME2 = paste(catchdata$Fecha_Arribo, catchdata$Hora_Arribo) # intentar con fecha de zarpe luego
  catchdata$TIME = parse_date_time(catchdata$TIME2, c("%m/%d/%Y %H:%M:%S"))
  catchdata$EMB_NOMBRE = gsub(pattern = '[[:space:]]', replacement = '_', x = catchdata$EMBARCACION)

  #Vessel info:
  listVessel = vesseldata$MATRICULA

  indData = list()
  list_ind = 1
  for(i in seq_along(listVessel)) {
    
    # select vessel i
    tmpData = filter(vmsdata, MATRICULA == listVessel[i])
    name_vessel = unique(tmpData$EMB_NOMBRE)[1]
    catch_tmpData = filter(catchdata, EMB_NOMBRE == name_vessel)

    if(nrow(catch_tmpData) == 0) {
      warning(paste0("No hay informacion de desembarques para la embarcacion ", name_vessel))
    }

    # order by time:
    tmpData = tmpData[order(tmpData$TIME), ]
    catch_tmpData = catch_tmpData[order(catch_tmpData$TIME), ]
    
    # find trip index
    tmpData$TRIP_IND = find_trip(ind_vec = tmpData$DIST_COAST, cutoff = cutoff_dc)
    
    # Find distances per trip:
    nTrips = max(tmpData$TRIP_IND)
    tmpData$DISTANCE_I_I1 = NA
    tmpData$TRIP_DISTANCE = NA
    tmpData$LANDING = NA
    tmpData$DISTANCE_FST_ALL = NA
    tmpData$DISTANCE_LST_ALL = NA
    tmpData$TRIP_TIME = NA
    tmpData$DIFF_IND_TRIP = NA
    tmpData$DISTANCE_RECT_LINE = NA
    tmpData$VELOCITY = NA
    tmpData$CG_LON = NA
    tmpData$CG_LAT = NA
    tmpData$CG_DC = NA
    tmpData$MIN_TIME = NA
    
    for(j in 1:nTrips) {
      
      tmpData_2 = filter(tmpData, TRIP_IND == j)
      
      # only for trip with more than 1 row:
      if(nrow(tmpData_2) > 1) {
        
        # tiempo de viaje:
        tmpData_2$TRIP_TIME = difftime(max(tmpData_2$TIME), 
                                       min(tmpData_2$TIME), units = 'hours')
        
        # find landing value:
        diffTimes = difftime(max(tmpData_2$TIME), catch_tmpData$TIME, units = 'hours')
        posSel = which(diffTimes < 0)[1]
        tmpData_2$DIFF_IND_TRIP = diffTimes[posSel] # time difference selected btw landing and vms data
        tmpData_2$LANDING = catch_tmpData$Pesca_Descargada[posSel] # hacerlo con pesca descargada
        
        lonlatPoints = tmpData_2[,c('LON', 'LAT')]
        distMat = raster::pointDistance(lonlatPoints, lonlat=TRUE)
        distMat = (distMat/111000)*60 # units: nm. assuming 1 degree is 111 km
        dind = row(distMat) - col(distMat)
        
        tmpData_2$DISTANCE_I_I1 = c(split(distMat, dind)$`1`, NA)
        tmpData_2$DISTANCE_FST_ALL = distMat[,1]
        tmpData_2$DISTANCE_LST_ALL = distMat[nrow(distMat),]
        tmpData_2$DISTANCE_RECT_LINE = mean(max(tmpData_2$DISTANCE_FST_ALL, na.rm = TRUE),
                                            max(tmpData_2$DISTANCE_LST_ALL, na.rm = TRUE))
        tmpData_2$TRIP_DISTANCE = sum(tmpData_2$DISTANCE_I_I1, na.rm = TRUE)
        
        point_time_diff = difftime(tmpData_2$TIME[2:nrow(tmpData_2)], 
                                   tmpData_2$TIME[1:(nrow(tmpData_2)-1)], units = 'hours')
        tmpData_2$VELOCITY = tmpData_2$DISTANCE_I_I1/c(as.numeric(point_time_diff), NA)
        cent_grav = 1/tmpData_2$VELOCITY
        cent_grav[is.infinite(cent_grav)] = 10 # max weight, assuming velocity = 0.1
        cent_grav = cent_grav/sum(cent_grav,na.rm = TRUE)
        tmpData_2$CG_LON = sum(tmpData_2$LON*cent_grav, na.rm = TRUE)
        tmpData_2$CG_LAT = sum(tmpData_2$LAT*cent_grav, na.rm = TRUE)
        tmpData_2$CG_DC = sum(tmpData_2$DIST_COAST*cent_grav, na.rm = TRUE)
        tmpData_2$MIN_TIME = min(tmpData_2$TIME)

        # save data:
        indData[[list_ind]] = tmpData_2
        list_ind = list_ind + 1
        
      }
      
    }
    
    print(paste0(i, ": Analizando datos para embarcacion ", name_vessel))
    
  }

  indData2 = rbindlist(indData)

  # Limpieza de datos: Eliminar los que no tienen datos de 
  outData = indData2[(!is.na(indData2$LANDING) & indData2$DIFF_IND_TRIP <= cutoff_time & indData2$TRIP_TIME >= min_trip_time & 
                            indData2$TRIP_DISTANCE <= 11*indData2$TRIP_TIME), ]

  return(outData)

}