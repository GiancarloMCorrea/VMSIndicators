#' Plot trajectories
#'
#' Make plots of trajectories per fishing trip. 
#'
#' @param data Dataset with required quantities obtained from the
#' preprocessing function.
#' @param vessel_name Vessel name to plot trajectories as shown in the
#' vessel information database obtained from the get_vessel_info function.
#' 'all' can be specified to plot trajectories of all fishing vessels. 
#' @param vesseldata Vessel information.
#' @param save_folder Folder to save the plots.
#' @param save_plot Logical value to indicate if plot is saved as PNG.
#' @param ... Other artuments for the plot function.
#' @return PNG files are created in the specified folder.
#' @export
plot_trajectory = function(data, vessel_name = 'all', save_folder = './', save_plot = FALSE, ...) {

  # Create trip index:
  data$TRIP = paste(data$MATRICULA, data$TRIP_IND, sep = '_')

  # Read map:
  Peru              = as(PER_ADM0, "SpatialPolygons")
  sp::proj4string(Peru) = sp::CRS("+proj=longlat")
  Peru.proj         = sp::spTransform(Peru, sp::CRS("+proj=utm +zone=18 ellips=WGS84"))

  #Vessel info:
  if(vessel_name != 'all') {
    if(!(vessel_name %in% unique(data$EMB_NOMBRE))) {
      stop(paste0("La embarcacion ", vessel_name, " no se pudo encontrar en la base de datos."))
    }
    selVessel = unique(data$MATRICULA[data$EMB_NOMBRE == vessel_name])[1]
  } else {
    selVessel = unique(data$MATRICULA)
  }

  # Start loop to plot:
  for(j in seq_along(selVessel)) {

    # Select vessel:
    selVesselInd = selVessel[j]

    tripIndex = unique(data$TRIP_IND[data$MATRICULA == selVesselInd])
    vesselName = unique(data$EMB_NOMBRE[data$MATRICULA == selVesselInd])[1]

    trip_i = 1
    for(k in seq_along(tripIndex)) {

      indViaje = tripIndex[k]

      plotData = dplyr::filter(data, TRIP_IND == indViaje & MATRICULA == selVesselInd)
      xLim = range(plotData$LON) 
      yLim = range(plotData$LAT) 
      nHours = difftime(plotData$TIME[nrow(plotData)], plotData$TIME[1], units = 'hours')
      sarr = seq(nrow(plotData) - 1)
      
        rgeos::plot(Peru, col = 'grey', xlim = xLim, ylim = yLim, axes = TRUE, 
             main = paste(vesselName, ' - Viaje: ', trip_i, sep = ''),
             sub = paste0('Inicio: ', plotData$TIME[1], ' - Fin: ', plotData$TIME[nrow(plotData)]))
        points(plotData$LON, plotData$LAT)
        arrows(plotData$LON[sarr], plotData$LAT[sarr], plotData$LON[sarr + 1], plotData$LAT[sarr + 1], length = 0.08, col = 'red')
        box()

      if(save_plot) {

        dev.copy(png, file.path(save_folder, paste0(selVesselInd, '_', trip_i, '.png')), width = 150, height = 150, units = 'mm', res = 300, ...)
        dev.off()

      }

      trip_i = trip_i+1

    }

  }

}