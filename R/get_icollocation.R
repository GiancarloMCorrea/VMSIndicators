#' Global index of collocation
#'
#' Calculate global index of collocation between trip i and trip i+1.
#'
#' @param data Dataset with required quantities obtained from the
#' preprocessing function.
#' @param vessel_name Vessel name to plot trajectories as shown in the
#' vessel information database obtained from the get_vessel_info function.
#' 'all' can be specified to plot trajectories of all fishing vessels.
#' @return A data.frame with the efficiency indicators per trip.
#' @export
get_icollocation = function (data, vessel_name) {

  if(!(class(data$MIN_TIME)[1] == "POSIXct")) {
    data$MIN_TIME = lubridate::parse_date_time(data$MIN_TIME, c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M"))
  }

  #Vessel info:
  if(vessel_name != 'all') {
    if(!(vessel_name %in% unique(data$EMB_NOMBRE))) {
      stop(paste0("La embarcacion ", vessel_name, " no se pudo encontrar en la base de datos."))
    }
    selVessel = vessel_name
  } else {
    selVessel = unique(data$EMB_NOMBRE)
  }

    if(vessel_name == 'all') {
      plot_dat = data
      vesselName = 'Toda_la_flota'
    } else {
      plot_dat = data[data$EMB_NOMBRE %in% selVessel, ]
      vesselName = paste(selVessel, collapse = '-')
    }

    plot_dat$MIN_TIME_ROUND = format(data$MIN_TIME, format = '%Y-%m-%d')

    # Select right column:
    if(vessel_name == 'all' | length(vessel_name) > 1) {
      for_vec = sort(as.vector(unique(plot_dat$MIN_TIME_ROUND)), decreasing = FALSE)
    } else {
      for_vec = sort(as.vector(unique(plot_dat$TRIP_IND)), decreasing = FALSE)
    }


    tmp_out = list()
    for(i in 1:(length(for_vec)-1)) {

      if(vessel_name == 'all' | length(vessel_name) > 1) {
        tmp1 = dplyr::filter(plot_dat, MIN_TIME_ROUND == for_vec[i])
        tmp2 = dplyr::filter(plot_dat, MIN_TIME_ROUND == for_vec[i+1])
      } else {
        tmp1 = dplyr::filter(plot_dat, TRIP_IND == for_vec[i])
        tmp2 = dplyr::filter(plot_dat, TRIP_IND == for_vec[i+1])
      }

      x1 = tmp1$LON
      y1 = tmp1$LAT
      z1 = 1/tmp1$VELOCITY
      z1[is.infinite(z1)] = 10
      z1[is.na(z1)] = 0

      x2 = tmp2$LON
      y2 = tmp2$LAT
      z2 = 1/tmp2$VELOCITY
      z2[is.infinite(z2)] = 10
      z2[is.na(z2)] = 0

      w1 = NA
      w2 = NA
      modproj = NA
      mlong = NA
      mlat = NA

      # Compute the centers of gravity and inertia of the two populations
      popZ1 <- cgi(x1, y1, z1, w1, modproj=modproj, mlong=mlong, mlat=mlat, plot=F)
      popZ2 <- cgi(x2, y2, z2, w2, modproj=modproj, mlong=mlong, mlat=mlat, plot=F)
      
      # Perform the projection
      Z1 <- dg2nm(x=popZ1$xcg, y=popZ1$ycg, modproj=modproj, mlong=mlong, mlat=mlat)
      Z2 <- dg2nm(x=popZ2$xcg, y=popZ2$ycg, modproj=modproj, mlong=mlong, mlat=mlat)   
      
      # Compute the 'GIC' index
      GIC <- (((Z1$x-Z2$x)^2+(Z1$y-Z2$y)^2) / (((Z1$x-Z2$x)^2+(Z1$y-Z2$y)^2) 
          + popZ1$I + popZ2$I))
      if(!is.na(GIC))
          GIC <- 1-GIC
      else GIC <- 1
      
      tmp_out[[i]] = data.frame(INDEX = for_vec[i], TIME = unique(tmp1$MIN_TIME)[1], 
                                CG_LAT_1 = mean(tmp1$CG_LAT), CG_LON_1 = mean(tmp1$CG_LON),
                                CG_LAT_2 = mean(tmp2$CG_LAT), CG_LON_2 = mean(tmp2$CG_LON),  
                                GIC = GIC, VESSEL = vesselName)

    }

    indData2 = data.table::rbindlist(tmp_out)

    return(indData2)

}
