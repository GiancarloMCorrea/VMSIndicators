#' Plot inertia and center of gravity
#'
#' Make a plot with center of gravity per trip, trip catch, and trip 
#' initial date.
#'
#' @param data Dataset with required quantities obtained from the
#' preprocessing function.
#' @param vessel_name Vessel name to plot trajectories as shown in the
#' vessel information database obtained from the get_vessel_info function.
#' 'all' can be specified to plot trajectories of all fishing vessels. 
#' @param save_folder Folder to save the plot.
#' @param color_scale Color scale to plot trip initial dates. Standard
#' scale specifies blue and red as the earliest and latest dates,
#' respectively.
#' @param alpha Alpha parameter for color scale.
#' @param mainTitle Main title in plot. Default value is the vessel name.
#' @param save_plot Logical value to indicate if plot is saved as PNG.
#' @return A PNG where the center of gravity and intertia are shown. 
#' Bubble size is proportinal to trip catch.
#' @export
plot_cgi = function(data, vessel_name, save_folder = "./", color_scale = c("blue", "white", "red"), 
                    alpha = 0.1, mainTitle = NULL, save_plot = FALSE, ...) {

  w = NA
  modproj = NA
  mlong = NA 
  mlat = NA
  col = 1
  plot = TRUE

  miss = function(x){
    length(x) == 1 && is.na(x)
  }

  if(!(class(data$MIN_TIME)[1] == "POSIXct")) {
    data$MIN_TIME = lubridate::parse_date_time(data$MIN_TIME, c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M"))
  }

  plotData = data %>%
                  dplyr::group_by(EMB_NOMBRE, TRIP_IND, MIN_TIME) %>%
                  dplyr::summarise(LAT = mean(CG_LAT), 
                            LON = mean(CG_LON),
                            LANDING = mean(LANDING), .groups = 'drop')


  # Read map:
  Peru              = as(PER_ADM0, "SpatialPolygons")
  sp::proj4string(Peru) = sp::CRS("+proj=longlat")
  Peru.proj         = sp::spTransform(Peru, sp::CRS("+proj=utm +zone=18 ellips=WGS84"))

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
      plot_dat = plotData
      vesselName = 'Toda_la_flota'
    } else {
      plot_dat = plotData[plotData$EMB_NOMBRE %in% selVessel, ]
      vesselName = paste(selVessel, collapse = '-')
    }

    plot_dat = plot_dat[order(plot_dat$MIN_TIME), ]
    plot_dat$COL_ID = 1:nrow(plot_dat)

    x = plot_dat$LON
    y = plot_dat$LAT
    z = plot_dat$LANDING

    if(miss(z))
      z <- rep(1, length(x))
    if(miss(w))
      w <- rep(1, length(x))
    sel <- !is.na(x * y * z * w)
    x <- x[sel]
    y <- y[sel]
    z <- z[sel]
    w <- w[sel]
    if(length(x[!is.na(x)]) > 0) {
      if(!miss(modproj)) {
        bid <- dg2nm(x = x, y = y, modproj = modproj, mlong = mlong, mlat = mlat)
        x <- bid$x
        y <- bid$y
      }
      # Center of gravity coordinates
      xg <- sum(x * z * w)/sum(z * w)
      yg <- sum(y * z * w)/sum(z * w)
      
      # Inertia
      dx <- x - xg
      dy <- y - yg
      d <- sqrt(dx^2 + dy^2)
      inert <- sum(z * w * (d^2))/sum(z * w)
      I <- inert  
      
      # Weigthed PCA 
      if(!is.na(I)) {
        M11 <- sum(dx^2 * z * w)
        M22 <- sum(dy^2 * z * w)
        M21 <- sum(dx * dy * z * w)
        M12 <- M21
        M <- matrix(c(M11, M12, M21, M22), byrow = T, ncol = 2)
        x1 <- eigen(M)$vectors[1, 1]
        y1 <- eigen(M)$vectors[2, 1]
        x2 <- eigen(M)$vectors[1, 2]
        y2 <- eigen(M)$vectors[2, 2]
        r1 <- eigen(M)$values[1]/(eigen(M)$values[1] + eigen(M)$values[2])
          
        # Principal axis coordinates
        e1 <- (y1/x1)^2
        sx1 <- x1/abs(x1)
        sy1 <- y1/abs(y1)
        sx2 <- x2/abs(x2)
        sy2 <- y2/abs(y2)
        xa <- xg + sx1 * sqrt((r1 * inert)/(1 + e1))
        ya <- yg + sy1 * sqrt((r1 * inert)/(1 + (1/e1)))
        xb <- 2 * xg - xa
        yb <- 2 * yg - ya
        xc <- xg + sx2 * sqrt(((1 - r1) * inert)/(1 + (1/e1)))
        yc <- yg + sy2 * sqrt(((1 - r1) * inert)/(1 + e1))
        xd <- 2 * xg - xc
        yd <- 2 * yg - yc
        Imax <- r1*inert 
        Imin <- (1-r1)*inert
        Iso <- sqrt(Imin/Imax)
      }
      else {
        xa <- NA
        ya <- NA
        xb <- NA
        yb <- NA
        xc <- NA
        yc <- NA
        xd <- NA
        yd <- NA
        Imax <- NA
        Imin <- NA
        Iso <- NA
      }
      if(!miss(modproj)) {
        bid <- nm2dg(x = c(xg, xa, xb, xc, xd), y = c(yg, ya, yb, yc, yd), 
          modproj = modproj, mlong = mlong, mlat = mlat)
        res <- list(xcg = bid$x[1], ycg = bid$y[1], I = I, Imax = Imax, 
          Imin = Imin, Iso = Iso, xaxe1 = bid$x[2:3], yaxe1 = bid$y[2:3], 
          xaxe2 = bid$x[4:5], yaxe2 = bid$y[4:5])
      }
      else res <- list(xcg = xg, ycg = yg, I = I, Imax = Imax, Imin = Imin, 
        Iso = Iso, xaxe1 = c(xa, xb), yaxe1 = c(ya, yb), xaxe2 = c(xc, xd), 
        yaxe2 = c(yc, yd))

        # Here plot:

        xLim = range(plot_dat$LON) 
        yLim = range(plot_dat$LAT)
        maxCatch = max(plot_dat$LANDING)
        colorsvec = colorRampAlpha(color_scale, n = max(plot_dat$COL_ID), alpha = alpha)
        if(is.null(mainTitle)) {
          thisTitle = vesselName
        } else {
          thisTitle = mainTitle
        }

        
          rgeos::plot(Peru, col = 'grey', xlim = xLim, ylim = yLim, axes = TRUE, main = thisTitle)
          points(x = plot_dat$LON, y = plot_dat$LAT, cex = (plot_dat$LANDING/maxCatch)*2, pch = 19, col = colorsvec[plot_dat$COL_ID])
          segments(res$xaxe1[1], res$yaxe1[1], res$xaxe1[2], res$yaxe1[2], col = 'black', lwd = 2)
          segments(res$xaxe2[1], res$yaxe2[1], res$xaxe2[2], res$yaxe2[2], col = 'black', lwd = 2)
          legend('bottomleft', legend = paste0('Max captura = ', maxCatch, ' t'), bty = 'n')
          box()

        if(save_plot) {
          dev.copy(png, file.path(save_folder, paste0('CGI_', vesselName, '.png')), width = 150, height = 150, units = 'mm', res = 300, ...)
          dev.off()
        }
      
    }
    else {
      res <- list(xcg = NA, ycg = NA, I = NA, Imax = NA, 
        Imin = NA, Iso = NA, xaxe1 = NA, yaxe1 = NA, xaxe2 = NA, yaxe2 = NA)
    }
    
    return(res$Iso)

}

