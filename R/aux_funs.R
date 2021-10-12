colorRampAlpha = function(..., n, alpha) {
   colors = grDevices::colorRampPalette(...)(n)
   paste(colors, sprintf("%x", ceiling(255*alpha)), sep="")
}


find_trip = function(ind_vec, cutoff) {
  
  outvec = rep(NA, times = length(ind_vec))
  indnum = 1
  
  for(j in 1:length(ind_vec)) {
    
    if(j == 1) {
      if(ind_vec[j] > cutoff) {
        outvec[j] = indnum
      }
      if(ind_vec[j] <= cutoff) { 
        outvec[j] = 0 
      } 
    }
    
    if(j > 1) {
      if(ind_vec[j] <= cutoff & outvec[j-1] == 0) {
        outvec[j] = 0 
      }
      if(ind_vec[j] <= cutoff & outvec[j-1] != 0) {
        outvec[j] = 0
        indnum = indnum + 1
      }
      if(ind_vec[j] > cutoff) {
        outvec[j] = indnum
      }
    }

  }
  
  return(outvec)
  
}

"cgi" <-
function(x = long, y = lat, z = NA, w = NA, modproj = NA, mlong = NA, 
  mlat = NA, col = 1, plot = T)
{
  miss <- function(x){
    length(x) == 1 && is.na(x)
  }

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
    if(plot == T) {
      segments(res$xaxe1[1], res$yaxe1[1], res$xaxe1[2], res$yaxe1[2], col = col, lwd = 2)
      segments(res$xaxe2[1], res$yaxe2[1], res$xaxe2[2], res$yaxe2[2], col = col, lwd = 2)
    }
  }
  else {
    res <- list(xcg = NA, ycg = NA, I = NA, Imax = NA, 
      Imin = NA, Iso = NA, xaxe1 = NA, yaxe1 = NA, xaxe2 = NA, yaxe2 = NA)
  }
  res
}


"dg2nm" <-
function(x, y = NA, modproj, mlong, mlat)
{

  miss <- function(x){
    length(x) == 1 && is.na(x)
  }
  
  if(is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if(!miss(modproj)) {
    x <- x - mlong
    y <- y - mlat
    if(modproj == "mean") {
      x <- x * 60 * cos((mlat * pi)/180)
      y <- y * 60
    }
    else if(is.numeric(modproj)) {
      x <- x * 60 * modproj
      y <- y * 60
    }
    else if(modproj == "cosine") {
      x <- x * 60 * cos((y * pi)/180)
      y <- y * 60
    }
  }
  list(x = x, y = y)
}

"nm2dg" <-
function(x, y = NA, modproj, mlong, mlat)
{

  miss <- function(x){
    length(x) == 1 && is.na(x)
  }
  
  if(is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if(!miss(modproj)) {
    if(modproj == "mean") {
      y <- y/60
      x <- x/(60 * cos((mlat * pi)/180))
    }
    else if(is.numeric(modproj)) {
      x <- x/(60 * modproj)
      y <- y/60
    }
    else if(modproj == "cosine") {
      y <- y/60
      x <- x/(60 * cos(((y + mlat) * pi)/180))
    }
  }
  x <- x + mlong
  y <- y + mlat
  list(x = x, y = y)
}
