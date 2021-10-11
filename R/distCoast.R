#' Distance to the shoreline
#'
#' Calculates minimum distance to the Peruvian shoreline.
#'
#' @param lon Longitude vector in decimal units (e.g. -76.5242).
#' @param lat Latitude vector in decimal units (e.g. -12.1352).
#' @param mainLand Logical value. Indicates if distance is calculated 
#' to the shoreline (TRUE) or to shoreline and islands (FALSE).
#' @return A vector with distances (in nautical miles).
#' @export
distCoast = function(lon, lat, mainLand = TRUE){
  
  temp = data.frame(lon = lon, lat = lat)
  posiciones = temp[,c("lon", "lat")]
  
  #- Convert VMS data to SpatialPolygons
  spTa              = SpatialPoints(data.frame(posiciones))
  proj4string(spTa) = CRS("+proj=longlat")
  spTa.proj         = spTransform(spTa, CRS("+proj=utm +zone=18 ellips=WGS84"))
  
  #- Read shapefile of Peru
  load("PER_ADM0.RData")
  Peru              = as(PER_ADM0, "SpatialPolygons")

  if(mainLand) {
    newPeru = Peru@polygons[[1]]@Polygons[[22]]
    firstPoly = sp::Polygons(list(newPeru), ID = "A")
    firstSpatialPoly = sp::SpatialPolygons(list(firstPoly))
    proj4string(firstSpatialPoly) = CRS("+proj=longlat")
    Peru.proj         = spTransform(firstSpatialPoly, CRS("+proj=utm +zone=18 ellips=WGS84"))
  } else {
    proj4string(Peru) = CRS("+proj=longlat")
    Peru.proj         = spTransform(Peru, CRS("+proj=utm +zone=18 ellips=WGS84"))
  }

  dists = gDistance(spgeom1 = spTa.proj, spgeom2=Peru.proj,byid=T) #
  distance       = as.vector(t(dists*0.00053996)) # convirtiendo de metros a millas nauticas
 
  return(distance)
  
}
