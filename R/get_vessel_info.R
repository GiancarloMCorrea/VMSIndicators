#' Vessel information
#'
#' Get information for vessel name and code (matricula) in the VMS data.
#'
#' @param vmsdata VMS dataset.
#' @return A data.frame with two columns: vessel name and code (matricula). 
#' @export
get_vessel_info = function(vmsdata) {

  vesselInfo = vmsdata %>%
                dplyr::group_by(MATRICULA) %>%
                dplyr::summarise(EMB_NOMBRE = unique(NAVE))
  vesselInfo$EMB_NOMBRE = gsub(pattern = '[[:space:]]', replacement = '_', x = vesselInfo$EMB_NOMBRE)


  if(length(vesselInfo$MATRICULA) != length(unique(vmsdata$MATRICULA))) {
    warning("Parece que existen dos nombres diferentes para la misma matricula. Revisar los datos.")
  }

  return(vesselInfo)

}
