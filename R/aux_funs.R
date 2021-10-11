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
