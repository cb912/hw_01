mh_distance <- function(x,y)
  if((is.logical(x) == TRUE) && (is.logical(y) == TRUE)) {
    if((is.na(x) == TRUE) || (is.na(y) == TRUE)) {
      warning("x or y cannot be NA, NaN, Inf or -Inf")
      return(-1)
    }else if(x != y) {
      return(1)
    }else {
      return(0)
    }
  }else if((is.character(x) == TRUE) && (is.character(y) == TRUE)) {
    if(nchar(x) != nchar(y)) {
      warning("x and y do not have the same number of digits or letters")
      return(-1)
    }else {
      total <- 0
      xsplit <- strsplit(x,"")[[1]]
      ysplit <- strsplit(y,"")[[1]]
      for(i in 1:length(ysplit)) {
        if(xsplit[i] != ysplit[i]) {
          total <- total + 1
        }
      }
      return(total)
    }
  }else if ((is.numeric(x) == TRUE) && (is.numeric(y) == TRUE)) {
    if(is.infinite(x) || is.nan(x) || is.na(x) || is.infinite(y) || is.nan(y) || is.na(y)) {
      warning("x or y cannot be NA, NaN, Inf or -Inf")
      return(-1)
    }else if((x %% 1 != 0) || (y %% 1 != 0)) {
      warning("x or y cannot contain decimal values")
      return(-1)
    }else if(nchar(as.character(x)) != nchar(as.character(y))) {
      warning("x and y do not have the same number of digits or letters")
      return(-1)
    }
    else {
      total <- 0
      xsplit <- strsplit(as.character(x),"")[[1]]
      ysplit <- strsplit(as.character(y),"")[[1]]
      for(i in 1:length(ysplit)) {
        if(xsplit[i] != ysplit[i]) {
          total <- total + 1
        }
      }
      return(total)
    }
  }else {
    warning("x and y are not both logical, numeric, or character")
    return(-1)
}