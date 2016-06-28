#' Transform your United States shape file by removing or rotating Alaska and Hawaii.
#'
#' @param mapdat The spatial data frame you would like to transform.
#'        Any of the spatial objects returned by the tigris package are acceptable.
#' @param cont.us If set to TRUE it will return a map of only the lower forty-eight states.
#' @param fifty.states If set TRUE it will return Alaska and Hawaii, but not any teirritories
#'        or outlying islands.
#' @param original.positions If set to FALSE it will re-scale and move Alaska and Hawaii.
#'        Note, it is set to FALSE by default.
#' @import sp maptools
#' @export map.transform
#' @examples \dontrun{
#' library(tigris)
#'
#'
map.transform <- function (mapdat, cont.us = NA, fifty.states = NA,
                           original.positions = FALSE){

  if(is.na(cont.us) && is.na(fifty.states)){
    stop("Please select a peramater for your transformation.")
  }
  if(isTRUE(cont.us) && isTRUE(fifty.states)){
    stop("Please select either continental US or fifty states.")
  }

  # Convert to equel area, we'll need this either way.
  mapdat <- spTransform(mapdat, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0
                                    +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  mapdat@data$id <- rownames(mapdat@data)

  if (isTRUE(cont.us)){
    # Remove Alaska(02) and Hawaii (15) from map.
    mapdat <- mapdat[!mapdat$STATEFP %in% c("02", "15"),]
    # Make sure other outling islands are removed including, Puerto Rico (72), Guam (66),
    #Virgin Islands (78), American Samoa (60) Mariana Islands (69)
    # Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
    mapdat <- mapdat[!mapdat$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                            "95", "79", "72", "66", "78", "60", "69",
                                            "64", "68", "70", "74"),]
  }

  if (isTRUE(fifty.states) && !isTRUE(original.positions)){
    # Rotate and shrink ak.
    ak <- subset(mapdat, STATEFP=="02")
    ak <- elide(ak, rotate=-50)
    ak <- elide(ak, scale=max(apply(bbox(ak), 1, diff)) / 2.3)
    ak <- elide(ak, shift=c(-2100000, -2500000))
    proj4string(ak) <- proj4string(mapdat)

    # Rotate and Shift hawi
    hawi <- subset(mapdat, STATEFP=="15")
    hawi <- elide(hawi, rotate=-35)
    hawi <- elide(hawi, shift=c(5400000, -1600000))
    proj4string(hawi) <- proj4string(mapdat)

    # Remove Alaska(02) and Hawaii (15) from map.
    mapdat <- mapdat[!mapdat$STATEFP %in% c("02", "15"),]
    # Make sure other outling islands are removed including, Puerto Rico (72), Guam (66),
    #Virgin Islands (78), American Samoa (60) Mariana Islands (69)
    # Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
    mapdat <- mapdat[!mapdat$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                            "95", "79", "72", "66", "78", "60", "69",
                                            "64", "68", "70", "74"),]
    # Rbind Alaska and Hawaii
    mapdat <- rbind(mapdat, ak, hawi)
  }

  if (isTRUE(fifty.states) && isTRUE(original.positions)){
    # Make sure other outling islands are removed including, Puerto Rico (72), Guam (66),
    #Virgin Islands (78), American Samoa (60) Mariana Islands (69)
    # Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
    mapdat <- mapdat[!mapdat$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                            "95", "79", "72", "66", "78", "60", "69",                                                "64", "68", "70", "74"),]
  }

  return(mapdat)
}
