#' Download shapefile for all states into R
#'
#' States and Equivalent Entities are the primary governmental divisions of the
#' United States.  In addition to the 50 states, the Census Bureau treats the
#' District of Columbia, Puerto Rico, American Samoa, the Commonwealth of the
#' Northern Mariana Islands, Guam, and the U.S. Virgin Islands as the statistical
#' equivalents of states for the purpose of data presentation.
#'
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        states file.  Defaults to FALSE (the most detailed TIGER/Line file)
#' @param resolution The resolution of the cartographic boundary file (if cb == TRUE).
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @param detailed (deprecated) Setting detailed to FALSE returns a 1:500k cartographic boundary file.
#'        This parameter will be removed in a future release.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2015).
#' @export
#' @family general area functions
#' @seealso \url{https://www.census.gov/geo/reference/gtc/gtc_state.html}
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' states <- states(detailed=FALSE)
#'
#' leaflet(states) %>%
#'   addProviderTiles("CartoDB.Positron") %>%
#'   addPolygons(fillColor = "white",
#'               color = "black",
#'               weight = 0.5) %>%
#'   setView(-98.5795, 39.8282, zoom=3)
#' }
states <- function(cb = FALSE, resolution = '500k', detailed = TRUE, ...) {

  if (!(resolution %in% c('500k', '5m', '20m'))) {
    stop("Invalid value for resolution. Valid values are '500k', '5m', and '20m'.", call. = FALSE)
    }

  if (detailed == FALSE) {
    cb = TRUE
    message("The `detailed` parameter is deprecated.  Use `cb` instead.")
  }

  if (cb == TRUE) {
    url <- paste0("http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_state_",
                  resolution,
                  ".zip")
  } else {
    url <- "http://www2.census.gov/geo/tiger/TIGER2015/STATE/tl_2015_us_state.zip"
  }

  return(load_tiger(url, tigris_type="state", ...))

}
#' Filter a \code{states} Spatial object for only those states matching the
#' contents of the \code{state} vector.
#'
#' @param states object returned from a call to \code{states}
#' @param state a vector of full state names. The function performs the
#'        comparison in a case-insensitive manner.
#' @export
#' @examples \dontrun{
#' states() %>% filter_state("south")
#' }
filter_state <- function(states, state) {
  if (is_tigris(states) & tigris_type(states) == "state") {
    tmp <- states[tolower(states$NAME) %in% tolower(state),]
    attr(tmp, "tigris") <- "state"
    return(tmp)
  }
}

#' Find states matching a term in a \code{state} object
#'
#' This is just shorthand for
#' \code{grep(term, list_states(states), value=TRUE, ignore.case=TRUE)}
#'
#' @param states object returned from a call to \code{state}
#' @param term equivalent to the \code{pattern} argument of \code{grep}
#' @export
#' @examples \dontrun{
#' states() %>% grep_state("north")
#' }
grep_state <- function(states, term) {
  if (is_tigris(states) & tigris_type(states) == "state") {
    grep(term, list_states(states), value=TRUE, ignore.case=TRUE)
  }
}

#' Return a list of all the states in a \code{state} object
#'
#' @param states object returned from a call to \code{state}
#' @param sorted return the list sorted or in the order found in the shapefile?
#' @export
#' @examples \dontrun{
#' states() %>% list_states()
#' }
list_states <- function(states, sorted=TRUE) {
  if (is_tigris(states) & tigris_type(states) == "state") {
    if (sorted) return(sort(states@data$NAME))
    return(states@data$NAME)
  }
}
