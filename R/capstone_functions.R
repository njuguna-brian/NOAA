
#' Clean NOAA earthquake data
#'
#' This function takes a raw NOAA earthquake data frame and returns a clean data frame
#' with a date column created by uniting the year, month, day, and time columns, and
#' converting it to the Date class; the LATITUDE and LONGITUDE columns converted to numeric class;
#' and the LOCATION_NAME column cleaned by stripping out the country name (including the colon)
#' and converting names to title case (as opposed to all caps).
#'
#' @param raw_data A data frame containing raw NOAA earthquake data
#'
#' @return A clean data frame containing date, latitude, longitude, depth, magnitude, and location columns
#'
#' @examples
#'\dontrun{
#' path <- "path/to/your/data.txt"
#' earthquakes <- read_delim(path, delim = "\t")
#' earthquakes_clean <- eq_clean_data(earthquakes)
#' }
#'
#' @importFrom dplyr filter select mutate
#' @importFrom lubridate ymd_hms
#' @importFrom stringr str_to_title
#' @importFrom stats na.omit
#' @importFrom magrittr  %>%
#'
#' @export
eq_clean_data <- function(raw_data) {
  # Define all global variables used in the script
  Latitude <- NULL
  Tsu <- NULL
  Vol <- NULL
  `Damage Description` <- NULL
  `Injuries Description` <- NULL
  `Missing Description` <- NULL
  `Search Parameters` <- NULL
  Year <- NULL
  Mo <- NULL
  Dy <- NULL
  Hr <- NULL
  Mn <- NULL
  Sec <- NULL
  Longitude <- NULL
  `Location Name` <- NULL
  latitude <- NULL
  longitude <- NULL
  `Focal Depth (km)` <- NULL
  Mag <- NULL
  country <- NULL
  location <- NULL
  `Houses Damaged` <- NULL
  `Houses Destroyed` <- NULL
  `Total Deaths` <- NULL
  suppressWarnings({
    clean_data <- raw_data %>%
      dplyr::filter(!is.na(Latitude)) %>% # remove rows with missing Latitude
      dplyr::select(
        # remove unused columns
        -c(
        Tsu,
        Vol,
        `Damage Description`,
        `Injuries Description`,
        `Missing Description`,
        `Search Parameters`
  )) %>%
    dplyr::mutate(
      # create date column
      date =  ymd_hms(paste(Year, Mo, Dy, Hr, Mn, Sec, sep = "-")),

      # convert Latitude to numeric
      latitude = as.numeric(Latitude),

      # convert Longitude to numeric
      longitude = as.numeric(Longitude),

      # Cleaning the location
      location = `Location Name` %>%
        sub(pattern = "^.*:\\s*", replacement = "") %>%
        stringr::str_to_title(),

      # Extracting the countries
      country = stringr:: str_extract(`Location Name`,  "^[A-Z]+(?=:)")

    ) %>%
    dplyr::select(
      date,
      latitude,
      longitude,
      depth = `Focal Depth (km)`,
      magnitude = Mag,
      country,
      location,
      house_damaged = `Houses Damaged`,
      house_destroyed = `Houses Destroyed`,
      deaths = `Total Deaths`,

    ) %>%
    stats:: na.omit()
  return(clean_data)
  })
}


#' GeomTimeline for ggplot2
#'
#' GeomTimeline is a custom geom for ggplot2 that allows you to plot a timeline of data points, where the x-axis represents time and the y-axis is fixed at y=0. Optional aesthetics include size, color, alpha, shape, stroke, and fill. The default values for aesthetics are size=1, color='grey50', alpha=0.5, shape=19, stroke=0.5, and fill=NA.
#'
#' @param mapping Aesthetic mapping.
#' @param data The dataset to be used for the plot.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position Position adjustment to use for this layer.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.
#' @param ... Other arguments passed on to layer(). These are often aesthetics, used to set an aesthetic to a fixed value, like color = "red" or size = 3. They may also be parameters to the paired geom/stat.
#' @return A ggplot layer containing a time line of earthquakes.
#'
#' @examples
#' \dontrun{
#' ggplot(data = earthquakes) +
#' geom_timeline(mapping = aes(x = date, size = depth, color = magnitude)) +
#' scale_size_continuous(trans = 'reverse')
#'}
#'
#' @importFrom grid gpar pointsGrob
#'
#' @export
geom_timeline <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      geom = GeomTimeline,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }
#' GeomTimeline for plotting earthquake data.
#'
#' GeomTimeline is a custom geom for ggplot2 that allows you to plot a timeline of data points,
#' where the x-axis represents time and the y-axis is fixed at y=0. Optional aesthetics include size,
#' color, alpha, shape, stroke, and fill. The default values for aesthetics are size=1,
#' color='grey50', alpha=0.5, shape=19, stroke=0.5, and fill=NA.
#'
#' @rdname GeomTimeLine
#' @format ggplot2::ggproto
#'
#' @export
GeomTimeline <- ggplot2::ggproto(
  "GeomTimeline",
  ggplot2::Geom,
  required_aes = c("x"),
  default_aes = ggplot2::aes(
    y = 0,
    size = 1,
    color = "grey50",
    alpha = 0.5,
    shape = 19,
    stroke = 0.5,
    fill = NA
  ),
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)
    coords$size <- coords$size / max(coords$size) * 1.5

    grid::pointsGrob(
      x = coords$x,
      y = coords$y,
      pch = coords$shape,
      gp = grid::gpar(
        col = coords$colour,
        alpha = coords$alpha,
        cex = coords$size
      )
    )
  }
)



#' GeomTimeLineLabel geom for label to earthquake data plot
#'
#' This geom adds a vertical line to each data point with a text annotation attached to each line.
#' There is an option to subset to n_max number of earthquakes, where the n_max largest (by magnitude) earthquakes are selected.
#' Aesthetics are x, which is the date of the earthquake, and tags which is the column name from which annotations will be obtained.
#'
#' @format A ggplot2::ggproto object
#' @export GeomTimeLineLabel
#' @param mapping A set of aesthetic mappings created by aes() or aes_(). Only x and tags are required.
#' @param data A data.frame or tibble containing the variables to be plotted.
#' @param na.rm A logical value. Should missing values be removed from the plot?
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped to variables with guide boxes.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position The position adjustment to use for overlapping points or bars, as a string.
#' @param inherit.aes A logical value. Should the default aesthetics be used for this layer? If FALSE, overrides are used instead.
#' @param ... Additional arguments passed to layer().
#' @return A ggplot2 plot with a timeline label.
#'
#' @examples
#' \dontrun{
#' earthquake_clean %>%
#' filter(country == "JAPAN") %>%
#' ggplot() +
#' geom_timeline(aes(
#' x = date,
#'  size = magnitude,
#'   color = deaths,
#'   fill = deaths
#'   )) +
#'   theme_minimal() +
#'   theme(axis.line = element_line()) +
#'   geom_timeline_label(aes(date, tags = location))}
#' @importFrom  ggplot2 ggproto aes
#'
#' @export
geom_timeline_label <-
  function(mapping = NULL,
           data = NULL,
           na.rm = TRUE,
           show.legend = NA,
           stat = "identity",
           position = "identity",
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      geom = GeomTimeLineLabel,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }
#' GeomTimeLineLabel geom for adding annotations to earthquake data
#'
#' This geom adds a vertical line to each data point with a text annotation attached to each line.
#' There is an option to subset to n_max number of earthquakes, where the n_max largest (by magnitude) earthquakes are selected.
#' Aesthetics are x, which is the date of the earthquake, and tags which is the column name from which annotations will be obtained.
#'
#' @rdname GeomTimeLineLabel
#' @format ggplot2::ggproto
#'
#' @export
GeomTimeLineLabel <- ggproto(
  "GeomTimeLineLabel2",
  ggplot2:: Geom,
  required_aes = c("x", "tags"),
  default_aes = aes(
    y = 0.5,
    number = NULL,
    max_aes = NULL
  ),

  draw_key = ggplot2:: draw_key_point,

  draw_panel = function(data, panel_params, coord) {
    coords <- coord$transform(data, panel_params)

    Timeline_seg_grobs <-
      segmentsGrob(
        x0 = unit(coords$x, "npc"),
        y0 = unit(coords$y, "npc"),
        x1 = unit(coords$x, "npc"),
        y1 = unit(coords$y + 0.06 /
                    length(unique(coords$y)), "npc"),
        default.units = "npc",
        arrow = NULL,
        name = NULL,
        gp = gpar(),
        vp = NULL
      )

    Earthquake_text_grobs <-
      textGrob(
        label = coords$tags,
        x = unit(coords$x, "npc"),
        y = unit(coords$y + 0.06 /
                   length(unique(coords$y)), "npc"),
        rot = 60,
        just = "left",
        gp = gpar(fontsize = 8)
      )

    gTree(children = gList(Timeline_seg_grobs, Earthquake_text_grobs))
  }
)





#' Creates a leaflet map with circle markers representing earthquake locations and their associated annotation
#'
#' This function takes earthquake data and an annotation column and creates a leaflet map with circle markers at each earthquake location. The annotation column is used to display additional information about each earthquake in the popup text when the user hovers over a marker.
#'
#' @param data A data frame containing earthquake data that has been cleaned using the `eq_clean_data` function.
#' @param annot_col A string representing the name of the column in the `data` data frame that should be used for annotation in the popup text.
#'
#' @return A leaflet map with circle markers representing earthquake locations and their associated annotation.
#'
#' @examples
#' \dontrun{
#' data <- read_delim("earthquakes.txt", delim = "\t") %>%
#'         eq_clean_data() %>%
#'         filter(country == "MEXICO" & year(date) >= 2000)
#' eq_map(data, annot_col = "date")
#'}
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @importFrom magrittr  %>%
#'
#' @export
eq_map <- function(data, annot_col) {

  # create popup text
  popup_text <- paste0(annot_col, ": ", data[[annot_col]])

  # create leaflet map
  leaflet:: leaflet(data) %>%
    leaflet:: addTiles() %>%
    leaflet:: addCircleMarkers(~longitude , ~latitude,
                     popup = paste0(data[[annot_col]]))
}




#' Create popup labels for earthquakes
#'
#' This function takes in earthquake data and creates popup labels for the map
#' visualization.
#'
#' @param data A data frame of earthquake data
#'
#' @return A character vector of popup labels
#'
#' @examples
#'\dontrun{
#' readr::read_delim("earthquakes.txt", delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(country == "MEXICO" & lubridate::year(date) >= 2000) %>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>%
#' eq_map(annot_col = "popup_text")
#'}
#'
#' @export
eq_create_label <- function(data) {
  location <- ifelse(is.na(data$location), data$location, data$location)
  mag <- ifelse(is.na(data$magnitude), "", paste0("<b>Magnitude</b>: ", data$magnitude, "<br>"))
  deaths <- ifelse(is.na(data$deaths), "", paste0("<b>Total deaths</b>: ", data$deaths, "<br>"))
  label <- paste0("<b>Location</b>: ", location, "<br>", mag, deaths)
  return(label)
}














