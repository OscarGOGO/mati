#' Map camera-trap records by sampling station
#'
#' Create maps showing the number of camera-trap records at each sampling
#' station for one or more species. Point size represents the number of
#' records, while point color distinguishes stations with and without
#' detections. All sampling stations are retained, including stations where
#' the selected species was not detected.
#'
#' The function uses station coordinates from the complete dataset before
#' filtering by species. Therefore, stations without records for a particular
#' species are represented with zero records instead of being removed.
#' A satellite basemap is downloaded using \code{maptiles::get_tiles()}.
#'
#' @param data `data.frame`. Table containing the camera-trap records.
#' Each row should represent a camera-trap record or detection and include
#' the sampling station, geographic coordinates, and, when applicable,
#' the species name.#'
#' @param station_field `character`. Name of the column identifying the
#' camera-trap station. Each station should have a unique identifier.
#' @param species_field `character` or `NULL`. Name of the column containing
#' the species name. If `NULL`, all records are treated as belonging to a
#' single group called `"All records"`.
#' @param species_filter `character` or `NULL`. Species to include in the
#' analysis. One or several species can be supplied. For example,
#' `species_filter = "Canis latrans"` or
#' `species_filter = c("Canis latrans", "Puma concolor")`.
#' If `NULL`, maps are generated for all species present in `species_field`.
#' This argument cannot be used when `species_field = NULL`.
#' @param lat `character`. Name of the column containing latitude in decimal
#' degrees. Latitude values must be between -90 and 90.
#' @param long `character`. Name of the column containing longitude in decimal
#' degrees. Longitude values must be between -180 and 180.
#' @param zoom `numeric`. Zoom level used to download the basemap tiles.
#' Default = `13`. Larger values provide more spatial detail but require
#' downloading more map tiles.
#' @param provider `character`. Basemap provider passed to
#' \code{maptiles::get_tiles()}. Default = `"Esri.WorldImagery"`.
#' @param save_map `character`. Directory where maps are saved. Default = NULL. If the directory does not exist, it is created.
#' @param width `numeric`. Width of the exported map in inches.
#' Default = `10`.
#' @param height `numeric`. Height of the exported map in inches.
#' Default = `6`.
#' @param dpi `numeric`. Resolution of exported maps in dots per inch.
#' Default = `600`.
#' @details
#' The number of records is calculated independently for each sampling station
#' and species. Stations where the focal species was not detected are assigned
#' `n = 0` and retained in the map.
#'
#' Point size represents the number of records. A continuous size scale with
#' a non-zero minimum symbol size is used so that stations with zero records
#' remain visible.
#'
#' The geographic coordinates are assumed to be in WGS84
#' (EPSG:4326). When more than one coordinate pair is found for the same
#' station, the mean latitude and longitude are used to represent that station.
#'
#' Satellite imagery is downloaded from the selected map provider and the
#' extent is expanded by approximately 5 percent around the sampling stations.
#'
#' @return Invisibly returns a named `list` of `ggplot` objects, with one map
#' for each species. The names of the list correspond to the species names.
#' Maps are also printed during execution. If `save_map` is not NULL, PNG files are
#' written.
#' @examples
#' \dontrun{
#' records <- event_independ(input = datab,
#'                            station_field = "Station",
#'                            species_field = "Species",
#'                            date_field = "Date",
#'                            date_format = "%d/%m/%Y",
#'                            time_field = "Time",
#'                            independent_time = 1,
#'                            independent_units = "hours")
#' maps <- records_map(
#'   data = records,
#'   station_field = "Station",
#'   species_field = "Species",
#'   species_filter = "Puma concolor",
#'   lat = "Latitud",
#'   long = "Longitud"
#' )
#'
#' # Map all records without separating by species
#' maps <- records_map(
#'   data = records,
#'   station_field = "Station",
#'   species_field = NULL,
#'   lat = "Latitud",
#'   long = "Longitud"
#' )
#' }
#'
#' @export
#'
#' @importFrom sf st_as_sf st_bbox st_as_sfc st_crs st_buffer
#' @importFrom maptiles get_tiles
#' @importFrom tidyterra geom_spatraster_rgb
#' @importFrom ggplot2 ggplot geom_sf aes scale_size_continuous
#' @importFrom ggplot2 scale_fill_manual coord_sf guide_legend labs
#' @importFrom ggplot2 theme_minimal theme element_line element_blank
#' @importFrom ggplot2 element_text element_rect margin ggsave
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @importFrom scales alpha
#' @importFrom grid unit

records_map <- function(data,
                        station_field   = NULL,
                        species_field   = NULL,
                        species_filter  = NULL,
                        lat             = NULL,
                        long            = NULL,
                        zoom            = 13,
                        provider        = "Esri.WorldImagery",
                        save_map        = NULL,
                        width           = 10,
                        height          = 6,
                        dpi             = 600) {
  if (is.null(data)) {
    stop("'data' cannot be NULL.")
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or an object inheriting from data.frame.")
  }

  if (nrow(data) == 0) {
    stop("'data' contains no rows.")
  }

  if (is.null(station_field)) {
    stop("'station_field' must be provided.")
  }

  if (length(station_field) != 1) {
    stop("'station_field' must contain only one column name.")
  }

  if (!station_field %in% names(data)) {
    stop(paste0(
      "The station field '", station_field,
      "' was not found in data."
    ))
  }
  # If NULL, all records will be treated as a single group.

  if (!is.null(species_field)) {
    if (length(species_field) != 1) {
      stop("'species_field' must contain only one column name.")
    }

    if (!species_field %in% names(data)) {
      stop(paste0(
        "The species field '", species_field,
        "' was not found in data."
      ))
    }
  }
  if (!is.null(species_filter)) {
    if (is.null(species_field)) {
      stop(
        "'species_filter' cannot be used when 'species_field' is NULL."
      )
    }

    if (length(species_filter) == 0) {
      stop("'species_filter' is empty.")
    }
  }

  if (is.null(lat)) {
    stop("'lat' must be provided.")
  }

  if (length(lat) != 1) {
    stop("'lat' must contain only one column name.")
  }

  if (!lat %in% names(data)) {
    stop(paste0(
      "The latitude field '", lat,
      "' was not found in data."
    ))
  }

  if (is.null(long)) {
    stop("'long' must be provided.")
  }

  if (length(long) != 1) {
    stop("'long' must contain only one column name.")
  }

  if (!long %in% names(data)) {
    stop(paste0(
      "The longitude field '", long,
      "' was not found in data."
    ))
  }

  # Latitude and longitude cannot be the same column
  if (lat == long) {
    stop("'lat' and 'long' must refer to different columns.")
  }

  if (!is.numeric(zoom) ||
      length(zoom) != 1 ||
      is.na(zoom) ||
      zoom < 1 ||
      zoom > 20) {

    stop("'zoom' must be a single numeric value between 1 and 20.")
  }

  if (!is.character(provider) ||
      length(provider) != 1 ||
      is.na(provider) ||
      provider == "") {

    stop("'provider' must be a valid character string.")
  }

  if(!is.null(save_map)){
    if (!is.character(save_map) ||
        length(save_map) != 1) {
      stop("'save_map' must be a single character string.")
    }
    # Create output directory if needed
    if (!dir.exists(save_map)) {
      dir.create(
        save_map,
        recursive = TRUE
      )
    }
  }
  if (!is.numeric(width) ||
      length(width) != 1 ||
      is.na(width) ||
      width <= 0) {
    stop("'width' must be a positive number.")
  }

  if (!is.numeric(height) ||
      length(height) != 1 ||
      is.na(height) ||
      height <= 0) {
    stop("'height' must be a positive number.")
  }

  if (!is.numeric(dpi) ||
      length(dpi) != 1 ||
      is.na(dpi) ||
      dpi <= 0) {
    stop("'dpi' must be a positive number.")
  }

  # Convert coordinates safely to numeric
  latitude <- suppressWarnings(
    as.numeric(as.character(data[[lat]]))
  )

  longitude <- suppressWarnings(
    as.numeric(as.character(data[[long]]))
  )


  # Check coordinate conversion
  if (any(is.na(latitude))) {
    stop(
      "Missing or non-numeric values were found in the latitude field."
    )
  }

  if (any(is.na(longitude))) {
    stop(
      "Missing or non-numeric values were found in the longitude field."
    )
  }


  # Check geographic ranges
  if (any(latitude < -90 | latitude > 90)) {
    stop("Latitude values must range from -90 to 90.")
  }

  if (any(longitude < -180 | longitude > 180)) {
    stop("Longitude values must range from -180 to 180.")
  }

  # Construct standardized internal data.frame
  data_work <- data.frame(
    Station   = as.character(data[[station_field]]),
    Latitude  = latitude,
    Longitude = longitude,
    stringsAsFactors = FALSE
  )

  # Check station IDs
  if (any(is.na(data_work$Station)) ||
      any(data_work$Station == "")) {
    stop("Missing or empty station IDs were found.")
  }

  # Species information
  if (is.null(species_field)) {
    data_work$Species <- "All records"
  } else {
    data_work$Species <- as.character(
      data[[species_field]]
    )
  }

  # GET ALL CAMERA-STATION COORDINATES
  # Consequently, a station without records for a particular
  # species can still appear on the map with n = 0.
  station_coords <- aggregate(
    cbind(Latitude, Longitude) ~ Station,
    data = data_work,
    FUN = mean
  )
  # APPLY SPECIES FILTER
  if (!is.null(species_filter)) {
    species_filter <- unique(
      as.character(species_filter)
    )
    species_available <- unique(
      data_work$Species[
        !is.na(data_work$Species)
      ]
    )

    # Species requested but not present
    species_missing <- species_filter[
      !species_filter %in% species_available
    ]

    if (length(species_missing) > 0) {
      warning(
        paste0(
          "The following species were not found: ",
          paste(species_missing, collapse = ", ")
        )
      )
    }
    # Keep requested species
    data_work <- data_work[
      data_work$Species %in% species_filter,
      ,
      drop = FALSE
    ]

    if (nrow(data_work) == 0) {
      stop(
        "There are no records for the selected species."
      )
    }
  }

  # SPECIES TO MAP
  species_list <- unique(
    data_work$Species[
      !is.na(data_work$Species) &
        data_work$Species != ""
    ]
  )

  if (length(species_list) == 0) {
    stop("No valid species records were found.")
  }

  maps <- vector(
    mode = "list",
    length = length(species_list)
  )

  names(maps) <- species_list

  # LOOP THROUGH SPECIES
  for (sp in species_list) {
    # Species records
    data_sp <- data_work[
      !is.na(data_work$Species) &
        data_work$Species == sp,
      ,
      drop = FALSE
    ]
    frequency_sp <- aggregate(
      x = rep(1L, nrow(data_sp)),
      by = list(
        Station = data_sp$Station
      ),
      FUN = sum
    )
    names(frequency_sp)[2] <- "n"
    result_sp <- station_coords
    position <- match(
      result_sp$Station,
      frequency_sp$Station
    )
    result_sp$n <- frequency_sp$n[position]
    # Stations without detections = 0
    result_sp$n[
      is.na(result_sp$n)
    ] <- 0
    result_sp$Species <- sp
    # Detection status
    result_sp$Status <- ifelse(
      result_sp$n == 0,
      "No records",
      "Records"
    )
    # Set factor order explicitly
    result_sp$Status <- factor(
      result_sp$Status,
      levels = c(
        "No records",
        "Records"
      )
    )
    # Convert to sf
    result_sf <- sf::st_as_sf(
      result_sp,
      coords = c(
        "Longitude",
        "Latitude"
      ),
      crs = 4326
    )
    # Bounding box
    bbox <- sf::st_bbox(result_sf) |> st_as_sfc() |> st_as_sf() |>
      st_buffer(0.05) |> st_bbox()

    # Download map tiles
    tiles <- tryCatch(
      maptiles::get_tiles(
        sf::st_as_sfc(bbox),
        provider = provider,
        zoom = zoom
      ),
      error = function(e) {stop(
        paste0(
          "Basemap tiles could not be downloaded: ",
          e$message
        )
      )
      }
    )
    # Determine useful size legend breaks
    max_records <- max(
      result_sp$n,
      na.rm = TRUE
    )
    if (max_records <= 5) {

      size_breaks <- 0:max_records

    } else {

      size_breaks <- pretty(
        c(0, max_records),
        n = 4
      )

      size_breaks <- unique(
        round(size_breaks)
      )

      size_breaks <- size_breaks[
        size_breaks >= 0 &
          size_breaks <= max_records
      ]
    }
    # Always include zero in legend
    size_breaks <- sort(
      unique(
        c(0, size_breaks)
      )
    )
    # CREATE MAP
    result_map <- ggplot2::ggplot() +
      # Satellite imagery
      tidyterra::geom_spatraster_rgb(
        data = tiles) +
      # Camera stations
      ggplot2::geom_sf(
        data = result_sf,
        ggplot2::aes(
          size = n,
          fill = Status
        ),
        shape = 21,
        colour = "white",
        alpha = 0.90,
        stroke = 0.8,
        show.legend = TRUE
      ) +
      ggplot2::scale_size_continuous(
        name = "Number of records",
        range = c(3, 10),
        breaks = size_breaks,
        limits = c(
          0,
          max_records
        ),
        guide = ggplot2::guide_legend(
          order = 1,
          title.position = "top",
          override.aes = list(
            fill = "#606060",
            colour = "white",
            alpha = 1,
            shape = 21,
            stroke = 0.8
          )
        )
      ) +
      ggplot2::scale_fill_manual(
        name = "Detection status",
        values = c(
          "No records" = "#0072B2",
          "Records"    = "#D55E00"
        ),
        drop = FALSE,
        guide = ggplot2::guide_legend(
          order = 2,
          title.position = "top",
          override.aes = list(
            size = 5,
            alpha = 1,
            colour = "white"
          )
        )
      ) +
      ggplot2::coord_sf(
        xlim = xlim,
        ylim = ylim,
        expand = FALSE
      ) +
      ggspatial::annotation_scale(
        location = "bl",
        width_hint = 0.25,
        text_cex = 0.7,
        line_width = 0.7,
        pad_x = grid::unit(
          0.4,
          "cm"
        ),
        pad_y = grid::unit(
          0.4,
          "cm"
        )
      ) +
      ggspatial::annotation_north_arrow(
        location = "tl",
        which_north = "true",
        height = grid::unit(
          1,
          "cm"
        ),
        width = grid::unit(
          1,
          "cm"
        ),
        pad_x = grid::unit(
          0.4,
          "cm"
        ),
        pad_y = grid::unit(
          0.4,
          "cm"
        ),
        style = ggspatial::north_arrow_minimal
      ) +
      ggplot2::labs(
        title = paste(
          "Camera-trap records:",
          sp
        ),
        x = NULL,
        y = NULL
      ) +
      ggplot2::theme_minimal(
        base_size = 11
      ) +
      ggplot2::theme(
        # Geographic grid
        panel.grid.major = ggplot2::element_line(
          colour = scales::alpha(
            "white",
            0.60
          ),
          linewidth = 0.35,
          linetype = "dotted"
        ),
        panel.grid.minor = ggplot2::element_blank(),
        # Coordinate labels
        axis.text = ggplot2::element_text(
          size = 8,
          colour = "grey20"
        ),
        axis.ticks = ggplot2::element_line(
          colour = "grey40",
          linewidth = 0.3
        ),
        # Title
        plot.title = ggplot2::element_text(
          size = 15,
          face = "bold",
          colour = "grey10",
          margin = ggplot2::margin(
            b = 6
          )
        ),
        # Legend
        legend.position = "right",
        legend.title = ggplot2::element_text(
          size = 10,
          face = "bold"
        ),
        legend.text = ggplot2::element_text(
          size = 9
        ),
        legend.key = ggplot2::element_blank(),
        legend.spacing.y = grid::unit(
          0.15,
          "cm"
        ),
        legend.background = ggplot2::element_rect(
          fill = scales::alpha(
            "white",
            0.92
          ),
          colour = "grey70",
          linewidth = 0.4
        ),

        legend.margin = ggplot2::margin(
          8,
          10,
          8,
          10
        ),
        # Background
        panel.background = ggplot2::element_blank(),

        plot.background = ggplot2::element_rect(
          fill = "white",
          colour = NA
        ),
        plot.margin = ggplot2::margin(
          10,
          10,
          10,
          10
        )
      )
    # Store map
    maps[[sp]] <- result_map
    # Print map
    print(
      result_map
    )

    if(!is.null(save_map)) {
      species_name <- gsub(
        "[^[:alnum:]_-]+",
        "_",
        sp
      )

      filename <- file.path(
        save_map,
        paste0(
          "records_",
          species_name,
          ".png"
        )
      )
      ggplot2::ggsave(
        filename = filename,
        plot = result_map,
        width = width,
        height = height,
        dpi = dpi,
        bg = "white"
      )
    }
  }
  # RETURN MAPS
  return(maps)
}
