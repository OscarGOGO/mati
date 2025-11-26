#' Classify Camera-Trap Detections into Diel Periods
#'
#' `periods_day()` classifies camera-trap records into diel periods
#' (twilight, day, night) based on the local times of dawn, sunrise, sunset,
#' and dusk. For each species, the function uses geographic coordinates and
#' dates to obtain sun times via \pkg{suncalc}, combines them with the
#' recorded time of each detection, and assigns each record to one of three
#' broad activity periods. It also creates a human-readable time interval
#' description for each record.
#'
#' The function returns a list with (1) a "raw" table containing the original
#' data plus the assigned diel period and time interval, and (2) a summary
#' table with the number of records per species and period (twilight, day,
#' night). The summary table can be used directly as input for diel activity
#' models, such as those in the \pkg{Diel.Niche} package.
#'
#' @param input `data.frame`. (*required*) Table containing the camera-trap data.
#'
#' @param species_field `character`. Name of the column in `input` that contains
#' the species identifier. If `NULL`, all records are assumed to belong to a
#' single species and an internal dummy column is created.
#'
#' @param date_field `character`. Name of the column containing the date of each
#' record. If `NULL`, all records are assumed to occur on the current date
#' (i.e., the function assigns `Sys.Date()` internally).
#'
#' @param date_format `character`. Date format used to parse the values in
#' `date_field`. The default is `"%d/%m/%Y"`, where:
#' \itemize{
#'   \item `%d` = day
#'   \item `%m` = month
#'   \item `%Y` = year
#' }
#' separated by `"/"`.
#' Modify this format if your dates use a different structure.
#'
#' Examples:
#' `"02/23/2023"` → `"%m/%d/%Y"`
#' `"02-23-2023"` → `"%m-%d-%Y"`
#'
#' If `date_format` also contains time components (e.g., `%H`, `%M`, `%S`),
#' these are extracted and used to populate `time_field` when needed.
#'
#' @param time_field `character`. Name of the column containing the **time** of
#' each record. The expected format is typically `"%H:%M:%S"` (Hour:Minute:Second).
#' If `NULL` and the `date_field` includes time information (e.g., in
#' `date_format`), `time_field` is created internally from that column.
#'
#' @param tz `character`. Time zone used to interpret dates and times, and to
#' request sun times from \pkg{suncalc}. The default is `"UTC"`, but it is
#' recommended to specify a region-specific time zone (e.g.,
#' `"America/Mexico_City"`). You can check your system time zone with
#' `Sys.timezone()` and list valid time zone names in R with `OlsonNames()`.
#'
#' @param lat `numeric` or `character`. Latitude information:
#' \itemize{
#'   \item If `numeric`, it must be a **single coordinate** (one value) used for
#'   all records (e.g., the centroid of the study area).
#'   \item If `character`, it must be the name of the column in `input` that
#'   contains the latitude for each record.
#' }
#'
#' @param long `numeric` or `character`. Longitude information:
#' \itemize{
#'   \item If `numeric`, it must be a **single coordinate** (one value) used for
#'   all records.
#'   \item If `character`, it must be the name of the column in `input` that
#'   contains the longitude for each record.
#' }
#'
#' @return
#' A `list` with two elements:
#' \itemize{
#'   \item `raw_table`: a `data.frame` containing all original columns from
#'   `input` plus two additional columns:
#'   \itemize{
#'     \item `Period`: a factor with values `"twilight"`, `"day"`, or `"night"`,
#'     indicating the diel period of each detection.
#'     \item `Time_period`: a character string describing the time interval
#'     relative to the sun events on that date (e.g., `"Between 06:15 and 18:45"`).
#'   }
#'
#'   \item `period_table`: a `data.frame` summarizing the number of detections
#'   per species and diel period. It has one row per species (row names are the
#'   species identifiers) and three columns:
#'   \itemize{
#'     \item `twilight`: number of detections classified as twilight.
#'     \item `day`: number of detections classified as day.
#'     \item `night`: number of detections classified as night.
#'   }
#' }
#'
#' @details
#' For each species, `periods_day()`:
#' \enumerate{
#'   \item Validates the presence of the required fields and the format of
#'   `lat`/`long` (either a single numeric coordinate or column names).
#'   \item Computes dawn, sunrise, sunset, and dusk using
#'   \code{suncalc::getSunlightTimes()} for each date and location.
#'   \item Combines the parsed date (`date_field`) and time (`time_field`) into a
#'   POSIXct timestamp in the specified time zone.
#'   \item Classifies each record into `"twilight"`, `"day"`, or `"night"`, and
#'   builds a descriptive time interval string (`Time_period`).
#'   \item Aggregates counts of detections per species and period in
#'   `period_table`.
#' }
#'
#' When more than one species is present, a progress bar is shown in the console.
#' @examples
#' \dontrun{
#' # Example 1: Records with per-record coordinates (lat/long columns)
#' datab <- read.csv("sipecam_final.csv", header = TRUE)
#'
#' # (Optional) First, derive independent events using your own function:
#' # ejemplo1 <- event_independ(
#' #   input             = datab,
#' #   station_field     = "Station",
#' #   species_field     = "Species",
#' #   date_field        = "Date",
#' #   date_format       = "%d/%m/%Y",
#' #   time_field        = "Time",
#' #   independent_time  = 1,
#' #   independent_units = "hours"
#' # )
#'
#' # Classify detections by diel period using record-level coordinates
#' test1 <- periods_day(
#'   input         = datab,
#'   species_field = "Species",
#'   date_field    = "Date",
#'   date_format   = "%d/%m/%Y",
#'   time_field    = "Time",
#'   tz            = "America/Mexico_City",
#'   lat           = "Latitud",
#'   long          = "Longitud"
#' )
#'
#' # Raw table with Period and Time_period
#' head(test1$raw_table)
#'
#' # Summary table: counts per species and period
#' test1$period_table
#'
#' # Example 2: Single reference coordinate for all records
#' datab2 <- read.csv("base_barranca.csv", header = TRUE)
#'
#' test2 <- periods_day(
#'   input         = datab2,
#'   species_field = "Species",
#'   date_field    = "Date",
#'   date_format   = "%d/%m/%Y",
#'   time_field    = "Time",
#'   tz            = "America/Mexico_City",
#'   lat           = 19.425889,   # reference latitude (decimal degrees)
#'   long          = -102.073574  # reference longitude (decimal degrees)
#' )
#'
#' # The structure is identical: a raw table and a period summary
#' head(test2$raw_table)
#' test2$period_table
#'
#' # Example 3 (conceptual): using period_table with Diel.Niche
#' # library(Diel.Niche)
#' # y <- as.matrix(test1$period_table["Odocoileus virginianus", ])
#' # out <- diel.fit(
#' #   y        = y,
#' #   hyp.set  = hyp.sets("Traditional"),
#' #   post.fit = TRUE,
#' #   n.chains = 3,
#' #   n.mcmc   = 3000,
#' #   burnin   = 500
#' # )
#' # out$bf.table
#' }
#' @importFrom suncalc getSunlightTimes
#' @importFrom dplyr rowwise mutate %>% case_when
#' @importFrom lubridate days
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
periods_day <- function(input = NULL,
                        species_field = NULL,
                        date_field = NULL,
                        date_format = "%d/%m/%Y",
                        time_field = NULL,
                        tz = "UTC",
                        lat = NULL,
                        long = NULL
){

  if(is.null(species_field)){
    input$speciesf <- 1; species_field <- "speciesf"
  } else {
    if(isFALSE(species_field %in% names(input))){
      stop("check the species_field parameter")
    }
  }

  if(any(isFALSE(date_field %in% names(input)) |  isFALSE(time_field %in% names(input)))){
    stop("Review the name of the date_field or time_field parameter")
  }

  if(any(is.null(lat) | is.null(long))){
    stop("Review the lat or long parameter")
  } else {
    if(is.numeric(lat)){
      if(length(lat)>1){
        stop("lat must be a unique element")
      }
    }
    if(is.numeric(long)){
      if(length(long)>1){
        stop("'long' must be a unique element")
      }
    }
  }

  if(is.null(date_field)){
    warning("no date field, assumed to be from the same day")
    input$datef <- as.Date(format(Sys.Date(), "%Y/%m/%d")) |> as.Date(x = _)
    date_field <- "datef"
  } else {
    if(isFALSE(date_field %in% names(input))){
      stop("check the date_field parameter")
    } else {
      date.1 <- strptime(input[[date_field]], date_format)
      input$datef <- format(date.1, "%Y/%m/%d") |> as.Date(x = _)
      date_field <- "datef"
      if(grepl("%H|%M|%S", date_format)){
        time.temp <- format(date.1, "%H:%M:%S")# |> as.character(x = _)
        if(is.null(time_field)){
          input$time.temp.1672 <- time.temp; time_field <- "time.temp.1672"
        } else if(time_field == date_field){
          input$time.temp.1672 <- time.temp; time_field <- "time.temp.1672"
        } else {
          input[time_field] <- time.temp
        }
      }
    }
  }

  if(is.null(time_field)){
    if(!grepl("%H|%M|%S", date_format)){
      stop("No time_field")
    }
  } else {
    if(isFALSE(time_field %in% names(input))){
      stop("check the time_field parameter")
    }
  }

  input.i <- split(input, input[[species_field]])

  if(length(input.i) > 1){
    pb <- txtProgressBar(0,length(input.i), style = 3)
  }
  result <- lapply(1:length(input.i), function(x){
    # Calcular horas de sol para cada día
    x.0 <- input.i[[x]]
    if(any(is.character(lat) | is.character(long))){
      x.1 <- x.0[,c(date_field, lat, long)]; names(x.1) <- c("date", "lat", "lon")
      sun_times <- getSunlightTimes(data = x.1,
                                    keep = c("dawn", "sunrise", "sunset", "dusk"),
                                    tz = tz)
    } else {
      sun_times <- getSunlightTimes(date = x.0$datef,
                                    lat = lat,
                                    lon = long,
                                    keep = c("dawn", "sunrise", "sunset", "dusk"),
                                    tz = tz)
    }
    x.2 <- data.frame(id = 1:nrow(x.0),#no. de fila
                      time = as.POSIXct(paste(x.0[[date_field]],
                                              x.0[[time_field]]),
                                        tryFormats = paste0("%Y-%m-%d ",
                                                            c("%H:%M:%S",
                                                              "%H:%M",
                                                              "%H:%S",
                                                              "%M:%S")),
                                        tz = tz))
    x.2 <- cbind(x.2, sun_times[,c("dawn","sunrise","sunset","dusk")])
    x.2 <- x.2 %>%
      rowwise() %>%
      mutate(
        Period = case_when(
          time >= dawn & time < sunrise ~ "twilight",
          time >= sunrise & time < sunset ~ "day",
          time >= sunset & time < dusk ~ "twilight",
          TRUE ~ "night"
        ),
        Time_period = case_when(
          time >= dawn & time < sunrise ~ paste("Between", format(dawn, "%H:%M"), "and", format(sunrise, "%H:%M")),
          time >= sunrise & time < sunset ~ paste("Between", format(sunrise, "%H:%M"), "and", format(sunset, "%H:%M")),
          time >= sunset & time < dusk ~ paste("Between", format(sunset, "%H:%M"), "and", format(dusk, "%H:%M")),
          TRUE ~ paste("Between", format(dusk, "%H:%M"), "and", format(dawn + days(1), "%H:%M"))
        )
      ) %>% as.data.frame()
    x.2 <- cbind(x.0, x.2[,c("Period", "Time_period")]); x.2$datef <- NULL
    x.3 <- data.frame("twilight" = length(which(x.2$Period == "twilight")),
                      "day" = length(which(x.2$Period == "day")),
                      "night" = length(which(x.2$Period == "night")))
    rownames(x.3) <- unique(x.0[[species_field]])

    if(length(input.i) > 1){
      setTxtProgressBar(pb, x)
    }
    return(list(x.2, x.3))
  })
  result <- lapply(1:2, function(x){do.call(rbind, lapply(result, function(y){y[[x]]}))})
  names(result) <- c("raw_table", "period_table")
  return(result)
}
