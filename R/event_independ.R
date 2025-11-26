#' Identify Independent Camera-Trap Events
#'
#' `event_independ()` processes camera-trap detection data and identifies
#' independent events based on a user-defined temporal threshold. The function
#' allows independence to be evaluated by station, species, and (optionally)
#' number of individuals in the photo. Records occurring within the specified
#' time window are grouped together, and only one record is retained per group,
#' either the first occurrence or the one with the highest number of individuals
#' if an `indiv_field` is provided.
#'
#' This function is useful for standardizing camera-trap datasets by reducing
#' temporal autocorrelation, estimating relative abundance indices, preparing
#' data for activity pattern analyses, and ensuring consistency across studies
#' that define independent detections using temporal rules (e.g., 30 minutes,
#' 1 hour, 24 hours). The function accommodates multiple stations, species,
#' time zones, and various date–time formats.
#' @param input `data.frame`. A table containing the camera-trap records.
#'
#' @param station_field `character`. Name of the column in `input` that uniquely
#' identifies each camera station. If set to `NULL`, all records are assumed to
#' come from a single station.
#'
#' @param species_field `character`. Name of the column containing the species
#' identifier. If `NULL`, all records are assumed to belong to the same species.
#'
#' @param indiv_field `character`. Name of the column indicating the number of
#' individuals recorded in each photograph. See Example 3 for details.
#'
#' @param date_field `character`. Name of the column containing the **date** of
#' each record. If set to `NULL`, all records are assumed to belong to the same day.
#'
#' @param date_format `character`. Date format used in the records. The default is
#' `"%d/%m/%Y"`, where `%d` = day, `%m` = month, and `%Y` = year, separated by `"/"`.
#' If your dates use a different structure, adjust accordingly.
#' Examples:
#' `"02/23/2023"` → `"%m/%d/%Y"`
#' `"02-23-2023"` → `"%m-%d-%Y"`
#'
#' @param time_field `character`. Name of the column containing the **time** of
#' each record. The required format is always `"%H:%M:%S"` (Hour:Minute:Second).
#'
#' @param tz `character`. Time zone used for date–time parsing. The default is `"UTC"`
#' (recommended), but other time zones may be specified depending on the country
#' or region of the study.
#'
#' @param independent_time `numeric`. Minimum time required between records to
#' consider them independent events. The value is interpreted according to the unit
#' specified in `independent_units`. For example, if `independent_units = "weeks"`
#' and `independent_time = 1`, then two records are considered independent only if
#' at least one week has passed. Default is `1`.
#'
#' @param independent_units `character`. Time unit used to define independence
#' between events. Options are `"secs"`, `"mins"`, `"hours"`, `"days"`, or `"weeks"`.
#' Default is `"hours"`.
#' @examples
#' \dontrun{
#' datab <- read.csv("base_barranca.csv", header = TRUE)
#'
#' #Example 1. Use a 1-hour threshold to define independent events for each
#' #species and station.
#'
#' Example1 <- event_independ(input = datab,
#'                            station_field = "Station",
#'                            species_field = "Species",
#'                            date_field = "Date",
#'                            date_format = "%d/%m/%Y",
#'                            time_field = "Time",
#'                            independent_time = 1,
#'                            independent_units = "hours")
#' Example1
#'
#' #Example 2. Use a 2-day threshold to define independent events for each
#' #species and station.
#'
#' Example2 <- event_independ(input = datab,
#'                            station_field = "Station",
#'                            species_field = "Species",
#'                            date_field = "Date",
#'                            date_format = "%d/%m/%Y",
#'                            time_field = "Time",
#'                            independent_time = 2,
#'                            independent_units = "days")
#' Example2
#'
#' #Example 3. Use a 1-hour threshold to define independent events for each
#' #species and station, giving priority to records with more individuals.
#' #For instance, if three records (a, b, c) fall within a one-hour interval
#' #and the number of individuals is a = 1, b = 3, and c = 2, then the first
#' #record (a) is retained when indiv_field = NULL.
#' #If a field for the number of individuals is provided and indiv_field is not NULL,
#' #then record b is retained because it has the highest number of individuals.
#'
#' #Example with fake data: add a new column containing random values of 2 and 3 individuals.
#' datab$Individuos <- 1
#' datab$Individuos[sample(1:nrow(datab), 100)] <- sample(c(2, 3), 100, replace = TRUE)
#'
#' Example3 <- event_independ(input = datab,
#'                            station_field = "Station",
#'                            species_field = "Species",
#'                            indiv_field = "Individuos",
#'                            date_field = "Date",
#'                            date_format = "%d/%m/%Y",
#'                            time_field = "Time",
#'                            independent_time = 1,
#'                            independent_units = "hours")
#' Example3
#' }
#' @export

event_independ <- function(input = NULL,
                           station_field = NULL,
                           species_field = NULL,
                           individ_field = NULL,
                           date_field = NULL,
                           date_format = "%d/%m/%Y",
                           time_field = NULL,
                           tz = "UTC",
                           independent_time = 1,
                           independent_units = "hours"){
  if(is.null(station_field)){
    input$station <- 1; station_field <- "station"
  } else {
    if(isFALSE(station_field %in% names(input))){
      stop("check the station_field parameter")
    }
  }

  if(!is.null(individ_field)){
    if(isFALSE(grepl(individ_field, names(input)))){
      stop("Review the name of the individ_field parameter")
    }
  }

  if(is.null(species_field)){
    input$speciesf <- 1; species_field <- "speciesf"
  } else {
    if(isFALSE(species_field %in% names(input))){
      stop("check the species_field parameter")
    }
  }

  if(is.null(date_field)){
    warning("no date field, assumed to be from the same day")
    input$datef <- format(Sys.Date(), "%d/%m/%Y") |> as.character(x = _)
    date_field <- "datef"
  } else {
    if(isFALSE(date_field %in% names(input))){
      stop("check the date_field parameter")
    } else {
      date.1 <- strptime(input[[date_field]], date_format)
      input[date_field] <- format(date.1, "%d/%m/%Y") |> as.character(x = _)

      if(grepl("%H|%M|%S", date_format)){
        time.temp <- format(date.1, "%H:%M:%S") |> as.character(x = _)
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

  input <- split(input, input[[station_field]])
  result <- lapply(input, function(x){
    x.1 <- lapply(split(x, x[[species_field]]), function(y){
      sp.i <- data.frame(id = 0,#no. de fila
                         time = as.POSIXct(paste(y[[date_field]],
                                                 y[[time_field]]),
                                           tryFormats = paste0("%d/%m/%Y ",
                                                               c("%H:%M:%S",
                                                                 "%H:%M",
                                                                 "%H:%S",
                                                                 "%M:%S")),
                                           tz = tz))
      if(!is.null(individ_field)){
        sp.i$ind <- y[[individ_field]]; sp.i$ind[which(is.na(sp.i$ind))] <- 1
      }
      y$temp.join <- sp.i$time
      sp.i <- sp.i[order(sp.i$time),]; sp.i$id <-  1:nrow(y)
      y <- y[order(y$temp.join),]
      #Parameters
      ids <- sp.i$id; conservar <- 1
      k = 1 ; i = 1 ; j = 1

      #loop
      repeat{
        k = i + 1
        if(k <= nrow(sp.i)){
          j = conservar[length(conservar)]
          dif <- difftime(sp.i[k, 2], sp.i[j, 2], units = independent_units) |>
            as.numeric(x = _)

          if(dif > independent_time){
            conservar <- append(conservar, ids[k]) |> unique(x = _)
          } else {
            if(!is.null(individ_field)){
              if(sp.i[k, 3] > sp.i[j, 3]){
                conservar[length(conservar)] <- k
              }
            }
          }
          i = k
        } else {
          break
        }
      }

      y <- y[conservar,]; y$temp.join <- NULL
      return(y)
    }) |> do.call(rbind, args = _)
    return(x.1)
  }) |> do.call(rbind, args = _)
  rownames(result) <- NULL; result$time.temp.1672 <- NULL
  return(result)
}
