#' Estimate Capture Frequency from Camera-Trap Data
#'
#' `capture_frequency()` computes capture frequency indices for one or multiple
#' species based on camera-trap detection data and a standardized measure of
#' sampling effort. The function returns, for each species, its total number of
#' records, the number of stations where it was detected, and two standardized
#' capture-frequency metrics expressed per 100 camera-days.
#'
#' The first index (`Frequency-100 days`) is the classical capture frequency,
#' calculated as the total number of records divided by the sampling effort
#' (camera-days) and scaled to 100 camera-days. The second index
#' (`Frequency modified-100 days`) additionally weights this frequency by the
#' proportion of stations where the species was recorded, highlighting species
#' that are both frequently detected and widely distributed across stations.
#'
#' @param input `data.frame`. (*required*) Table containing the camera-trap records.
#'
#' @param species `character`. Name (or vector of names) of the species for which
#' capture frequency should be estimated. These values must match those in the
#' column specified by `species_field`. If `NULL`, capture frequency is computed
#' for **all species** in the dataset.
#'
#' @param species_field `character`. (*required*) Name of the column in `input`
#' that contains the species identifier. If `NULL`, all records are assumed to
#' belong to a single species.
#'
#' @param indiv_field `character`. Name of the column indicating the number of
#' individuals recorded in each photo. See Example 6 for details.
#'
#' @param station_field `character`. (*required*) Name of the column in `input`
#' that contains the unique identifier for each camera station. If `NULL`, all
#' records are assumed to originate from a single station.
#'
#' @param date_field `character`. Name of the column containing the **date** of
#' each record. If `NULL`, all records are assumed to occur on the same day.
#'
#' @param date_format `character`. Format of the dates in `date_field`.
#' Default is `"%d/%m/%Y"`, where:
#' \itemize{
#'   \item `%d` = day
#'   \item `%m` = month
#'   \item `%Y` = year
#' }
#' All components are separated by `"/"`. Adjust this format depending on your data.
#'
#' Examples:
#' `"02/23/2023"` → `"%m/%d/%Y"`
#' `"02-23-2023"` → `"%m-%d-%Y"`
#'
#' @param total_stations `numeric`. Total number of camera stations in the study.
#' If `NULL`, the number of stations is inferred directly from `station_field`.
#'
#' @param sampling_effort `numeric`. Sampling effort expressed in camera-days.
#' If `NULL`, effort is automatically estimated from the oldest and most recent
#' dates in the data (or according to the selected `method`).
#'
#' @param method `integer`. (*required*) Method used to estimate sampling effort.
#' Two options are available:
#'
#' \itemize{
#'   \item **`1`**: Does not differentiate among the minimum and maximum dates
#'   for each camera. The data are split by year. For each year, the earliest
#'   and latest dates across all cameras are used to estimate the number of
#'   sampling days, which is then multiplied by the number of cameras. Results
#'   are summed across years.
#'
#'     \itemize{
#'       \item If `season_field` is not `NULL`, effort is further subdivided
#'       within each year according to sampling periods (e.g., wet/dry season),
#'       preventing inactive periods from inflating effort.
#'       \item This method may **overestimate** sampling effort because it assumes
#'       all cameras were active during the entire date range.
#'     }
#'
#'   \item **`2`**: Similar to method 1 but calculated **per camera**. For each
#'   station, the number of days between its first and last photograph is computed
#'   and then summed across stations.
#'
#'     \itemize{
#'       \item This method may **underestimate** sampling effort because it relies
#'       only on days with registered detections.
#'     }
#' }
#'
#' @param season_field `character`. Name of the column representing categories
#' that distinguish different sampling periods (e.g., wet vs. dry season).
#' If `NULL`, sampling is assumed to be continuous.
#'
#' @param distance_clean `numeric`. Minimum distance (in meters) between camera
#' stations. Stations that are **farther apart** than this threshold will be
#' discarded. This parameter requires both `lat` and `long` to be defined.
#'
#' @param lat `character`. Name of the column containing the **latitude** for
#' each record or station.
#'
#' @param long `character`. Name of the column containing the **longitude** for
#' each record or station.
#'
#' @return A `data.frame` with one row per species and the following columns:
#' \itemize{
#'   \item `Species`: species name.
#'   \item `NRecords`: total number of records (or independent detections) for the species.
#'   \item `NStations`: total number of camera stations where the species was detected.
#'   \item `Frequency-100 days`: capture frequency standardized to 100 camera-days,
#'   typically computed as
#'   \eqn{(NRecords / sampling\_effort) * 100}.
#'   \item `Frequency modified-100 days`: capture frequency standardized to 100
#'   camera-days and weighted by the proportion of stations where the species
#'   occurred, emphasizing species that are both frequent and widespread across stations.
#' }
#'
#' @examples
#' \dontrun{
#' datab <- read.csv("sipecam_final.csv", header = TRUE)
#'
#'#Run function to find out about independent events
#' datab <- event_independ(input = datab,
#'                         station_field = "Station",
#'                         species_field = "Species",
#'                         date_field = "Date",
#'                         date_format = "%d/%m/%Y",
#'                         time_field = "Time",
#'                         independent_time = 1,
#'                         independent_units = "hours")
#'
#'#Example 1: All species. The values set for the station_field and
#'#sampling_effort parameters are not the actual values.
#'
#'Ejemplo1 <- freq_capture(input = datab,
#'                         species = NULL,
#'                         species_field = "Species",
#'                         station_field = "Station",
#'                         total_stations = 52,
#'                         sampling_effort = 4740,
#'                         distance_clean = NULL,
#'                         lat = NULL,
#'                         long = NULL)
#'
#'#Example 2: Species filter.
#'Example2 <- freq_capture(input = datab,
#'                         species = c("Bassariscus astutus", "Odocoileus virginianus", "Lynx rufus"),
#'                         species_field = "Species",
#'                         station_field = "Station",
#'                         total_stations = 52,
#'                         sampling_effort = 4740,
#'                         distance_clean = NULL,
#'                         lat = NULL,
#'                         long = NULL)
#'
#'#Example 3: Minimum distance filter between stations.
#'
#'Example3 <- freq_capture(input = datab,
#'                         species = NULL,
#'                         species_field = "Species",
#'                         station_field = "Station",
#'                         total_stations = 52,
#'                         sampling_effort = 4740,
#'                         distance_clean = 600,
#'                         lat = "Latitud",
#'                         long = "Longitud")
#'#Example 4: No sampling effort.
#'Example4 <- freq_capture(input = datab,
#'                         species = NULL,
#'                         species_field = "Species",
#'                         station_field = "Station",
#'                         date_field = "Date",
#'                         total_stations = 52,
#'                         sampling_effort = NULL,
#'                         method = 1,
#'                         season_field = NULL)
#'#Example 5: Consider the content of individuals per record. In a real case,
#'#the independent events function event_independ() should be run considering
#'#the “indiv_field” parameter and then the freq_capture()
#'#function should be applied.
#'
#'#Example with fake data. New field with random data with 2 and 3 individuals
#'datab$Individuos <- 1
#'datab$Individuos[sample(1:nrow(datab), 100)] <- sample(c(2, 3), 100, replace = TRUE)
#'
#'Ejemplo5 <- freq_capture(input = datab,
#'                         species = NULL,
#'                         species_field = "Species",
#'                         individ_field = "Individuos",
#'                         station_field = "Station",
#'                         date_field = "Date",
#'                         total_stations = 52,
#'                         sampling_effort = NULL,
#'                         method = 1,
#'                         season_field = NULL)
#'}
#' @importFrom sf st_as_sf st_transform st_distance
#' @export
freq_capture <- function(input = NULL,
                         species = NULL,
                         species_field = "Species",
                         individ_field = NULL,
                         station_field = "Station",
                         date_field = "Date",
                         date_format = "%d/%m/%Y",
                         total_stations = NULL,
                         sampling_effort = NULL,
                         method = 1,
                         season_field = NULL,
                         distance_clean = NULL,
                         lat = NULL,
                         long = NULL){

  if(isFALSE(grepl(station_field, names(input)))){
    stop("Review the name of the station_field parameter")
  }

  if(isFALSE(grepl(species_field, names(input)))){
    stop("Review the name of the species_field parameter")
  }

  if(!is.null(individ_field)){
    if(isFALSE(grepl(individ_field, names(input)))){
      stop("Review the name of the individ_field parameter")
    }
  }

  if(is.null(total_stations)){
    total_stations <- length(unique(input[[station_field]]))
  } else {
    if(length(unique(input[[station_field]])) > total_stations){
      stop("Review the total_stations parameter")
    }
  }

  if(is.null(sampling_effort)){
    sampling_effort <- sampling_effort(input = input,
                                       method = method,
                                       station_field = station_field,
                                       season_field = season_field,
                                       date_field = date_field,
                                       date_format = date_format)
  }

  if(!is.null(species)){
    input.i <- input[which(input[[species_field]] %in% species),]
    if(nrow(input.i) == 0){
      stop("There are no records for the species")
    }
  } else {
    input.i <- input
  }

  if(!is.null(distance_clean)){
    if(is.null(lat) | is.null(long)){
      message("It was not possible to perform the cleaning due to the distance between stations, the lat or long parameter is missing")
    } else {
      df <- input.i[, c(station_field, long, lat)]
      df <- lapply(unique(df[[station_field]]), function(x){
        x.1 <- df[which(df[[station_field]] == x),]; x.1 <- x.1[1,]
        return(x.1)
      }) |> do.call(rbind, args = _)
      df <- st_as_sf(x = df[, c(station_field, long, lat)],
                     coords = c(long, lat),
                     crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      df <- st_transform(df, "+proj=merc +units=m")
      dist_nodes <- st_distance(df); rownames(dist_nodes) <- df[[station_field]]
      colnames(dist_nodes) <- df[[station_field]]
      dist_nodes[lower.tri(dist_nodes, diag = TRUE)] <- NA; dist_nodes <- as.data.frame(as.table(dist_nodes))
      dist_nodes <- na.omit(dist_nodes); names(dist_nodes) <- c("From", "To", "Distance")
      dist_nodes[,3] <- as.numeric(dist_nodes[,3])
      conservar <- unique(df[[station_field]])
      #i.3 borrar; #i.2 = conservar
      for(j in 1:length(conservar)){
        if(conservar[j] != ""){
          i.1 <- dist_nodes[c(which(dist_nodes$From == conservar[j]), which(dist_nodes$To == conservar[j])),]
          i.2 <- i.1[i.1$Distance >= distance_clean,]; i.3 <- i.1[i.1$Distance < distance_clean,]
          i.2 <- as.character(unique(c(i.2$From, i.2$To)))
          i.3 <- as.character(unique(c(i.3$From, i.3$To)));  i.3 <- i.3[which(i.3 != conservar[j])]

          if(length(i.3 > 0)){
            conservar[which(conservar %in% i.3)] <- ""
          }
        }
      }
      conservar <- conservar[which(conservar != "")]
      input.i <- input.i[which(input.i[[station_field]] %in% conservar),]
    }
    if(nrow(input.i) == 0){
      stop("After filtering by distance, all stations were discarded.")
    }
  }
  sp <- unique(input.i[[species_field]])
  result <- lapply(1:length(sp), function(i){
    i.1 <- input.i[which(input.i[[species_field]] == sp[i]),]
    if(nrow(i.1) == 0){
      message(paste0("There are no records for the species: ", sp[i]))
      i.1 <- data.frame("species" = sp[i],
                        "NRecords" = 0,
                        "NStations" = 0,
                        "Frequency-100 days" = NA,
                        "Frequency modified-100 days" = NA, check.names = FALSE)
    } else {
      NE <- length(unique(i.1[[station_field]]))
      if(!is.null(individ_field)){
        i.1[[individ_field]][which(is.na(i.1[[individ_field]]))] <- 1
        i.1 <- sum(i.1[[individ_field]])
      } else {
        i.1 <- nrow(i.1)
      }
      FRQ <- i.1/sampling_effort * 100
      FRQ.i <- FRQ * (NE/total_stations)
      i.1 <- data.frame("species" = sp[i],
                        "NRecords" = i.1,
                        "NStations" = NE,
                        "Frequency-100 days" = round(FRQ, 4),
                        "Frequency modified-100 days" = round(FRQ.i, 4), check.names = FALSE)
    }
    return(i.1)
  }) |> do.call(what = rbind, args = _)

  if(!is.null(species)){
    if(length(sp) < length(species)){
      result.i <- data.frame("species" = species[which(!('%in%'(species, sp)))],
                             "NRecords" = 0,
                             "NStations" = 0,
                             "Frequency-100 days" = NA,
                             "Frequency modified-100 days" = NA, check.names = FALSE)
      result <- rbind(result, result.i); result <- result[order(result[[1]]),]
    }
  } else {
    species <- unique(result[[1]])
    if(length(sp) > length(species)){
      result.i <- data.frame("species" = sp[which(!('%in%'(sp, species)))],
                             "NRecords" = 0,
                             "NStations" = 0,
                             "Frequency-100 days" = NA,
                             "Frequency modified-100 days" = NA, check.names = FALSE)
      result <- rbind(result, result.i); result <- result[order(result[[1]]),]
    }
  }
  return(result)
}
