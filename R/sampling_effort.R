#' Sampling effort
#'
#' Estimate Camera-Trap Sampling Effort
#'
#' `effort_estimate()` calculates the sampling effort (e.g., camera-days or
#' camera-nights) from camera-trap detection data using two alternative methods.
#' The function relies on the minimum and maximum sampling dates recorded either
#' across all cameras (method 1) or for each camera individually (method 2).
#' Optionally, sampling periods such as wet and dry seasons can be considered to
#' avoid counting inactive intervals.
#'
#' The function is designed to support standardized effort calculations in
#' ecological studies based on camera-trap data, enabling comparisons among
#' stations, seasons, and years. Depending on the chosen method, the resulting
#' effort may represent an upper- or lower-bound estimate, providing flexibility
#' for different study designs and data availability.
#' @param input `data.frame`. (*required*) Table containing the camera-trap data.
#'
#' @param method `integer`. (*required*) Method used to estimate sampling effort.
#' Two options are available:
#'
#' \itemize{
#'   \item **`1`**: Does not discriminate between the minimum and maximum date
#'   of each individual camera. The input table is split by year. For each year,
#'   the earliest (first photo) and latest (last photo) dates across all cameras
#'   are computed. The number of days between these two dates is multiplied by
#'   the number of cameras. This process is repeated for each year, and
#'   camera-days/nights are summed across years.
#'
#'     \itemize{
#'       \item If `season_field` is not `NULL`, each year is further subdivided
#'       by sampling periods (e.g., dry vs. wet season), preventing the inclusion
#'       of inactive periods.
#'       \item This method may **overestimate** sampling effort, because it
#'       assumes all cameras operated for the same number of days.
#'     }
#'
#'   \item **`2`**: Similar to method 1, but the calculation is performed
#'   **per camera**. For each camera, the number of days between its first and
#'   last photo is computed, then results are summed across cameras.
#'
#'     \itemize{
#'       \item This method may **underestimate** sampling effort, as it depends
#'       entirely on the records actually captured by each camera.
#'     }
#' }
#'
#' @param station_field `character`. Name of the column in the `data.frame` that
#' contains the unique identifier for each station. If `NULL`, all records are
#' assumed to belong to a single station.
#'
#' @param season_field `character`. Name of the column representing any category
#' used to distinguish among different sampling periods (e.g., wet vs. dry season).
#' If `NULL`, sampling is assumed to be continuous and all records belong to a
#' single period.
#'
#' @param date_field `character`. Name of the column containing the date of each
#' record. If `NULL`, all records are assumed to occur on the same day.
#'
#' @param date_format `character`. Date format used in the data. The default is
#' `"%d/%m/%Y"`, where `%d` = day, `%m` = month, and `%Y` = year, separated by
#' `"/"`. Change the format according to your dataset.
#'
#' Examples:
#' `"02/23/2023"` → `"%m/%d/%Y"`
#' `"02-23-2023"` → `"%m-%d-%Y"`
#' @examples
#' \dontrun{
#' datab <- read.csv("sipecam_final.csv", header = TRUE)
#'
#' samp_eff <- sampling_effort(input = datab,
#'                             method = 1,
#'                             station_field = "Station",
#'                             season_field = NULL,
#'                             date_field = "Date",
#'                             date_format = "%d/%m/%Y")
#' samp_eff
#'
#' samp_eff <- sampling_effort(input = datab,
#'                             method = 2,
#'                             station_field = "Station",
#'                             season_field = NULL,
#'                             date_field = "Date",
#'                             date_format = "%d/%m/%Y")
#' samp_eff
#'
#' samp_eff <- sampling_effort(input = datab,
#'                             method = 1,
#'                             station_field = "Station",
#'                             season_field = "Muestreo",
#'                             date_field = "Date",
#'                             date_format = "%d/%m/%Y")
#' samp_eff
#'
#' samp_eff <- sampling_effort(input = datab,
#'                             method = 2,
#'                             station_field = "Station",
#'                             season_field = "Muestreo",
#'                             date_field = "Date",
#'                             date_format = "%d/%m/%Y")
#' samp_eff
#' }
#' @export
sampling_effort <- function(input = NULL,
                            method = 1,
                            station_field = "Station",
                            season_field = NULL,
                            date_field = "Date",
                            date_format = "%d/%m/%Y"){
  if(is.null(date_field)){
    warning("no date field, assumed to be from the same day")
    input$datef <- format(Sys.Date(), "%d/%m/%Y") |> as.character(x = _)
  } else {
    if(isFALSE(date_field %in% names(input))){
      stop("check the date_field parameter")
    } else {
      date.1 <- strptime(input[[date_field]], date_format)
      input[date_field] <- format(date.1, "%d/%m/%Y") |> as.character(x = _)
    }
  }

  if(is.null(station_field)){
    input$station <- 1; station_field <- "station"
  } else {
    if(isFALSE(station_field %in% names(input))){
      stop("check the station_field parameter")
    }
  }
  input.1 <- input[,c(date_field, station_field, season_field)]; row.names(input.1) <- NULL
  input.1[[date_field]] <- as.Date(input.1[[date_field]], format="%d/%m/%Y")

  if(method == 1){
    #NO se discrimina entre tiempos min & max x camara
    #Para cada año se obtienen dias (Min,Max de cada año) * total camaras en el año, OPC=dentro de cada año por season * total camaras en el season
    tots <- sapply(unique(format(input.1[[date_field]], "%Y")), function(x){
      x.1 <- input.1[which(format(input.1[[date_field]], "%Y") == x),]
      if(nrow(x.1) > 0){
        if(is.null(season_field)){
          cam <- length(unique(x.1[[station_field]]))
          x.1 <- x.1[[date_field]]; x.1 <- as.numeric(diff(x.1[order(x.1)][c(1, length(x.1))]))
          if(x.1 == 0){#misma fecha
            x.1 <- 1
          }
          x.1 <- x.1 * cam; return(x.1)
        } else {
          x.1 <- sapply(unique(x.1[[season_field]]), function(y){
            y.1 <- x.1[which(x.1[[season_field]] == y),]
            if(nrow(y.1)>0){
              cam <- length(unique(y.1[[station_field]]))
              y.1 <- y.1[[date_field]]; y.1 <- as.numeric(diff(y.1[order(y.1)][c(1, length(y.1))]))
              if(y.1 == 0){#misma fecha
                y.1 <- 1
              }
              y.1 <- y.1 * cam; return(y.1)
            } else {
              return(0)
            }
          }) |> sum()
          return(x.1)
        }
      } else {
        return(0)
      }
    }) |> sum()
  } else {
    #Se discrimina entre tiempos min & max x camara
    #Para cada año y cada camara se obtienen dias (Min,Max), OPC=dentro de cada año y para camara dias por season
    tots <- sapply(unique(format(input.1[[date_field]], "%Y")), function(x){
      x.1 <- input.1[which(format(input.1[[date_field]], "%Y") == x),]
      if(nrow(x.1) > 0){
        if(is.null(season_field)){
          x.1 <- sapply(unique(x.1[[station_field]]), function(y){
            y.1 <- x.1[[date_field]][which(x.1[[station_field]] == y)]
            y.1 <- as.numeric(diff(y.1[order(y.1)][c(1, length(y.1))]))
            if(y.1 == 0){#misma fecha
              y.1 <- 1
            }
            return(y.1)
          })|> sum()
          return(x.1)
        } else {
          x.1 <- sapply(unique(x.1[[station_field]]), function(k){
            k.1 <- x.1[which(x.1[[station_field]] == k),]
            k.1 <- sapply(unique(k.1[[season_field]]), function(y){
              y.1 <- k.1[which(k.1[[season_field]] == y),]
              if(nrow(y.1)>0){
                y.1 <- y.1[[date_field]]; y.1 <- as.numeric(diff(y.1[order(y.1)][c(1, length(y.1))]))
                if(y.1 == 0){#misma fecha
                  return(1)
                } else {
                  return(y.1)
                }
              } else {
                return(0)
              }
            }) |> sum()
            return(k.1)
          })|> sum()
          return(x.1)
        }
      } else {
        return(0)
      }
    }) |> sum()
  }
  return(tots)
}
