#' Generate Input Objects for the iNEXT Diversity Estimator
#'
#' `input_iNEXT()` transforms camera-trap detection data into the formats required
#' by the **iNEXT** package for estimating sample-size– and coverage-based species
#' diversity. The function supports two types of sampling effort:
#' **(a) days**, based on the temporal extent of sampling, and
#' **(b) cameras**, based on the number of active stations.
#'
#' Depending on the selected `type`, the function generates either a list of raw
#' incidence vectors (presence/absence per sampling unit) or a species-by-assemblage
#' frequency table. These formats correspond directly to `"incidence_raw"` and
#' `"incidence_freq"` data types expected by the `iNEXT()` function.
#'
#' @param input `data.frame`. (*required*) Table containing the camera-trap data.
#'
#' @param species_field `character`. (*required*) Name of the column in `input`
#' that contains the species identifier. If `NULL`, all records are assumed to
#' belong to a single species.
#'
#' @param effort `character`. Type of sampling effort to use: `"days"` or `"cameras"`.
#' Determines whether effort is defined by date range or by number of stations.
#'
#' @param assemblages_field `character`. Name of the column defining sampling units or
#' assemblage categories (e.g., wet/dry season, disturbed/undisturbed).
#' If `NULL`, all records are assumed to belong to a single assemblage.
#'
#' @param date_field `character`. Name of the column containing the date of each
#' record. If `NULL`, all records are assumed to occur on the same day.
#' **Only used when `effort = "days"`.**
#'
#' @param date_format `character`. Format used to parse the dates in `date_field`.
#' Default: `"%d/%m/%Y"`
#' where `%d` = day, `%m` = month, `%Y` = year, separated by `"/"`.
#' Adjust this if your date format differs.
#'
#' Examples:
#' `"02/23/2023"` → `"%m/%d/%Y"`
#' `"02-23-2023"` → `"%m-%d-%Y"`
#'
#' @param station_field `character`. Name of the column in `input` containing the
#' unique station identifier. If `NULL`, all records are assumed to originate from
#' a single station.
#' **Only used when `effort = "cameras"`.**
#'
#' @param type `character`. Output format to generate for **iNEXT**:
#' \itemize{
#'   \item `"raw"` — returns a list of incidence vectors (presence/absence per
#'   sampling unit). Corresponds to `datatype = "incidence_raw"` in `iNEXT`.
#'
#'   \item `"frequency"` — returns a species (rows) × assemblages (columns)
#'   frequency table. Corresponds to `datatype = "incidence_freq"` in `iNEXT`.
#' }
#'
#' @return
#' The output depends on the selected `type`:
#'
#' **If `type = "raw"`:**
#' A named `list`, where each element corresponds to one assemblage and contains a
#' **binary incidence vector** (1 = species detected, 0 = not detected).
#'
#' **If `type = "frequency"`:**
#' A `data.frame` (or matrix) with:
#' \itemize{
#'   \item Rows = species
#'   \item Columns = assemblages
#'   \item Values = number of records per species per assemblage
#' }
#'
#' This object is ready to be passed directly to:
#' \preformatted{
#' iNEXT(x, q = 0, datatype = "incidence_raw")
#' }
#' or
#' \preformatted{
#' iNEXT(x, q = 0, datatype = "incidence_freq")
#' }
#'
#' @examples
#' \dontrun{
#' datab <- read.csv("sipecam_final.csv", header = TRUE)
#'
#' # -------- Effort = days --------
#' # Raw input
#' library(iNEXT); library(ggplot2); library(gridExtra)
#'
#' test1 <- input_iNEXT(input = datab,
#'                      effort = "days",
#'                      species_field = "Species",
#'                      date_field = "Date",
#'                      date_format = "%d/%m/%Y",
#'                      type = "raw")
#'
#' # Apply iNEXT:
#' out <- iNEXT(test1, q = 0, datatype = "incidence_raw")
#' out
#'
#' # Plot iNEXT:
#' p1 <- ggiNEXT(out, type = 1) + theme_classic() +
#'       labs(x = "Camera-trap days", y = "Species richness")
#' p2 <- ggiNEXT(out, type = 2) + theme_classic() +
#'       labs(x = "Camera-trap days")
#' grid.arrange(p1, p2, nrow = 1)
#'
#' # Frequency input
#' test2 <- input_iNEXT(input = datab,
#'                      effort = "days",
#'                      species_field = "Species",
#'                      date_field = "Date",
#'                      date_format = "%d/%m/%Y",
#'                      type = "frequency")
#'
#' # Multiple assemblages
#' test3 <- input_iNEXT(input = datab,
#'                      effort = "days",
#'                      species_field = "Species",
#'                      assemblages = "Muestreo",
#'                      date_field = "Date",
#'                      date_format = "%d/%m/%Y",
#'                      type = "frequency")
#' out <- iNEXT(test3, q = 0, datatype = "abundance")
#' ggiNEXT(out, type = 1) + theme_classic() +
#'   labs(x = "Camera-trap days", y = "Species richness")
#'
#' # -------- Effort = cameras --------
#'
#' test4 <- input_iNEXT(input = datab,
#'                      effort = "cameras",
#'                      species_field = "Species",
#'                      station_field = "Station",
#'                      type = "raw")
#' }
#' @export
input_iNEXT <- function(input = NULL,
                        effort = c("days", "cameras"),
                        species_field = NULL,
                        assemblages_field = NULL,
                        date_field = NULL,
                        date_format = "%d/%m/%Y",
                        station_field = NULL,
                        type = c("raw", "frequency")){
  if(length(effort) > 1){
    effort = "days"
  }

  if(length(type) > 1){
    type = "raw"
  }

  if(effort == "days"){
    if(is.null(date_field)){
      stop("date_field missing")
    } else {
      if(isFALSE(date_field %in% names(input))){stop("date_field missing, correct the name")}
    }

    result <- input_iNEXT_days(input = input,
                               species_field = species_field,
                               assemblages = assemblages_field,
                               date_field = date_field,
                               date_format = date_format,
                               type = type)

  } else {
    if(is.null(station_field)){
      stop("station_field missing")
    } else {
      if(isFALSE(station_field %in% names(input))){stop("station_field missing, correct the name")}
    }

    result <- input_iNEXT_cameras(input = input,
                                  species_field = species_field,
                                  assemblages = assemblages_field,
                                  station_field = station_field,
                                  type = type)
  }

  return(result)
}

#'input_iNEXT_days
#'
#' @param input  data.frame
#' @param species_field character
#' @param assemblages character
#' @param date_field character
#' @param date_format character
#' @param type character
#' @importFrom iNEXT as.incfreq
#' @keywords internal
input_iNEXT_days <- function(input = NULL,
                             species_field = "Species",
                             assemblages = NULL,
                             date_field = "Date",
                             date_format = "%d/%m/%Y",
                             type = c("raw", "frequency")
){
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
  if(isFALSE(grepl(species_field, names(input)))){
    stop("Review the name of the species_field parameter")
  }

  if(!is.null(assemblages)){
    if(isFALSE(grepl(assemblages, names(input)))){
      stop("Review the name of the assemblages parameter")
    }
  }

  input.1 <- input[,c(date_field, species_field)]
  input.1$ids <- if(is.null(assemblages)){"1"} else {input[[assemblages]]}

  output <- lapply(unique(input.1$ids)[order(unique(input.1$ids))], function(x){
    x.1 <- input.1[which(input.1$ids == x),c(date_field, species_field)]; row.names(x.1) <- NULL

    if(nrow(x.1) == 0){
      message(paste0('No records for ', x))
      return(NULL)
    } else {
      x.1[[date_field]] <- as.Date(x.1[[date_field]], format="%d/%m/%Y")
      intervals <- seq(min(x.1[[date_field]]), max(x.1[[date_field]]), "1 day")
      matrix_sp <- matrix(0, nrow = length(unique(x.1[[species_field]])), ncol = length(intervals))
      for(i in 1:length(unique(x.1[[species_field]]))){
        i.1 <- unique(x.1[[species_field]])[i]; i.2 <- x.1[which(x.1[[species_field]] == i.1),]
        matrix_sp[i, which(intervals %in% i.2[[date_field]])] <- 1
      }

      if(type == "raw"){
        colnames(matrix_sp) <- paste0(1:ncol(matrix_sp), "-days")
        return(matrix_sp)
      } else {
        return(iNEXT::as.incfreq(matrix_sp))
      }
    }
  })
  names(output) <- unique(input.1$ids)[order(unique(input.1$ids))]
  return(output)
}


#'input_iNEXT_cameras
#'
#' @param input data.frame
#' @param species_field character
#' @param assemblages character
#' @param station_field character
#' @param type character
#' @importFrom iNEXT as.incfreq
#' @keywords internal
input_iNEXT_cameras <- function(input = NULL,
                                species_field = "Species",
                                assemblages = NULL,
                                station_field = "Station",
                                type = c("raw", "frequency")
){
  if(isFALSE(grepl(station_field, names(input)))){
    stop("Review the name of the station_field parameter")
  }
  if(isFALSE(grepl(species_field, names(input)))){
    stop("Review the name of the species_field parameter")
  }

  if(!is.null(assemblages)){
    if(isFALSE(grepl(assemblages, names(input)))){
      stop("Review the name of the assemblages parameter")
    }
  }

  input.1 <- input[,c(station_field, species_field)]
  input.1$ids <- if(is.null(assemblages)){"1"} else {input[[assemblages]]}

  output <- lapply(unique(input.1$ids)[order(unique(input.1$ids))], function(x){
    x.1 <- input.1[which(input.1$ids == x) ,c(station_field, species_field)]; row.names(x.1) <- NULL
    cameras <- unique(x.1[[station_field]])
    matrix_sp <- matrix(0, nrow = length(unique(x.1[[species_field]])), ncol = length(cameras))
    for(i in 1:length(unique(x.1[[species_field]]))){
      i.1 <- unique(x.1[[species_field]])[i]; i.2 <- x.1[which(x.1[[species_field]] == i.1),]
      matrix_sp[i, which(cameras %in% i.2[[station_field]])] <- 1
    }

    if(type == "raw"){
      colnames(matrix_sp) <- paste0(1:ncol(matrix_sp), "-cameras")
      return(matrix_sp)
    } else {
      return(iNEXT::as.incfreq(matrix_sp))
    }
  })
  names(output) <- unique(input.1$ids)[order(unique(input.1$ids))]
  return(output)
}

