#' Assign a Minimum Number of Individuals (MNI) Using a Space–Time HMM
#'
#' `assign_MNI_hmm()` assigns detections from camera-trap data to putative
#' individuals and estimates the Minimum Number of Individuals (MNI) for a focal
#' species. The function uses a discrete-time, discrete-space movement model:
#' movement between camera stations is governed by a transition matrix derived
#' from inter-station distances and a spatial scale parameter `alpha`, while
#' temporal dynamics are controlled by the time-step `dt`. For each new
#' detection, the function compares the probability that it belongs to an
#' existing individual against a minimum probability threshold (`min_prob`);
#' detections with low assignment probability are treated as new individuals.
#'
#' Optionally, a sensitivity analysis can be performed over a vector of `alpha`
#' values (`alpha_sensitivity`). In that case, the function repeatedly
#' recomputes the MNI for each `alpha` and generates diagnostic plots that
#' highlight where the estimator is most sensitive to spatial scale and where
#' it begins to saturate.
#'
#' @param data `data.frame`. Table containing the camera-trap records.
#'
#' @param station_field `character`. Name of the column in `data` that uniquely
#' identifies each camera station. If set to `NULL`, all records are assumed to
#' originate from a single station.
#'
#' @param species_col `character`. Name of the column containing the species
#' identifier. If `NULL`, all records are assumed to belong to the same species.
#'
#' @param date_field `character`. Name of the column containing the **date** of
#' each record. If set to `NULL`, all records are assumed to occur on the same day.
#'
#' @param date_format `character`. Date format of the values in `date_field`.
#' The default is `"%d/%m/%Y"`, where `%d` = day, `%m` = month, and `%Y` = year,
#' separated by `"/"`. Adjust this format according to your dataset.
#'
#' Examples:
#' `"02/23/2023"` → `"%m/%d/%Y"`
#' `"02-23-2023"` → `"%m-%d-%Y"`
#'
#' @param time_field `character`. Name of the column containing the **time** of
#' each record. The expected format is always `"%H:%M:%S"` (Hour:Minute:Second).
#'
#' @param tz `character`. Time zone used when parsing date–time values. The
#' default is `"UTC"`, although other time zones may be specified depending on
#' the study region.
#'
#' @param species_filter `character`. Optional species name used to filter
#' the dataset (e.g., `"Odocoileus virginianus"`). If `NULL`, all species are included.
#'
#' @param lat `character`. Name of the column containing the latitude for each
#' record or station.
#'
#' @param long `character`. Name of the column containing the longitude for each
#' record or station.
#'
#' @param alpha `numeric`. Spatial scale parameter controlling the ease of
#' movement between cameras (spatial smoothing or movement scale, typically in meters).
#'
#' @param alpha_sensitivity `numeric` or `NULL`. Optional vector of alternative
#' `alpha` values used to perform a sensitivity analysis of the MNI estimator to
#' changes in spatial scale. For each value in `alpha_sensitivity`, the function
#' recomputes MNI internally and produces diagnostic plots showing how MNI varies
#' with `alpha`. When this argument is not `NULL`, the plots also highlight the
#' `max_change_point` and `saturation_point` along the MNI–`alpha` curve.
#'
#' @param dt `numeric`. Time-step size of the model, expressed in hours
#' (e.g., `1` for 1-hour steps).
#'
#' @param min_prob `numeric`. Minimum probability threshold used to determine
#' whether a new detection is assigned to an existing individual or triggers
#' the creation of a new individual.
#'
#' @return
#' A list with two elements:
#' \itemize{
#'   \item `data_with_ids`: a `data.frame` with the original records plus three
#'   additional columns: `indiv_id` (assigned individual identifier),
#'   `prob_last_ind` (probability of assignment to the selected existing individual),
#'   and `prob_new_ind` (complementary probability of creating a new individual).
#'
#'   \item `MNI`: a `data.frame` summarizing the estimated Minimum Number of
#'   Individuals for the focal species, including the columns `species`, `MNI`,
#'   `alpha`, `dt`, and `min_prob`.
#' }
#'
#' When `alpha_sensitivity` is not `NULL`, the function also performs a sensitivity
#' analysis of MNI across the supplied `alpha` values and generates a diagnostic
#' plot. In this plot:
#' \itemize{
#'   \item The **max_change_point** marks the range of `alpha` where the MNI
#'   estimator is most sensitive to changes in spatial scale (i.e., where the
#'   structure of the data “breaks” the most).
#'
#'   \item The **saturation_point** marks a value of `alpha` beyond which
#'   increasing `alpha` produces only small changes in MNI, indicating that the
#'   curve is approaching a plateau. From this point onward, the assumed
#'   connectivity is so high that most detections tend to be treated as distinct
#'   individuals.
#' }
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' datab <- read.csv("sipecam_final.csv", header = TRUE)
#'
#' Example1 <- event_independ(
#'   input             = datab,
#'   station_field     = "Station",
#'   species_field     = "Species",
#'   date_field        = "Date",
#'   date_format       = "%d/%m/%Y",
#'   time_field        = "Time",
#'   independent_time  = 1,
#'   independent_units = "hours"
#' )
#'
#' # Replace missing individual counts with 1 and keep a subset of columns
#' Example1$Individuos[is.na(Example1$Individuos)] <- 1
#' Example1 <- Example1[, c(1:2, 3:6, 9, 14:15, 20)]
#'
#' test <- assign_MNI_hmm(
#'   data           = Example1,
#'   station_field  = "Carpeta",
#'   species_col    = "Species",
#'   date_field     = "Date",
#'   date_format    = "%d/%m/%Y",
#'   time_field     = "Time",
#'   tz             = "UTC",
#'   species_filter = "Odocoileus virginianus",
#'   lat            = "Latitud",
#'   long           = "Longitud",
#'   alpha          = 300,                      # spatial scale (m)
#'   dt             = 1,                        # temporal step (hours)
#'   alpha_sensitivity = seq(200, 10000, 200),  # vector of alpha values
#'   min_prob       = 0.1
#' )
#' test
#' }
#' @importFrom sf st_as_sf st_transform st_distance
#' @importFrom ggplot2 ggplot geom_point geom_smooth geom_line geom_vline geom_label labs aes theme_minimal
#' @importFrom dplyr arrange mutate filter slice_max slice_min %>%
#' @importFrom expm %^%
#' @import patchwork
#' @export
assign_MNI_hmm <- function(data,
                           station_field   = NULL,
                           species_col  = NULL,
                           date_field = NULL,
                           date_format = "%d/%m/%Y",
                           time_field = NULL,
                           tz = "UTC",
                           species_filter = NULL,
                           lat = NULL,
                           long = NULL,
                           alpha = NULL,
                           alpha_sensitivity = NULL,
                           dt = NULL,
                           min_prob = 0.1
){
  . = NULL
  if (!requireNamespace("expm", quietly = TRUE)) {
    stop("Necesitas instalar el paquete 'expm' para usar assign_MNI_hmm().")
  }

  # Copia local
  df <- data

  # Filtrar especie si se especifica
  if (!is.null(species_filter)) {
    df <- df[df[[species_col]] == species_filter, , drop = FALSE]
  }

  if (nrow(df) == 0) {
    stop("No hay registros para la especie indicada (o el filtro dejó 0 filas).")
  }

  # Asegurar que datetime es POSIXct
  if(!inherits(df[[time_field]], "POSIXct")) {
    date.1 <- strptime(df[[date_field]], date_format)
    date.1 <- format(date.1, "%Y/%m/%d") |> as.character(x = _)
    df[[time_field]] <- as.POSIXct(paste0(date.1, " ", df[[time_field]]), tz = tz)
  }

  if(is.null(lat) | is.null(long)){
      message("It was not possible to perform the cleaning due to the distance between stations, the lat or long parameter is missing")
    } else {
      # Coord únicas por cámara
      coords <- df[, c(station_field, lat, long)]
      coords_unique <- coords[!duplicated(coords), ]
      cam_ids <- coords_unique[[station_field]]
      coords_sf <- st_as_sf(x = coords_unique,
                     coords = c(long, lat),
                     crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      coords_sf <- st_transform(coords_sf, "+proj=merc +units=m")
      dist_mat <- st_distance(coords_sf); attr(dist_mat, "units") <- NULL; class(dist_mat) <- setdiff(class(dist_mat),"units")
      rownames(dist_mat) <- coords_sf[[station_field]];colnames(dist_mat) <- coords_sf[[station_field]]

  }

  T1 <- build_T_from_dist(dist_mat, alpha)

  ###
  # Índice cámara -> posición en la matriz
  cam_index <- setNames(seq_along(cam_ids), cam_ids)

  # Ordenar detecciones por tiempo
  df <- df[order(df[[time_field]]), , drop = FALSE]

  # Columnas de salida
  df$indiv_id  <- NA_integer_
  df$prob_last_ind <- NA_real_
  df$prob_new_ind <- NA_real_

  # Estado de cada individuo (última detección)
  indiv_last_time <- as.POSIXct(character(0), tz = "UTC")
  indiv_last_cam  <- character(0)

  # Caché de potencias de T para no recalcular T^k todo el tiempo
  T_powers <- list()
  T_powers[["1"]] <- T1

  get_Tk <- function(k) {
    k_char <- as.character(k)
    if (!k_char %in% names(T_powers)) {
      # calcular y guardar
      T_powers[[k_char]] <<- expm::`%^%`(T1, k)
    }
    return(T_powers[[k_char]])
  }


  # Bucle principal

  for (r in seq_len(nrow(df))) {
    cam_r  <- df[[station_field]][r]
    time_r <- df[[time_field]][r]
    idx_cam_r <- cam_index[[as.character(cam_r)]]

    if (length(indiv_last_time) == 0) {
      # Primer individuo
      df$indiv_id[r]  <- 1L
      df$prob_last_ind[r] <- 1
      df$prob_new_ind[r] <- 1
      indiv_last_time <- c(indiv_last_time, time_r)
      indiv_last_cam  <- c(indiv_last_cam,  cam_r)
      next
    }

    # Diferencias de tiempo (horas) con el último registro de cada individuo
    dt_hours <- as.numeric(difftime(time_r, indiv_last_time, units = "hours"))

    # Solo consideramos individuos cuya última detección fue en el pasado (dt_hours >= 0)
    valid <- which(dt_hours >= 0)

    if (length(valid) == 0) {
      # Por seguridad, nuevo individuo
      new_id <- length(indiv_last_time) + 1L
      df$indiv_id[r]  <- new_id
      df$prob_last_ind[r] <- NA_real_
      df$prob_new_ind[r] <- NA_real_
      indiv_last_time <- c(indiv_last_time, time_r)
      indiv_last_cam  <- c(indiv_last_cam,  cam_r)
      next
    }

    dt_valid <- dt_hours[valid]

    # Número de pasos k por individuo
    k_vec <- pmax(1, round(dt_valid / dt))

    # Cálculo de probabilidades T^k[i->cam_r] para cada individuo válido
    P_vec <- numeric(length(valid))

    for (ii in seq_along(valid)) {
      ind_idx <- valid[ii]
      k <- k_vec[ii]
      Tk <- get_Tk(k) # revisar

      cam_last <- indiv_last_cam[ind_idx]
      idx_last <- cam_index[[as.character(cam_last)]]

      # prob. de pasar de cámara última de ese individuo a cámara actual en k pasos
      P_vec[ii] <- Tk[idx_last, idx_cam_r]
    }

    # Elegimos el individuo con mayor probabilidad
    best_idx_pos <- which.max(P_vec)
    best_P       <- P_vec[best_idx_pos]
    best_indiv   <- valid[best_idx_pos]

    if (best_P >= min_prob) {
      # Asignar a este individuo
      df$indiv_id[r]  <- best_indiv
      df$prob_last_ind[r] <- best_P
      df$prob_new_ind[r] <- 1-best_P
      indiv_last_time[best_indiv] <- time_r
      indiv_last_cam[best_indiv]  <- cam_r
    } else {
      # Probabilidad baja → nuevo individuo
      new_id <- length(indiv_last_time) + 1L
      df$indiv_id[r]  <- new_id
      df$prob_last_ind[r] <- best_P
      df$prob_new_ind[r] <- 1-best_P
      indiv_last_time <- c(indiv_last_time, time_r)
      indiv_last_cam  <- c(indiv_last_cam,  cam_r)
    }
  }

   # MNI = número de individuos
  MNI <- data.frame(species = species_filter,
                    MNI = length(unique(df$indiv_id)),
                    alpha       = alpha,
                    dt          = dt,
                    min_prob    = min_prob)
  result <- list(data_with_ids = df, MNI = MNI)

  if(!is.null(alpha_sensitivity)){
    MNI2 <- lapply(alpha_sensitivity, function(x){
      x.1 <- assign_MNI_hmm(data = data,
                            station_field   = station_field,
                            species_col  = species_col,
                            date_field = date_field,
                            date_format = date_format,
                            time_field = time_field,
                            tz = tz,
                            species_filter = species_filter,
                            lat = lat,
                            long = long,
                            alpha = x,
                            dt    = dt,
                            min_prob = min_prob)
      return(x.1[[2]])
    }) |> do.call(what = rbind, args = _)


    p1 <- ggplot(MNI2, aes(x = alpha, y = MNI)) +
      geom_point(size = 2, alpha = 0.7) +
      geom_smooth(method = "loess", span = 0.3, linewidth = 1) +
      theme_minimal(base_size = 14) +
      labs(
        title = "Minimum Number of Individuals (MNI) as a Function of α",
        x = expression(alpha~"(spatial scale parameter)"),
        y = "MNI"
      )

    df_mni2 <- MNI2 %>%
      arrange(alpha) %>%
      mutate(
        dMNI      = MNI - lag(MNI),                  # cambio en MNI
        dAlpha    = alpha - lag(alpha),              # cambio en alpha
        slope     = dMNI / dAlpha                    # pendiente aproximada
      )

    # quitar primer NA
    df_mni_valid <- df_mni2 %>% filter(!is.na(.data$slope))

    # 1) punto de máxima pendiente (cambio más fuerte)
    max_change_point <- df_mni_valid %>%
      slice_max(.data$slope, n = 1)

    # 2) punto de saturación aproximado:
    # primer alpha donde la pendiente cae por debajo de un umbral pequeño
    threshold <- 0.0005

    saturation_point <- df_mni_valid %>%
      filter(.data$slope <= threshold) %>%
      slice_min(alpha, n = 1)

    p2 <- ggplot(df_mni2, aes(x = alpha, y = MNI)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      # línea vertical en el punto de mayor cambio
      geom_vline(
        data = max_change_point,
        aes(xintercept = alpha),
        linetype = "dashed"
      ) +
      # línea vertical en el punto de saturación (si existe)
      geom_vline(
        data = saturation_point,
        aes(xintercept = alpha),
        linetype = "dotted"
      ) +
      # texto para el punto de mayor cambio
      geom_label(
        data = max_change_point,
        aes(
          x = alpha,
          y = MNI,
          label = paste0("Max change\nα = ", alpha, "\nMNI = ", MNI)
        ),
        vjust = -0.5,
        size = 3
      ) +
      # texto para el punto de saturación (solo si hay uno)
      geom_label(
        data = saturation_point,
        aes(
          x = alpha,
          y = MNI,
          label = paste0("Saturation\nα ≈ ", alpha, "\nMNI = ", MNI)
        ),
        vjust = -1,
        size = 3
      ) +
      theme_minimal(base_size = 14) +
      labs(
        title = "MNI as a function of α with change and saturation points",
        x = expression(alpha),
        y = "MNI"
      )

   p3 <- (p1 / p2) + plot_annotation(
      title = "MNI Sensitivity Analysis Across α",
      subtitle = "Top: MNI vs α \nBottom: MNI vs α with key points"
    )
    result[[3]] <- p3; names(result)[3] <- "Plot MNI vs α"
  }
  return(result)
}




#' Construir matriz de transición T (un paso de dt horas)
#'
#' @param dist_mat distance matrix
#' @param alpha numeric
#' @keywords internal
build_T_from_dist <- function(dist_mat, alpha) {
  K <- exp(-dist_mat / alpha)
  # Refuerzo de permanencia en la misma cámara (opcional)
  diag(K) <- 1
  Transition <- K / rowSums(K)
  return(Transition)
}



