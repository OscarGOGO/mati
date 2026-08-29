#' Video metadata to record table
#'
#' Use the video metadata to create a table with data from the camera trap records,
#' including the date, time, and tags.
#' @param folderpath `character`. The path to the folder with the video metadata;
#' if there are subfolders inside, the search will include them.
#' @param tags `character`. The name of the column that holds video tags. It usually includes
#' the species name but might also contain other tags, such as the number of individuals or
#' a specific behavior.
#' @param date_field `character`. Name of the column containing the **date and time** of
#' each record.
#' @param date_format `character`. Date format used in the records. The default is
#' `"%d/%m/%Y"`, where `%d` = day, `%m` = month, and `%Y` = year, separated by `"/"`.
#' If your dates use a different structure, adjust accordingly.
#' Examples:
#' `"2026/02/17 01:29:23"` → `"%Y/%m/%d %H:%M:%S"`
#' `"02:23:2023 01:29:23"` → `"%m-%d-%Y %H:%M:%S"`
#' @param ext `character`. Metadata file extension, default = ".xmp" (see exifr::read_exif())
#'
#' @examples
#' \dontrun{
#' #Carpeta con subcarpetas
#' test <- video_meta_table(folderpath = "D:/Camaras_trampaR/Videos_prueba2",
#'                          tags = "TagsList",
#'                          date_field = "MetadataDate",
#'                          date_format = "%Y:%m:%d %H:%M:%S",
#'                          ext = ".xmp")
#' head(test)
#' }
#' @export
#' @importFrom purrr map_dfc
#' @importFrom exifr read_exif
#' @importFrom data.table rbindlist

video_meta_table <- function(folderpath,
                             tags = "TagsList",
                             date_field = "MetadataDate",
                             date_format = "%Y:%m:%d %H:%M:%S",
                             ext = ".xmp"){
  lf <- list.files(folderpath, pattern = paste0(ext, "$"),
                   full.names = TRUE, recursive = TRUE)
  # Leer metadatos directamente del archivo
  meta_exif_final <- exifr::read_exif(lf)
  tabla <- data.frame(Directory = dirname(lf),
                      FileName = gsub("\\.xmp$", "", basename(lf)))

  if(tags %in% names(meta_exif_final)){
    tag <- meta_exif_final[[tags]]
    tag2 <- list()
    spl <- c("/", "|")[which(grepl(c("/"), tag[[1]]), grepl(c("|"), tag[[1]]))]
    if(length(spl) > 0){
      for(i in 1:length(tag)){
        i.1 <- tag[[i]] |> strsplit(x = _, spl)
        i.1 <- map_dfc(i.1, function(x){
          x.2 <- data.frame(nom = x[[2]])
          names(x.2)[1] <- x[[1]]; return(x.2)})
        tag2[[i]] <- i.1
      }
      tag <- as.data.frame(data.table::rbindlist(tag2, fill = TRUE))
    } else {
      if(is.list(tag)){
        tag <- do.call(c, tag)
      }
    }
    tabla <- cbind(tabla, tag)
  } else {
    warning(paste0("No se encontro la columna ", tags))
  }


  if(date_field %in% names(meta_exif_final)){
    date.1 <- strptime(meta_exif_final[[date_field]], date_format)
    tabla$Date <- format(date.1, "%d/%m/%Y") |> as.character(x = _)
    if(grepl("%H|%M|%S", date_format)){
      tabla$time <- format(date.1, "%H:%M:%S") |> as.character(x = _)
    }
  }
  return(tabla)
}
