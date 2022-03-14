#' Sort and number markers
#'
#' @param data Marker data
#' @param groupname_col Group column name, Default: NULL
#' @param sort_col Sort column name, Default: 'lon'
#' @param number If TRUE, number the markers, Default: TRUE
#' @param sort_desc default FALSE
#' @return OUTPUT_DESCRIPTION
#' @rdname number_markers
#' @export
#' @importFrom overedge st_coords
#' @importFrom dplyr arrange mutate row_number
number_markers <- function(data, groupname_col = NULL, number = TRUE, sort = "lon", desc = FALSE) {
  number_col <- "number"

  if ((sort %in% c("lat", "lon")) && !all(c("lat", "lon") %in% names(data))) {
    data <-
      overedge::st_coords(
        data,
        geometry = "centroid",
        crs = 4326,
        drop = FALSE
      )
  }

  if (!is.null(groupname_col)) {
    data <- group_by_col(data, groupname_col = groupname_col)
    by_group <- TRUE
  } else {
    by_group <- FALSE
  }

  if (desc) {
    data <-
      dplyr::arrange(data, dplyr::desc(.data[[sort]]), .by_group = by_group)
  } else {
    data <-
      dplyr::arrange(data, .data[[sort]], .by_group = by_group)
  }


  if (number) {
    data <- dplyr::mutate(data, number = dplyr::row_number())
  }

  return(data)
}
