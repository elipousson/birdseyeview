#' Make a key for a marker map
#'
#' Designed for use with maps created using `layer_show_markers()`
#'
#' @param data Data frame or sf object with marker data
#' @param title_col Title column name, Default: NULL
#' @param groupname_col Group column name, Default: NULL
#' @param number_col Number column name, Default: NULL. If NULL and data does
#'   not contain a column named "number", add a number column created using
#'   `dplyr::row_number()` function.
#' @rdname tbl_marker_key
#' @export
#' @importFrom dplyr mutate row_number
#' @importFrom overedge check_sf
#' @importFrom sf st_drop_geometry
#' @importFrom gt gt
tbl_marker_key <- function(data,
                           title_col = NULL,
                           groupname_col = NULL,
                           number_col = NULL) {

  data <- group_by_col(data, groupname_col = groupname_col)

  if (is.null(number_col) && !("number" %in% names(data))) {
    number_col <- "number"
    data <- dplyr::mutate(
      data,
      number = dplyr::row_number()
    )
  }

  if (overedge::check_sf(data)) {
    data <- sf::st_drop_geometry(data)
  }

  tbl <-
    gt::gt(
      data,
      rowname_col = number_col
    )

  return(tbl)
}
