#' Make group layers
#'
#' @inheritParams overedge::layer_location_data
#' @param groupname_col Group column name
#' @param layers If `TRUE`; return a list of layers; if `FALSE`; return a list
#'   of ggplot2 maps; defaults to `TRUE`
#' @param ... Additional parameters passed to `overedge::layer_location_data`
#' @rdname layer_group_data
#' @export
#' @importFrom dplyr group_by group_nest
#' @importFrom purrr map
#' @importFrom overedge layer_location_data
#' @importFrom ggplot2 ggplot
layer_group_data <- function(data,
                             mapping = NULL,
                             groupname_col = "group",
                             geom = "sf",
                             layers = TRUE,
                             ...) {
  if (!is.null(groupname_col)) {
    data <- data %>% dplyr::group_by({{ groupname_col }})
  }

  nested <- dplyr::group_nest(data, keep = TRUE)

  if (layers) {
    purrr::map(
      nested$data,
      ~ overedge::layer_location_data(
        mapping = mapping,
        data = .x,
        geom = geom,
        ...
      )
    )
  } else {
    purrr::map(
      nested$data,
      ~ ggplot2::ggplot() +
        overedge::layer_location_data(
          mapping = mapping,
          data = .x,
          geom = geom,
          ...
        )
    )
  }
}
