#' Create a map layer showing a location in context of another area or location
#'
#' Intended for use with inset_map function.
#'
#' @param context sf or bbox object for context area or a `geom` layer
#' @param context_aes list with aesthetic attributes for context area; must include fill and color; defaults to list(fill = "white", color = "black", ...)
#' @param neatline If `TRUE`, add a neatline layer to the returned layer
#' @inheritParams overedge::layer_location_data
#' @name layer_location_context
#' @export
#' @importFrom overedge check_sf layer_location_data layer_neatline
#' @importFrom sf st_crs
#' @importFrom ggplot2 theme_void
layer_location_context <- function(data = NULL,
                                   fill = "gray70",
                                   color = "black",
                                   context = NULL,
                                   context_aes = list(fill = "white", color = "black", alpha = 1, ...),
                                   crs = NULL,
                                   neatline = TRUE,
                                   ...) {
  if (overedge::check_sf(data)) {
    if (is.null(crs)) {
      crs <- sf::st_crs(data)
    }

    location_layer <-
      overedge::layer_location_data(data = data, fill = fill, color = color, crs = crs, ...)
  } else if ("gg" %in% class(data)) {
    location_layer <- data
  }
  check_context <- overedge::check_sf(context)
  if (check_context) {
    context_layer <- overedge::layer_location_data(data = context, fill = context_aes$fill, color = context_aes$color, alpha = context_aes$alpha, crs = crs, ...)
  } else if ("gg" %in% class(context)) {
    context_layer <- context
  }

  if (neatline && check_context) {
    neatline_layer <-
      list(
        overedge::layer_neatline(data = context, color = NA, bgcolor = "none", crs = crs),
        ggplot2::theme_void()
      )
  } else {
    neatline_layer <- NULL
  }

  list(
    context_layer,
    location_layer,
    neatline_layer
  )
}
