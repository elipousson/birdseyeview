#' Create a map with an inset context map using ggplot2
#'
#' Works with `layer_show_context`
#'
#' @param map plot or map created with ggplot2
#' @param inset plot or map created with ggplot2
#' @param location sf or bbox object
#' @param context sf or bbox object with location context, Default: NULL
#' @param position inset map position, Default: 'bottomright'
#' @param nudge_x nudge X position of inset map, Default: 0
#' @param nudge_y nudge Y position of inset map, Default: 0
#' @param ... Additional parameters passed to layer_show_context
#' @return ggplot2 with inset map added using patchwork
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   map <-
#'     ggplot2::ggplot() +
#'     overedge::layer_location_data(data = mapbaltimore::get_area("neighborhood", "Harwood")) +
#'     overedge::layer_neatline(
#'       data = mapbaltimore::get_area("neighborhood", "Harwood"),
#'       asp = "8.5:11"
#'     )
#'
#'   location <-
#'     mapbaltimore::get_area("neighborhood", "Harwood")
#'
#'   context <-
#'     mapbaltimore::baltimore_city
#'
#'   inset_context_map(
#'     map = map,
#'     location = location,
#'     context = context,
#'     position = "bottomright",
#'     nudge_x = -0.05,
#'     nudge_y = 0.05
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[ggplot2]{ggplot}}
#'  \code{\link[patchwork]{inset_element}}
#' @rdname make_inset_map
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom patchwork inset_element
make_inset_map <-
  function(map = NULL,
           inset = NULL,
           location = NULL,
           context = NULL,
           position = "bottomright",
           scale = 1,
           nudge_x = 0,
           nudge_y = 0,
           ...) {
    if (!is.null(location) && !is.null(context)) {
      inset <-
        ggplot2::ggplot() +
        layer_show_context(
          data = location,
          context = context,
          ...
        )
    }

    map_position <-
      get_inset_position(
        scale = scale,
        position = position,
        nudge_x = nudge_x,
        nudge_y = nudge_y
        )


    map +
      patchwork::inset_element(
        p = inset,
        left = map_position$left,
        bottom = map_position$bottom,
        right = map_position$right,
        top = map_position$top,
        align_to = "full"
      )
  }

#' @noRd
get_inset_position <- function(scale = 1, position = NULL, nudge_x = 0, nudge_y = 0) {

  # FIXME: This is an incomplete implementation of a scale factor for an inset map
  # top, bottom, left, and right probably should all be based on scale as well
  top <- 0.5
  bottom <- 0.5
  left <- 0.5
  right <- 0.5
  width <- 0.25

  if (is.numeric(scale)) {
    width <- width * scale
  }

  if (grepl("top", position)) {
    top <- 1
    bottom <- bottom + width
  } else if (grepl("bottom", position)) {
    top <- width
    bottom <- 0
  }

  if (grepl("left", position)) {
    left <- 0
    right <- width
  } else if (grepl("right", position)) {
    left <- 1 - width
    right <- 1
  }

  left <- left + nudge_x
  right <- right + nudge_x
  top <- top + nudge_y
  bottom <- bottom + nudge_y

  list(
    "top" = top,
    "right" = right,
    "bottom" = bottom,
    "left" = left
  )
}
