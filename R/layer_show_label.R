#' Label simple feature objects in a location
#'
#' Label markers, streets, areas, or other simple feature objects using any of
#' the following geoms: "text", "label", "textsf", "labelsf", "text_repel", or
#' "label_repel". This function is experimental and may be adopted into the
#' overedge package in the future.
#'
#' Note:
#'
#' - Unlike in some overedge package functions, fn is applied to the whole data; not a
#' subset of the data based on location.
#' - For this function, dist and unit are both used by [overedge::st_clip()] (not by
#' [layer_location_data])
#'
#' The function also overrides the label aesthetics to hide the colored letters
#' that would otherwise appear when using a theme legend.
#'
#' @param data Data to use for label creation
#' @param location Location to label (if not specified the data is assumed to conver the whole location);
#' @param fn Function to apply to data before creating labels; can be used in the creation of the label_col
#' @param label_col Label column name
#' @param geom A geom to use "text", "label", "textsf", "labelsf", "text_repel", or "label_repel"
#' @param mapping Aesthetic mapping, Default: NULL
#' @param union If TRUE, group by label_col and union geometry, Default: FALSE
#' @inheritParams overedge::st_clip
#' @param ... Additional parameters passed to [overedge::layer_location_data]
#' @seealso
#'  \code{\link[overedge]{st_clip}},\code{\link[overedge]{layer_location_data}}
#' @rdname layer_show_label
#' @aliases layer_label
#' @export
#' @importFrom dplyr summarise
#' @importFrom sf st_union
#' @importFrom overedge st_clip layer_location_data
#' @importFrom ggplot2 guides guide_legend aes
layer_show_label <- function(data,
                             location = NULL,
                             geom = "text",
                             fn = NULL,
                             label_col = NULL,
                             mapping = NULL,
                             union = FALSE,
                             clip = NULL,
                             dist = NULL,
                             unit = NULL,
                             ...) {
  if (!is.null(fn)) {
    fn <- rlang::as_function(fn)
    data <- fn(data)
  }

  if (union) {
    data <- group_by_col(data = data, groupname_col = label_col) %>%
      dplyr::summarise(geometry = sf::st_union(geometry))
  }

  if (is.character(clip)) {
    # FIXME: st_clip likely should be passed to fn for layer_location_data to apply clip to any subset of the data specified by location rather than the whole area
    clip_fn <- rlang::as_function(~ overedge::st_clip(x = .x, clip = clip, dist = dist))
  } else if (!is.null(clip)) {
    clip_fn <- rlang::as_function(~ overedge::st_clip(x = data, clip = clip, dist = dist))
  } else {
    clip_fn <- NULL
  }

  # TODO: Double-check this works
  if (geom %in% c("text", "label", "textsf", "labelsf", "text_repel", "label_repel")) {
    label_layer <-
      overedge::layer_location_data(
        location = location,
        fn = clip_fn,
        data = data,
        mapping = mapping,
        geom = geom,
        label_col = label_col,
        ...
      )
  }

  list(
    label_layer,
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = ggplot2::aes(label = "")
      )
    )
  )
}
