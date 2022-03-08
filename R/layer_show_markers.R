#' Add a marker layer to a map with or without numbered markers
#'
#' @inheritParams get_markers
#' @param get If `TRUE`, pass data to get_markers.
#' @param number If `TRUE`, number markers using `layer_number_markers`
#' @param style Style; defaults to `NULL` for `layer_show_markers` (supports
#'   "facet"); defaults to "roundrect" for `layer_number_markers`,
#' @param ... Additional parameters passed to `layer_group_data`
#' @return ggplot2 layers
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ggplot2::ggplot() +
#'     layer_number_markers(
#'       data = mapbaltimore::parks,
#'       groupname_col = "park_district"
#'     )
#' }
#' }
#' @name layer_show_markers
#' @export
#' @importFrom overedge layer_location_data
#' @importFrom ggplot2 facet_wrap
layer_show_markers <- function(data,
                               mapping = NULL,
                               get = TRUE,
                               groupname_col = "group",
                               group_meta = NULL,
                               style = NULL, # "facet",
                               crs = NULL,
                               fn = NULL,
                               number = FALSE,
                               number_col = NULL,
                               ...) {
  if (get) {
    data <-
      get_markers(
        data = data,
        groupname_col = groupname_col,
        group_meta = group_meta,
        crs = crs,
        fn = fn
      )
  }

  if (numbered) {
    # FIXME: Add in numbering
  }

  if (!is.null(style) && (style == "facet")) {
    list(
      overedge::layer_location_data(data = data, mapping = mapping, ...),
      ggplot2::facet_wrap(~ .data[[groupname_col]])
    )
  } else {
    layer_group_data(data = data, groupname_col = groupname_col, mapping = mapping, ...)
  }
}

#' @rdname layer_show_markers
#' @name layer_number_markers
#' @param number_col Name of column with numbers; defaults to NULL.
#' @param size Marker size, Default: 5
#' @param ... Additional parameters passed to `overedge::layer_location_data`
#'   (include label.size, label.padding, and label.r to define alternate style)
#' @export
#' @importFrom dplyr group_by mutate row_number
#' @importFrom ggplot2 aes unit
#' @importFrom rlang list2
#' @importFrom overedge layer_location_data
#' @importFrom magrittr inset
layer_number_markers <- function(data,
                                 number_col = NULL,
                                 groupname_col = NULL, # "group",
                                 size = 5,
                                 style = "roundrect",
                                 ...) {
  data <- group_by_col(data, groupname_col = groupname_col)

  if (is.null(number_col)) {
    number_col <- "number"

    data <- dplyr::mutate(
      data,
      number = dplyr::row_number()
    )
  }

  if (!is.null(groupname_col)) {
    mapping <-
      ggplot2::aes(
        label = .data[[number_col]],
        fill = .data[[groupname_col]]
      )
  } else {
    mapping <-
      ggplot2::aes(label = .data[[number_col]])
  }

  dots <- rlang::list2(...)

  if ("roundrect" %in% style) {
    label.size <- 0.0
    label.padding <- ggplot2::unit(size / 10, "lines")
    label.r <- label.padding * 1.5
  } else {
    label.size <- dots$label.size
    label.padding <- dots$label.padding
    label.r <- dots$label.r
  }

  dots <-
    magrittr::inset(
      dots,
      c("label.size", "label.padding", "label.r"),
      NULL
    )

  if (!all(c("hjust", "vjust") %in% names(dots))) {
    hjust <- 0.5
    vjust <- 0.5
  }

  list(
    overedge::layer_location_data(
      data = data,
      geom = "label",
      mapping = mapping,
      size = size,
      label.size = label.size,
      label.padding = label.padding,
      label.r = label.r,
      hjust = hjust,
      vjust = vjust,
      ...
    ),
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = ggplot2::aes(label = "")
      )
    )
  )
}
