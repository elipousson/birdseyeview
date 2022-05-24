#' Add a marker layer to a map with or without numbered markers
#'
#' If get is `TRUE`, groupname_col, group_meta, crs, and fn is all passed on to
#' [overedge::make_markers]
#'
#' The number parameter is not currently supported so the number_col parameter
#' is not implemented.
#'
#' @inheritParams overedge::make_markers
#' @param get If `TRUE`, pass data to [overedge::make_markers]
#' @param number If `TRUE`, number markers using [layer_number_markers()] (not
#'   currently supported)
#' @param style Style; defaults to `NULL` for [layer_show_markers()] (supports
#'   "facet"); defaults to "roundrect" for [layer_number_markers()],
#' @param ... Additional parameters passed to [layer_group_data()]
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
#' @md
#' @export
#' @importFrom overedge layer_location_data
#' @importFrom ggplot2 facet_wrap
layer_show_markers <- function(data,
                               mapping = NULL,
                               geom = "sf",
                               style = NULL, # "facet",
                               get = FALSE,
                               groupname_col = NULL,
                               group_meta = NULL,
                               crs = NULL,
                               fn = NULL,
                               number = FALSE,
                               number_col = NULL,
                               scale = NULL,
                               ...) {
  if (get) {
    data <-
      overedge::make_markers(
        data = data,
        groupname_col = groupname_col,
        group_meta = group_meta,
        crs = crs,
        fn = fn
      )
  }

  if (overedge::is_geom_type(x = data)$POINTS) {
    if (!is.null(groupname_col)) {
      mapping <-
        modify_mapping(
          mapping = mapping,
          color = groupname_col
        )
    }
  } else if (!is.null(groupname_col)) {
    mapping <-
      modify_mapping(
        mapping = mapping,
        fill = groupname_col
      )
  }

  if (!is.null(style) && (style == "facet")) {
    list(
      overedge::layer_location_data(data = data, mapping = mapping, ...),
      ggplot2::facet_wrap(~ .data[[groupname_col]])
    )
  } else {
    list(
      overedge::layer_location_data(
        data = data,
        geom = geom,
        mapping = mapping,
        ...
      ),
      scale
    )
  }
}

#' @rdname layer_show_markers
#' @name layer_number_markers
#' @param number_col Name of column with numbers; defaults to NULL.
#' @param size Marker size, Default: 5
#' @inheritParams overedge::number_features
#' @param ... Additional parameters passed to `overedge::layer_location_data`
#'   (include label.size, label.padding, and label.r to define alternate style)
#' @export
#' @importFrom ggplot2 aes
#' @importFrom rlang list2
#' @importFrom purrr list_modify zap
#' @importFrom usethis ui_stop
#' @importFrom utils modifyList
#' @importFrom overedge layer_location_data
#' @importFrom dplyr arrange mutate row_number
layer_number_markers <- function(data,
                                 mapping = NULL,
                                 number_col = NULL,
                                 groupname_col = NULL,
                                 number_by = FALSE,
                                 size = 5,
                                 style = "roundrect",
                                 geom = "label",
                                 sort = "lon",
                                 desc = NULL,
                                 scale = NULL,
                                 ...) {
  if (is.null(number_col)) {
    if (number_by) {
      data <- overedge::number_features(data = data, col = groupname_col, sort = sort, desc = desc)
    } else {
      data <- overedge::number_features(data = data, col = NULL, sort = sort, desc = desc)
    }

    number_col <- "number"
  }

  mapping <-
    modify_mapping(
      mapping = mapping,
      label = number_col
    )

  if (!is.null(groupname_col)) {
    mapping <-
      modify_mapping(
        mapping = mapping,
        fill = groupname_col
      )
  }

  params <- rlang::list2(...)

  if ("roundrect" %in% style) {
    label.size <- 0.0
    label.padding <- ggplot2::unit(size / 10, "lines")
    label.r <- label.padding * 1.5

    # Make sure to remove any alternate values from the params
    params <-
      purrr::list_modify(
        params,
        list(
          label.size = purrr::zap(),
          label.padding = purrr::zap(),
          label.r = purrr::zap()
        )
      )
  } else if (!all(c("label.size", "label.padding", "label.r") %in% names(params))) {
    usethis::ui_stop("layer_show_markers requires a valid style or that you pass the label.size, label.padding, and label.r through the ... parameter.")
  }

  # Set hjust and vust to defaults unless passed in params
  if (!all(c("hjust", "vjust") %in% names(params))) {
    hjust <- 0.5
    vjust <- 0.5
  } else {
    hjust <- params$hjust
    vjust <- params$vjust

    params <-
      purrr::list_modify(
        params,
        list(
          hjust = purrr::zap(),
          vjust = purrr::zap()
        )
      )
  }

  list(
    overedge::layer_location_data(
      data = data,
      geom = geom,
      mapping = mapping,
      size = size,
      label.size = label.size,
      label.padding = label.padding,
      label.r = label.r,
      hjust = hjust,
      vjust = vjust,
      ...
    ),
    scale,
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = ggplot2::aes(label = "")
      )
    )
  )
}
