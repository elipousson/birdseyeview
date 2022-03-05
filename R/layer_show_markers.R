#' Make marker map
#'
#' @param data Data for mapping
#' @name make_marker_map
#' @export
make_marker_map <- function(data,
                            mapping = ggplot2::aes(),
                            prep = TRUE,
                            groupname_col = "group",
                            groupmeta = NULL,
                            style = NULL, # "facet",
                            crs = NULL,
                            fn = NULL,
                            ...) {
  if (prep) {
    data <-
      prep_markers(
        data = data,
        groupname_col = groupname_col,
        groupmeta = groupmeta,
        crs = crs,
        fn = fn
      )
  }

  if (!is.null(style) && (style == "facet")) {
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = data, mapping = mapping) +
      ggplot2::facet_wrap(~group)
  } else {
    make_group_layers(data = data, groupname_col = groupname_col, mapping = mapping, ...)
  }
}

#' Make group layers
#'
#' @param layers defaults to FALSE
#' @inheritParams overedge::layer_location_data
#' @rdname make_marker_map
#' @name make_group_layers
#' @export
make_group_layers <- function(data,
                              mapping = ggplot2::aes(),
                              groupname_col = "group",
                              layers = FALSE,
                              ...) {
  if (layers) {
    purrr::map(
      unique(data[[groupname_col]]),
      ~ overedge::layer_location_data(
        data = dplyr::filter(data, .data[[groupname_col]] == .x),
        geom = "sf",
        mapping = mapping,
        ...
      )
    )
  } else {
    purrr::map(
      unique(data[[groupname_col]]),
      ~ ggplot2::ggplot() +
        overedge::layer_location_data(
          data = dplyr::filter(data, .data[[groupname_col]] == .x),
          geom = "sf",
          mapping = mapping,
          ...
        )
    )
  }
}

#' Prep markers for mapping
#'
#' @rdname make_marker_map
#' @name prep_markers
#' @export
prep_markers <- function(data = NULL,
                         groupname_col = "group",
                         groupmeta = NULL,
                         geocode = FALSE,
                         address_col = "address",
                         geometry = "point",
                         crs = NULL,
                         fn = NULL,
                         ...) {

  if (!geocode) {
    data <-
      overedge::get_location_data(
        data = data,
        crs = crs,
        ...
      )
  } else if ((address_col %in% names(data)) && is.data.frame(data)) {
    data <- data |>
      tidygeocoder::geo(address = {{address_col}}) |>
      overedge::df_to_sf(coords = c("long", "lat"), crs = crs)
  }

  if (!is.null(groupname_col)) {
    data <-
      dplyr::filter(data, !is.na({{ groupname_col }}))
  }

  if (!is.null(groupmeta)) {
    data <-
      dplyr::left_join(data, groupmeta, by = {{ groupname_col }})
  }

  # FIXME: Use geometry column to set geometry for all markers
  if (!is.null(cast)) {
    data <-
      sf::st_cast(data, to = "POINT")
  }

  if (!is.null(fn)) {
    fn <- rlang::as_function(fn)
    data <- fn(data)
  }

  return(data)
}
