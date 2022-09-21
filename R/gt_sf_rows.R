#' Add maps of simple features into rows of a `gt` table
#'
#' Add maps of simple features into rows of a `gt` table with {ggplot2}.
#' Function naming convention borrowed from [gtExtras::gt_img_rows]. Previously
#' named gt_geom_sf_rows.
#'
#' @param gt_object An existing gt table object of class gt_tbl or an sf object.
#'   If a gt_object must be convertible to an sf object with [sf::st_as_sf()]
#'   (e.g. geometry must be retained before passing to [gt::gt()]).
#' @param mapping aesthetic mapping passed to [maplayer::layer_location_data()], Default: `NULL`
#' @param fill Fill (used for polygon geometry), Default: `NA`
#' @param color Color (used for polygon, point, and line geometry), Default:
#'   'black'
#' @param size Color (used for polygon, point, and line geometry), Default: 1
#' @param linetype Color (used for polygon and line geometry), Default: 'solid'
#' @param height The absolute height (px) of the map in the table cell
#' @param asp The aspect ratio of the map in the table cell.
#' @param layer_fn Optional rowwise function that results in ggplot2 map the replaces
#'   geometry column.
#' @param sf_label Label used for column with ggplot2 map.
#' @family gt
#' @aliases gt_geom_sf_rows
#' @export
#' @importFrom tibble rowid_to_column
#' @importFrom sf st_as_sf st_geometry_type
#' @importFrom dplyr group_nest group_by
#' @importFrom ggplot2 ggplot
#' @importFrom rlang as_function
#' @importFrom purrr map
#' @importFrom gt text_transform cells_body ggplot_image
#' @importFrom tidyselect all_of
gt_sf_rows <-
  function(gt_object,
           mapping = NULL,
           fill = NA,
           color = "black",
           size = 1,
           linetype = "solid",
           height = 100,
           asp = 1,
           geom = "sf",
           layer_fn = NULL,
           sf_label = "map",
           ...) {

    sfext:::is_pkg_installed("maplayer", "elipousson/maplayer")

    if (sfext::is_sf(gt_object)) {
      sf_object <- gt_object
      gt_object <- gt::gt(gt_object)
    } else {
      sf_object <-
        tibble::rowid_to_column(
          sf::st_as_sf(gt_object[["_data"]])
        )
    }

    sf_col <-
      attr(sf_object, "sf_column")

    geom_type <-
      sf::st_geometry_type(sf_object)

    sf_nested <-
      dplyr::group_nest(
        dplyr::group_by(sf_object, .data[["rowid"]]),
        keep = TRUE
      )

    if (is.null(layer_fn)) {
      if (all(sapply(geom_type, function(x) {
        x %in% c("POLYGON", "MULTIPOLYGON")
      }))) {
        layer_fn <-
          ~ ggplot2::ggplot() +
            maplayer::layer_location_data(
              data = .x,
              mapping = mapping,
              geom = geom,
              fill = fill,
              color = color,
              size = size,
              linetype = linetype,
              ...
            ) +
            maplayer::layer_neatline(data = .x, asp = asp, expand = FALSE, color = NA)
      } else if (all(sapply(geom_type, function(x) {
        x %in% c("POINT", "MULTIPOINT")
      }))) {
        layer_fn <- ~ ggplot2::ggplot() +
          maplayer::layer_location_data(
            data = .x,
            mapping = mapping,
            geom = geom,
            color = color,
            size = size,
            ...
          ) +
          maplayer::layer_neatline(data = .x, asp = asp, expand = TRUE, color = NA)
      } else if (all(sapply(geom_type, function(x) {
        x %in% c("LINESTRING", "MULTILINESTRING")
      }))) {
        layer_fn <-
          ~ ggplot2::ggplot() +
            maplayer::layer_location_data(
              data = .x,
              mapping = mapping,
              geom = geom,
              color = color,
              size = size,
              linetype = linetype,
              ...
            ) +
            maplayer::layer_neatline(data = .x, asp = asp, expand = TRUE, color = NA)
      }
    }

    layer_fn <- rlang::as_function(layer_fn)

    gt_map_col <- purrr::map(
      sf_nested$data,
      ~ layer_fn(.x)
    )

    gt::text_transform(
      data = gt_object,
      locations = gt::cells_body(columns = tidyselect::all_of(sf_col)),
      fn = function(x) {
        purrr::map(
          gt_map_col,
          ~ gt::ggplot_image(
            plot_object = .x,
            height = height,
            aspect_ratio = asp
          )
        )
      }
    ) %>%
       gt::cols_label(
         .list = stats::setNames(list(sf_label), sf_col)
     )
  }
