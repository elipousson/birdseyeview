#' Add maps of simple features into rows of a `gt` table
#'
#' Add maps of simple features into rows of a `gt` table with {ggplot2}. Function naming convention borrowed from {gtExtras} <https://jthomasmock.github.io/gtExtras/reference/gt_img_rows.html>
#'
#' @param gt_object An existing gt table object of class gt_tbl. The gt_object must be convertible to an sf object with sf::st_as_sf so do not drop the geometry before creating an sf object.
#' @param mapping aesthetic mapping passed to layer_location_data, Default: NULL
#' @param fill Fill (used for polygon geometry), Default: NA
#' @param color Color (used for polygon, point, and line geometry), Default: 'black'
#' @param size Color (used for polygon, point, and line geometry), Default: 1
#' @param linetype Color (used for polygon and line geometry), Default: 'solid'
#' @param height The absolute height (px) of the map in the table cell
#' @param asp The aspect ratio of the map in the table cell.
#' @param fn Optional rowwise function that results in ggplot2 map the replaces geometry column.
#' @family gt
#' @export
#' @importFrom tibble rowid_to_column
#' @importFrom sf st_as_sf st_geometry_type
#' @importFrom dplyr group_nest group_by
#' @importFrom ggplot2 ggplot
#' @importFrom overedge layer_location_data layer_neatline
#' @importFrom rlang as_function
#' @importFrom purrr map
#' @importFrom gt text_transform cells_body ggplot_image
#' @importFrom tidyselect all_of
gt_geom_sf_rows <-
  function(gt_object,
           mapping = NULL,
           fill = NA,
           color = "black",
           size = 1,
           linetype = "solid",
           height = 100,
           asp = 1,
           fn = NULL,
           ...) {
    sf_object <-
      tibble::rowid_to_column(
        sf::st_as_sf(gt_object[["_data"]])
      )

    geom_col <-
      attr(sf_object, "sf_column")

    geom_type <-
      sf::st_geometry_type(sf_object)

    sf_nested <-
      dplyr::group_nest(
        dplyr::group_by(sf_object, .data[["rowid"]]),
        keep = TRUE
      )

    if (is.null(fn)) {
      if (all(sapply(geom_type, function(x) {
        x %in% c("POLYGON", "MULTIPOLYGON")
      }))) {
        fn <- ~ ggplot2::ggplot() +
          overedge::layer_location_data(
            data = .x,
            mapping = mapping,
            fill = fill,
            color = color,
            size = size,
            linetype = linetype,
            ...
          ) +
          overedge::layer_neatline(data = .x, asp = asp, expand = TRUE, color = NA)
      } else if (all(sapply(geom_type, function(x) {
        x %in% c("POINT", "MULTIPOINT")
      }))) {
        fn <- ~ ggplot2::ggplot() +
          overedge::layer_location_data(
            data = .x,
            mapping = mapping,
            color = color,
            size = size,
            ...
          ) +
          overedge::layer_neatline(data = .x, asp = asp, expand = TRUE, color = NA)
      } else if (all(sapply(geom_type, function(x) {
        x %in% c("LINESTRING", "MULTILINESTRING")
      }))) {
        fn <- ~ ggplot2::ggplot() +
          overedge::layer_location_data(
            data = .x,
            mapping = mapping,
            color = color,
            size = size,
            linetype = linetype,
            ...
          ) +
          overedge::layer_neatline(data = .x, asp = asp, expand = TRUE, color = NA)
      }
    }

    fn <- rlang::as_function(fn)

    gt_map_col <- purrr::map(
      sf_nested$data,
      ~ fn(.x)
    )

    gt::text_transform(
      data = gt_object,
      locations = gt::cells_body(columns = tidyselect::all_of(geom_col)),
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
    )
  }
