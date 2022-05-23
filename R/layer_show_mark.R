#' Use ggforce to create an annotation layer using simple feature data
#'
#' Use [ggforce::geom_mark_rect()], [ggforce::geom_mark_circle()],
#' [ggforce::geom_mark_ellipse()], or [ggforce::geom_mark_hull()] to annotate
#' simple feature objects. This function modifies the geometry and sets the stat
#' to "st_coordinates" to make it easy to use these annotation geoms with simple
#' feature objects.
#'
#' @section Geometry conversion for MULTIPOLYGON or POLYGON data:
#'
#' If cast is `FALSE` and the data geometry type is MULTIPOLYGON or POLYGON, the
#' annotation uses a centroid for the annotation geometry. If cast is `TRUE`,
#' the data is converted to POINT geometry using [sf::st_cast()] and the
#' modified geometry passed on to selected geom.
#'
#' @section Setting label and description:
#'
#' Labels and descriptions can be included in the aesthetic mapping for the
#' layer consistent with the standard documented approaches for all four
#' functions.
#'
#'   Labels and descriptions also can be set in two non-standard ways:
#'
#'   - Setting label_col and/or or desc_col to character strings with the column
#'   names for labels and/or descriptions
#'   - Setting label_col and/or desc_col with the value of the label/description
#'
#'   If the first approach is used, label_col and desc_col can also can be
#'   created or modified by a function provided to the fn parameter. Otherwise,
#'   the columns must be present in the data to work. If the second approach is
#'   used, the length and order of vector provided to label_col and/or desc_col
#'   must match that length and order of the data (after the fn is applied).
#'
#' @param data A `sf`, `sfc`, or `bbox` object that can be converted with
#'   [overedge::as_sf]
#' @param fn Function to apply to data before passing to geom, typically a
#'   predicate or filter to define area for annotation. A filter can also be
#'   passed to any of the {ggforce} functions using the filter aesthetic. Default:
#'   NULL
#' @param mapping Aesthetic mapping to pass to geom, Default: NULL
#' @param cast If TRUE, cast MULTIPOLYGON and POLYGON data to POINT; defaults to
#'   TRUE.
#' @param expand defaults to ggplot2::unit(5, "mm")
#' @param radius defaults to expand
#' @param geom geom to use for layer ("rect", "circle", "ellipse", or "hull"),
#'   Default: `NULL`
#' @param font,face,color Parameters passed to label.family, label.fontface, and label.colour. If `NULL`, set to match general defaults with [ggplot2::theme_get()]
#' @param ... Additional parameters passed to ggforce annotation geoms.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   library(ggplot)
#'   library(overedge)
#'
#'   ggplot() +
#'     layer_location_data(
#'       data = "neighborhoods",
#'       package = "mapbaltimore"
#'     ) +
#'     layer_show_mark(
#'       data = get_location(
#'         type = "council district",
#'         name = "District 12",
#'         package = "mapbaltimore"
#'       ),
#'       geom = "hull",
#'       color = "red",
#'       size = 1.5
#'     )
#' }
#' }
#' @seealso
#'  - \code{\link[ggforce]{geom_mark_rect}}
#'  - \code{\link[ggforce]{geom_mark_circle}}
#'  - \code{\link[ggforce]{geom_mark_ellipse}}
#'  - \code{\link[ggforce]{geom_mark_hull}}
#' @rdname layer_show_mark
#' @aliases layer_show_mark
#' @export
#' @importFrom ggplot2 unit aes theme_get
#' @importFrom overedge as_sf st_cast_ext st_center
#' @importFrom rlang as_function
#' @importFrom utils modifyList
layer_show_mark <- function(data,
                            fn = NULL,
                            mapping = NULL,
                            label_col = NULL,
                            desc_col = NULL,
                            geom = NULL,
                            cast = TRUE,
                            expand = ggplot2::unit(5, "mm"),
                            radius = expand,
                            family = NULL,
                            face =  c("bold", "plain"),
                            color = NULL,
                            ...) {
  data <- overedge::as_sf(data)

  if (!is.null(fn)) {
    fn <- rlang::as_function(fn)
    data <- fn(data)
  }

  if (is.null(family)) {
    family <- ggplot2::theme_get()$text$family
  }

  if (is.null(face)) {
    face <- ggplot2::theme_get()$text$face
  }

  if (is.null(color)) {
    color <- ggplot2::theme_get()$text$color
  }

  check_pkg_installed("ggforce")

  # Add label_col to data if not present
  data <- add_col(data = data, col = label_col)

  # Add desc_col to data if not present
  data <- add_col(data = data, col = desc_col)

  # Add label to mapping
  mapping <-
    modify_mapping(mapping = mapping, label = label_col, description = desc_col)

  # Add geometry to mapping
  mapping <-
    modify_mapping(mapping = mapping, data = data)

  stat <- "sf_coordinates"

  if (cast) {
    data <- overedge::st_cast_ext(x = data)
  } else {
    sf::st_geometry(data) <- overedge::st_center(x = data)$sfc
  }

  geom <- match.arg(geom, c("rect", "circle", "ellipse", "hull"))

  mark_layer <-
    switch(geom,
      "rect" = ggforce::geom_mark_rect(
        data = data, mapping = mapping,
        label.family = family,
        label.fontface = face,
        label.colour = color,
        stat = stat, ...
      ), # Annotate areas with rectangles
      "circle" = ggforce::geom_mark_circle(
        data = data, mapping = mapping,
        label.family = family,
        label.fontface = face,
        label.colour = color,
        stat = stat, ...
      ), # Annotate areas with circles
      "ellipse" = ggforce::geom_mark_ellipse(
        data = data, mapping = mapping,
        label.family = family,
        label.fontface = face,
        label.colour = color,
        stat = stat, ...
      ), # Annotate areas with ellipses
      "hull" = ggforce::geom_mark_hull(
        data = data, mapping = mapping,
        label.family = family,
        label.fontface = face,
        label.colour = color,
        stat = stat, ...
      ) # Annotate areas with hulls
    )

  return(mark_layer)
}
