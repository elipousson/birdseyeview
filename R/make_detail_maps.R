#' Make detail maps
#'
#' Make detail maps, e.g. nodes of development within a broader area, examples
#' of a type using data created by using [dplyr::group_by()] and
#' [dplyr::group_nest()] if the data is an sf object and keep = TRUE.
#'
#' @param detail an sf object (or object that is convertible with [overedge::as_sf()]), list of sf objects, or data frame with a sf list column named data
#' @param context Broader context area containing all detail maps, Default: NULL
#' @param overlay Overlay for all detail maps, Default: NULL
#' @rdname make_detail_map
#' @export
#' @importFrom overedge get_paper
#' @importFrom usethis ui_stop
make_detail_map <- function(detail,
                            context = NULL,
                            fn = NULL,
                            number = FALSE,
                            overlay = NULL,
                            paper = NULL,
                            orientation = NULL,
                            context_dist = 0,
                            detail_diag_ratio = 0.1,
                            access_token = Sys.getenv("MAPBOX_SECRET_TOKEN"),
                            columns = 1,
                            rows = 1,
                            sect_params = list(...)) {
  stopifnot(
    check_sf(detail, ext = TRUE)
  )

  if (check_sf_list(detail)) {
    batch <- length(detail) # list column with data
  } else if (check_sf(detail, ext = TRUE)) {
    detail <- list(as_sf(detail)) # coercible sf object in list length 1
    batch <- 1
  } else {
    # data frame with nested list column named data (produced by group_by then group_nest using keep_all = TRUE)
    if ((data %in% names(detail)) && check_sf_list(detail$data)) {
      # FIXME: This may not be preferred in all cases
      detail <- detail$data
      batch <- length(detail)
    }
  }
  if (!is.null(context)) {
    context_bbox <-
      st_bbox_ext(
        x = context,
        dist = dist$context,
        asp = NULL # TODO: Consider alternatives to this approach
      )
  } else {
    context_bbox <- NULL
  }

  if (is.character(paper)) {
    if (is.null(orientation) && (check_bbox(context_bbox, ext = TRUE))) {
      orientation <- sf_bbox_asp(bbox = context_bbox, orientation = TRUE)
    }
    detail_paper <- overedge::get_paper(paper = paper, orientation = orientation)
  } else if (is.data.frame(paper)) {
    if (!c("width", "height", "orientation", "asp") %in% names(paper)) {
      usethis::ui_stop("The dataframe provided to paper provided does not appear to include the required columns.")
    } else {
      detail_paper <- paper
    }
  }

  # TODO: Re-implement margins here to get the block_asp value

  if (!is.null(context)) {
    context_bbox <-
      st_bbox_ext(
        x = context,
        dist = context_dist,
        asp = paper$asp
      )
  }


  if ((columns > 1) || rows > 1) {
    paper$cols <- columns
    paper$col_width <- paper$width / columns
    paper$rows <- rows
    paper$row_height <- paper$height / rows
    paper$section_asp <- paper$col_width / paper$row_height
  } else {
    paper$section_asp <- paper$asp
  }

  detail_maps <-
    map(
      detail,
      ~ make_section_map(.x,
        access_token = access_token,
        fn = fn,
        asp = paper$section_asp,
        scale = scale,
        numbered = numbered,
        sect_params = sect_params
      )
    )

  return(detail_maps)
}

#' @details Make section map:
#'
#' All parameters that are specific to individual section maps should be passed
#' to sect_params. asp is automatically calculated based on the width and height
#' of the overall paper, and the number of rows and columns specified in
#' make_detail_map.
#'
#' Some make_detail_map parameters are passed on directly to make_section_map
#' including:
#'
#' - fn (applied to each section before mapping - may be useful for recoding labels)
#' - access_token and scale - used to create section base map
#' - number - indicator of whether to call layer_number_markers and add to layers for section map
#'
#' All other parameters are defined by creating a nested named list for
#' sect_params. Supported options for sect_params are passed to one of the
#' following functions:
#'
#' - layer_number_markers: sort, number_col, and group_col parameters
#' - [overedge::theme_legend]: all parameters (except method) area supported but these
#' parameters must be passed as a list so they can be called in the following
#' way, e.g. sect_params$legend$position.
#' - [overedge::layer_neatline]: the expand parameter for  and the parameter neatline_dist to
#' allow the adjustment of the section neatline (needed for the expand parameter
#' to work as expected)
#'
#' All of these variables are currently defined globally and cannot be modified
#' for detail maps with the section (unless you call make_section_map directly).
#'
#' @param section PARAM_DESCRIPTION
#' @param overlay PARAM_DESCRIPTION, Default: NULL
#' @param fn PARAM_DESCRIPTION, Default: NULL
#' @param access_token PARAM_DESCRIPTION, Default: NULL
#' @param scale PARAM_DESCRIPTION, Default: 0.6
#' @param asp PARAM_DESCRIPTION, Default: NULL
#' @param number PARAM_DESCRIPTION, Default: FALSE
#' @param sect_params PARAM_DESCRIPTION, Default: list(...)
#' @details DETAILS
#' @rdname make_detail_map
#' @name make_section_map
#' @export
#' @importFrom rlang list2 as_function
make_section_map <- function(section,
                             overlay = NULL,
                             fn = NULL,
                             access_token = NULL,
                             scale = 0.6,
                             asp = NULL,
                             number = FALSE,
                             sect_params = list(...)) {
  if (!is.null(fn)) {
    fn <- rlang::as_function(fn)
    section <- fn(section)
  }

  sect_bbox <-
    st_bbox_ext(
      x = section,
      asp = asp,
      dist = sect_params$dist,
      diag_ratio = sect_params$diag_ratio
    )

  sect_layers <-
    make_mapbox_basemap(
      data = sect_bbox,
      scale = scale,
      access_token = access_token
    )

  if (numbered) {
    sect_layers <-
      list(
        sect_layers,
        layer_number_markers(
          data = section,
          sort = sect_params$sort,
          number_col = sect_params$number_col,
          groupname_col = sect_params$groupname_col
        )
      )
    }


  if ("legend" %in% sect_params) {
    if (is.list(legend)) {
      sect_layers <- list(
        sect_layers,
        theme_legend(
          position = sect_params$legend$position,
          margin = sect_params$legend$margin,
          unit = sect_params$legend$unit,
          inset = sect_params$legend$inset,
          bgcolor = sect_params$legend$bgcolor,
          justification = sect_params$legend$bgcolor
        )
      )
    }
  }

  if (is.null(overlay) && check_class(overlay, "ggproto")) {
    sect_layers <- list(
      sect_layers,
      overlay
    )
  }


  if (neatline) {
    sect_layers <- list(
      sect_layers,
      layer_neatline(
        data = sect_bbox,
        dist = sect_params$neatline_dist, # TODO: Document the existence of the neatline_dist parameter as distinct from sect_params$dist used to create the section_bbox
        asp = asp,
        expand = sect_params$expand
      )
    )
  }
}
