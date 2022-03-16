#' Make detail maps
#'
#' Make detail maps, e.g. nodes of development within a broader area, examples
#' of a type using data created by using [dplyr::group_by()] and
#' [dplyr::group_nest()] if the data is an sf object and keep = TRUE.
#'
#' @param detail an sf object (or object that is convertible with
#'   [overedge::as_sf()]), list of sf objects, or data frame with a sf list
#'   column named data
#' @param context Broader context area containing all detail maps, Default: NULL
#' @param overlay Overlay for all detail maps, Default: NULL
#' @param neatline_params List with dist, diag_ratio, color, size, and expand
#'   passed on to layer_neatline.
#' @param labs_ext Label object created with [ggplot2::labs] or [overedge::labs_ext] function.
#' @rdname make_detail_map
#' @inheritParams overedge::get_paper
#' @export
#' @importFrom overedge check_sf_list check_sf as_sf st_bbox_ext check_bbox
#'   sf_bbox_asp get_paper
#' @importFrom usethis ui_stop
make_detail_map <- function(detail,
                            context = NULL,
                            fn = NULL,
                            groupname_col = NULL,
                            palette = NULL,
                            aesthetics = c("color", "fill"),
                            number = FALSE,
                            overlay = NULL,
                            paper = NULL,
                            orientation = NULL,
                            context_dist = 0,
                            detail_diag_ratio = 0.1,
                            access_token = Sys.getenv("MAPBOX_SECRET_TOKEN"),
                            cols = 1,
                            rows = 1,
                            margin = "none",
                            gutter = 0,
                            sort = "lon",
                            scale = 1,
                            neatline = TRUE,
                            neatline_params = list(dist = 0, diag_ratio = NULL, color = "black", size = 1, expand = TRUE),
                            crs = NULL,
                            inset_legend = TRUE,
                            legend_position = "bottomleft",
                            labs_ext = NULL,
                            ...) {
  if (overedge::check_sf_list(detail)) {
    batch <- length(detail) # list column with data
  } else if (overedge::check_sf(detail)) {
    detail <- list(overedge::as_sf(detail)) # coercible sf object in list length 1
    # TODO: Should this be a named list, e.g. list("data" = overedge::as_sf(detail))
    batch <- 1
  } else {
    # data frame with nested list column named data (produced by group_by then group_nest using keep_all = TRUE)
    if (("data" %in% names(detail)) && overedge::check_sf_list(detail$data)) {
      # FIXME: This may not be preferred in all cases
      detail <- detail$data
      batch <- length(detail)
    } else {
      usethis::ui_stop("The object passed to detail does not match any supported formats.
                       Supported format include an sf object, a list of sf objects, or a data frame with a nested list column where each row is an sf object.")
    }
  }

  # FIXME: If this function can put data back together should it also handle the nest_group function and be able to take it apart
  # This would require disambiguating geographical grouping variables from display grouping variables which will typically be different
  detail_df <- dplyr::bind_rows(detail)

  # FIXME: Should this be dependent on a NULL orientation value if the only use for the context_bbox is to generate a page orientation
  if (!is.null(context) && (is.numeric(context_dist) || is.null(context_dist))) {
    context_bbox <-
      overedge::st_bbox_ext(
        x = context,
        dist = context_dist,
        asp = NULL, # TODO: Consider alternatives to this two-step approach to bounding box generation
        crs = crs
      )
  } else {
    context_bbox <- NULL
  }

  # TODO: This could be a check_paper function or added to the existing get_paper function in overedge
  if (is.character(paper)) {
    if (is.null(orientation) && (overedge::check_bbox(context_bbox, ext = TRUE))) {
      orientation <- overedge::sf_bbox_asp(bbox = context_bbox, orientation = TRUE)
    }
    detail_paper <- overedge::get_paper(paper = paper, orientation = orientation, cols = cols, rows = rows, gutter = gutter, margin = margin)
  } else if (is.data.frame(paper)) {
    if (!c("width", "height", "orientation", "asp", "section_asp") %in% names(paper)) {
      usethis::ui_stop("The dataframe provided to paper provided does not appear to include the required columns.")
    }

    detail_paper <- paper
  }

  # TODO: Re-implement margins here to get the block_asp value
  # FIXME:
  if (!is.null(context)) {
    context_bbox <-
      overedge::st_bbox_ext(
        x = context,
        dist = context_dist,
        asp = detail_paper$asp,
        crs = crs
      )
  }

  # Create maps with make_section_map
  detail_maps <-
    map(
      detail,
      ~ make_section_map(
        section = .x,
        groupname_col = groupname_col,
        overlay = overlay,
        # FIXME: group scale should avoid the need to add scales as below
        access_token = access_token,
        fn = fn,
        sort = sort,
        asp = detail_paper$section_asp,
        scale = scale,
        number = number,
        neatline = neatline,
        neatline_params = neatline_params,
        expand = expand,
        crs = crs,
        ...
      )
    )

  # Create scale based on detail_df and groupname_col
  if (!is.null(groupname_col)) {
    group_scale <-
      scale_group_data(
        data = detail_df,
        groupname_col = groupname_col,
        palette = palette,
        aesthetics = aesthetics
      )
  } else {
    group_scale <- NULL
  }

  if (!is.null(legend_position)) {
    detail_legend <-
      overedge::theme_legend(
        position = legend_position,
        #  margin = 10,
        #  unit = "pt",
        inset = inset_legend
      )
  }

  # Combine detail maps with scale and legend
  detail_maps <-
    purrr::map(
      detail_maps,
      ~ .x +
        group_scale +
        detail_legend +
        labs_ext
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
#' All other parameters are defined by the dots ... Supported options for ...
#' are passed to one of the following functions:
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
#' @param section Section (sf obect)
#' @param overlay Overlay, Default: NULL
#' @param fn Function, Default: NULL
#' @param access_token Mapbox access token, Default: NULL
#' @param scale Scale, Default: 0.6
#' @param asp Aspect ratio, Default: NULL
#' @param number If TRUE, number data, Default: FALSE
#' @param ... Additional parameters used by various functions.
#' @rdname make_detail_map
#' @name make_section_map
#' @export
#' @importFrom rlang list2 as_function
make_section_map <- function(section,
                             groupname_col = NULL,
                             overlay = NULL,
                             fn = NULL,
                             sort = NULL,
                             access_token = NULL,
                             scale = 0.6,
                             asp = NULL,
                             number = FALSE,
                             neatline = TRUE,
                             neatline_params = list(dist = 0, diag_ratio = NULL, color = "black", size = 1, expand = TRUE),
                             expand = TRUE,
                             crs = NULL,
                             ...) {
  if (!is.null(fn)) {
    fn <- rlang::as_function(fn)
    section <- fn(section)
  }

  neatline_params <-
    utils::modifyList(
      list(dist = 0, diag_ratio = NULL, color = "black", size = 1, expand = TRUE),
      neatline_params,
      keep.null = TRUE
    )

  sect_params <- rlang::list2(...)

  mapbox_crs <- 3857

  sect_bbox <-
    st_bbox_ext(
      x = section,
      asp = asp,
      dist = sect_params$dist,
      diag_ratio = sect_params$diag_ratio,
      crs = mapbox_crs
    )

  sect_basemap <-
    make_mapbox_basemap(
      data = sect_bbox,
      scale = scale,
      access_token = access_token,
      basemap = TRUE,
      crs = mapbox_crs
    )

  if (number) {
    sect_layer <-
      layer_number_markers(
        data = section,
        sort = sort,
        groupname_col = groupname_col,
        crs = crs
      )
  } else {
    sect_layer <-
      layer_show_markers(
        data = section,
        sort = sort,
        groupname_col = groupname_col,
        crs = crs
      )
  }

  # FIXME: This may not be needed if legends are being applied in the wrapping function
  if ("legend" %in% sect_params) {
    if (is.list(sect_params$legend)) {
      legend_layer <-
        overedge::theme_legend(
          position = sect_params$legend$position,
          margin = sect_params$legend$margin,
          unit = sect_params$legend$unit,
          inset = sect_params$legend$inset,
          bgcolor = sect_params$legend$bgcolor,
          justification = sect_params$legend$bgcolor
        )
    }
  } else {
    legend_layer <- NULL
  }

  if (!is.null(overlay) && check_class(overlay, "ggproto")) {
    overlay_layer <- overlay
  } else {
    overlay_layer <- NULL
  }


  if (neatline) {
    neatline_layer <-
      layer_neatline(
        data = sect_bbox,
        asp = asp,
        dist = neatline_params$dist, # TODO: Document the existence of the neatline_dist parameter as distinct from sect_params$dist used to create the section_bbox
        diag_ratio = neatline_params$diag_ratio,
        expand = neatline_params$expand,
        color = neatline_params$color,
        size = neatline_params$size,
        crs = crs
      )
  } else {
    neatline_layer <- NULL
  }

  sect_basemap +
    sect_layer +
    legend_layer +
    overlay_layer +
    neatline_layer
}
