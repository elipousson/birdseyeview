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
#' @importFrom overedge is_sf_list is_sf as_sf st_bbox_ext is_bbox
#'   sf_bbox_asp get_paper
#' @importFrom usethis ui_stop
make_detail_map <- function(detail,
                            dist = NULL,
                            diag_ratio = NULL,
                            layer_fn = NULL,
                            context = NULL,
                            context_dist = 0,
                            groupname_col = NULL,
                            geom = NULL,
                            palette = NULL,
                            aesthetics = c("color", "fill"),
                            size = 5,
                            alpha = 1,
                            number = FALSE,
                            paper = NULL,
                            orientation = NULL,
                            access_token = Sys.getenv("MAPBOX_SECRET_TOKEN"),
                            map_style = NULL,
                            scale = 1,
                            cols = 1,
                            rows = 1,
                            margin = "none",
                            gutter = 0,
                            sort = "lon",
                            neatline = TRUE,
                            neatline_params = list(dist = 0, diag_ratio = NULL, color = "black", size = 1, expand = TRUE),
                            crs = NULL,
                            fn = NULL,
                            inset_legend = TRUE,
                            legend_position = "bottomleft",
                            labs_ext = NULL,
                            below = NULL,
                            above = NULL,
                            ...) {
  if (overedge::is_sf_list(detail)) {
    batch <- length(detail) # list column with data
  } else if (overedge::is_sf(detail)) {
    detail <- list(overedge::as_sf(detail)) # coercible sf object in list length 1
    # TODO: Should this be a named list, e.g. list("data" = overedge::as_sf(detail))
    batch <- 1
  } else {
    # data frame with nested list column named data (produced by group_by then group_nest using keep_all = TRUE)
    if (("data" %in% names(detail)) && overedge::is_sf_list(detail$data)) {
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
    if (is.null(orientation) && (overedge::is_bbox(context_bbox))) {
      orientation <- overedge::sf_bbox_asp(bbox = context_bbox, orientation = TRUE)
    } else if (is.null(orientation)) {
      orientation <- "landscape"
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

  # print(detail_paper$section_asp)

  if (!is.null(detail_paper)) {
    section_asp <- unique(detail_paper$section_asp)
  }

  stopifnot(
    is.numeric(section_asp)
  )

  # Create maps with make_section_map
  detail_maps <-
    map(
      detail,
      ~ make_section_map(
        section = .x,
        layer_fn = layer_fn,
        dist = dist,
        diag_ratio = diag_ratio,
        groupname_col = groupname_col,
        geom = geom,
        size = size,
        alpha = alpha,
        # FIXME: group scale should avoid the need to add scales as below
        access_token = access_token,
        map_style = map_style,
        fn = fn,
        sort = sort,
        section_asp = section_asp,
        scale = scale,
        number = number,
        neatline = neatline,
        neatline_params = neatline_params,
        expand = expand,
        crs = crs,
        below = below,
        above = above,
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
        margin = 5,
        unit = "pt",
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
#' @param section_asp Aspect ratio, Default: NULL
#' @param number If TRUE, number data, Default: FALSE
#' @param ... Additional parameters used by various functions.
#' @rdname make_detail_map
#' @name make_section_map
#' @export
#' @importFrom rlang list2 as_function
make_section_map <- function(section,
                             dist = NULL,
                             diag_ratio = NULL,
                             section_asp = NULL,
                             groupname_col = NULL,
                             access_token = NULL,
                             map_style = snapbox::mapbox_gallery_moonlight(),
                             size = 5,
                             alpha = 1,
                             geom = NULL,
                             sort = NULL,
                             scale = 0.5,
                             number = FALSE,
                             neatline = TRUE,
                             neatline_params = list(dist = 0, diag_ratio = NULL, color = "black", size = 1, expand = TRUE),
                             expand = TRUE,
                             crs = NULL,
                             fn = NULL,
                             layer_fn = NULL,
                             below = NULL,
                             above = NULL,
                             ...) {
  if (!is.null(fn)) {
    fn <- rlang::as_function(fn)
    section <- fn(section)
  }


  # FIXME: This is a work around but not ideal
  # if (overedge::is_geom_type(section_combine, check = "MULTIPOINT")) {
  #  section_combine <- sf::st_cast(overedge::st_buffer_ext(section_combine, dist = 1), to = "POLYGON")
  # }

  stopifnot(
    is.numeric(dist) || is.null(dist),
    is.numeric(diag_ratio) || is.null(diag_ratio),
    is.null(section_asp) || is.numeric(section_asp),
    nrow(section) > 0
  )

  mapbox_crs <- 3857
  section_combine <- sf::st_union(sf::st_combine(section))

  sect_bbox <-
    overedge::st_bbox_ext(
      x = section_combine,
      asp = section_asp,
      dist = dist,
      diag_ratio = diag_ratio,
      crs = mapbox_crs
    )

  sect_basemap <-
    make_mapbox_basemap(
      data = sect_bbox,
      map_style = map_style,
      scale = scale,
      access_token = access_token,
      basemap = TRUE,
      mapbox_logo = FALSE,
      attribution = FALSE
    )

  sect_params <- rlang::list2(...)

  if (!is.null(layer_fn)) {
    layer_fn <- rlang::as_function(layer_fn)
    sect_layer <- layer_fn(section)
  } else if (number) {
    geom <- match.arg(geom, c("label", "label_repel"))
    sect_layer <-
      layer_number_markers(
        data = section,
        geom = geom,
        sort = sort,
        groupname_col = groupname_col,
        size = size,
        alpha = alpha
      )
  } else {
    geom <- match.arg(geom, c("sf", "text", "text_repel", "label", "label_repel"))

    sect_layer <-
      layer_show_markers(
        data = section,
        geom = geom,
        groupname_col = groupname_col,
        size = size,
        alpha = alpha
      )
  }

  if (!is.null(below) && is.list(below) && check_class(below[[1]], "ggproto")) {
    below_layer <- below
  } else {
    below_layer <- NULL
  }


  if (!is.null(above) && is.list(above) && check_class(above[[1]], "ggproto")) {
    above_layer <- above
  } else {
    above_layer <- NULL
  }

  neatline_params <-
    utils::modifyList(
      list(dist = 0, diag_ratio = NULL, color = "black", size = 1, expand = TRUE),
      neatline_params,
      keep.null = TRUE
    )

  if (neatline) {
    neatline_layer <-
      overedge::layer_neatline(
        data = sect_bbox,
        asp = section_asp,
        dist = neatline_params$dist, # TODO: Document the existence of the neatline_dist parameter as distinct from sect_params$dist used to create the section_bbox
        diag_ratio = neatline_params$diag_ratio,
        expand = neatline_params$expand,
        color = neatline_params$color,
        size = neatline_params$size
      )
  } else {
    neatline_layer <- NULL
  }

  sect_basemap +
    below_layer +
    sect_layer +
    above_layer +
    neatline_layer
}
