#' Use snapbox to make a Mapbox basemap
#'
#' Expect this function to shift from using snapbox to mapboxapi if/when this
#' pull request is accepted: <https://github.com/walkerke/mapboxapi/pull/18>
#'
#' layer_show_mapbox works the same as make_mapbox_basemap but defaults to
#' basemap and neatline to FALSE
#'
#' @param data sf, sfc, or bbox object; any objects convertible with [overedge::as_bbox()]
#' @param map_style Map style, Default: [snapbox::mapbox_gallery_moonlight()]
#' @param scale_ratio Scale ratio, Default: 1
#' @param access_token Access token, following default format for public token from [mapboxapi::mb_access_token()], Default: Sys.getenv("MAPBOX_PUBLIC_TOKEN")
#' @param basemap If FALSE, create a stand alone layer; if TRUE, the layer is precededed by [ggplot2::ggplot()] to allow use as a basempa, Default: TRUE
#' @param neatline If TRUE, add a neatline matching the provided data, Default: TRUE
#' @inheritParams overedge::layer_neatline
#' @param ... Additional parameter passed to [overedge::layer_neatline()]
#' @seealso
#'  \code{\link[snapbox]{reexports}},\code{\link[snapbox]{layer_mapbox}}
#'  \code{\link[overedge]{st_transform_ext}},\code{\link[overedge]{as_sf}},\code{\link[overedge]{layer_neatline}}
#' @rdname make_mapbox_basemap
#' @md
#' @export
#' @importFrom snapbox mapbox_gallery_moonlight layer_mapbox
#' @importFrom overedge st_transform_ext as_bbox layer_neatline
#' @importFrom ggplot2 ggplot
make_mapbox_basemap <-
  function(data,
           map_style = snapbox::mapbox_gallery_moonlight(),
           scale_ratio = 0.5,
           access_token = Sys.getenv("MAPBOX_PUBLIC_TOKEN"),
           basemap = TRUE,
           neatline = TRUE,
           expand = TRUE,
           ...) {

    # Set appropriate CRS for Mapbox
    crs_mapbox <- 3857

    bbox <- overedge::as_bbox(data)
    # FIXME: should this function support dist, diag_ratio, and asp?
    bbox <-
      overedge::sf_bbox_transform(
        bbox = bbox,
        crs = crs_mapbox
      )

    snapbox_layer <- NULL

    # Get Mapbox map
    snapbox_layer <-
      list(
        snapbox::layer_mapbox(
          area = bbox,
          map_style = map_style,
          scale_ratio = scale_ratio,
          mapbox_api_access_token = access_token
        )
      )

    if (neatline) {
      snapbox_layer <-
        list(
          snapbox_layer,
          overedge::layer_neatline(
            data = bbox,
            expand = expand,
            crs = crs_mapbox,
            ...
          )
        )
    }

    if (basemap) {
      ggplot2::ggplot() +
        snapbox_layer
    } else {
      return(snapbox_layer)
    }
  }

#' @rdname make_mapbox_basemap
#' @name layer_show_mapbox
#' @export
layer_show_mapbox <- function(data,
                              map_style = snapbox::mapbox_gallery_moonlight(),
                              scale_ratio = 0.5,
                              access_token = Sys.getenv("MAPBOX_PUBLIC_TOKEN"),
                              basemap = FALSE,
                              neatline = FALSE,
                              expand = FALSE,
                              ...) {
  make_mapbox_basemap(
    data = data,
    map_style = map_style,
    scale_ratio = scale_ratio,
    access_token = access_token,
    basemap = basemap,
    neatline = neatline,
    expand = expand,
    ...
  )
}
