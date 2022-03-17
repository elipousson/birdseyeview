
#' Create discrete fill and color scales for grouped data
#'
#' Designed for use with layer_group_data. group_data_pal generates palettes
#' that are passed to [ggplot2::scale_fill_manual] and
#' [ggplot2::scale_color_manual].
#'
#' @inheritParams paletteer::paletteer_d
#' @inheritParams ggplot2::scale_fill_manual
#' @inheritParams layer_group_data
#' @seealso
#'  \code{\link[scales]{viridis_pal}}
#'  \code{\link[paletteer]{paletteer_d}}
#' @name scale_group_data
#' @export
#' @importFrom ggplot2 scale_fill_manual scale_color_manual
scale_group_data <-
  function(...,
           data,
           groupname_col = NULL,
           palette = NULL,
           n = NULL,
           direction = 1,
           na.value = "grey50",
           drop = FALSE,
           limits = NULL,
           aesthetics = "fill") {
    aesthetics <- match.arg(aesthetics, c("fill", "color"), several.ok = TRUE)

    group_pal <-
      group_data_pal(
        data = data,
        groupname_col = groupname_col,
        palette = palette,
        n = n,
        direction = direction
      )

    scale_fill <- NULL
    scale_color <- NULL

    if ("fill" %in% aesthetics) {
      scale_fill <-
        list(
          ggplot2::scale_fill_discrete(limits = names(group_pal)),
          ggplot2::scale_fill_manual(
            ...,
            values = group_pal,
            limits = names(group_pal),
            na.value = na.value,
            drop = drop
          )
        )
    }

    if ("color" %in% aesthetics) {
      scale_color <-
        list(
          ggplot2::scale_color_discrete(limits = names(group_pal)),
          ggplot2::scale_color_manual(
            ...,
            values = group_pal,
            limits = names(group_pal),
            na.value = na.value,
            drop = drop
          )
        )
    }

    list(
      scale_fill,
      scale_color
    )
  }

#' @name group_data_pal
#' @rdname scale_group_data
#' @export
#' @importFrom dplyr n_groups filter mutate bind_cols group_keys
#' @importFrom scales viridis_pal
#' @importFrom paletteer paletteer_d
#' @importFrom tibble deframe
group_data_pal <- function(data,
                           palette = NULL,
                           groupname_col = NULL,
                           n = NULL,
                           direction = 1) {
  data <-
    group_by_col(
      data = data,
      groupname_col = groupname_col
    )

  if (is.null(n)) {
    n <- dplyr::n_groups(data)
  }

  if (is.null(palette)) {
    palette <- scales::viridis_pal()(n)
  } else {
    pal_opts <-
      dplyr::filter(
        paletteer::palettes_d_names,
        length >= n
      ) %>%
      dplyr::mutate(
        pkg_pal = paste0(package, "::", palette)
      )

    palette <- match.arg(palette, pal_opts$pkg_pal)
    palette <- paletteer::paletteer_d(palette = palette, n = n, direction = direction)
  }

  group_palette <-
    tibble::deframe(
      dplyr::bind_cols(
        "values" = dplyr::group_keys(data),
        "colors" = palette
      )
    )

  return(group_palette)
}
