#' Create numbered photo key as a `gt` table (not working)
#'
#' Designed to work with the `overedge::read_sf_exif` function. More information on this approach here <https://elipousson.github.io/posts/2021-03-06-creating-a-key-map-for-photographs-with-r/>
#'
#' Function naming convention inspired by the {gtsummary} package <https://www.danieldsjoberg.com/gtsummary/index.html>
#'
#' @param data sf object or data frame with photo column containing the file
#'   path or url for photos.
#' @param height The height in pixels of image in table.
#' @param asp Aspect ratio of image (width/height)
#' @param photo_col Photo path/url column name.
#' @param title_col Title/description column name.
#' @param title_size Title text size, Default: 'large'
#' @param title_align Title tex alignment, Default: 'right'
#' @param number If `TRUE`, add a number column that will by 20% of the width of
#'   the photo column; defaults to `FALSE`. There may be a name conflict if the
#'   dataframe already contains a column named "rowid".
#' @param orientation Orientation to filter by if data contains image_width and
#'   image_height columns.
#' @md
#' @export
#' @importFrom overedge check_sf get_asp
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr mutate case_when filter select row_number
#' @importFrom usethis ui_stop
#' @importFrom tidyselect everything all_of
#' @importFrom gt gt cols_width px tab_style cell_text cells_body text_transform web_image local_image
#' @importFrom purrr map
tbl_photo_key <- function(data,
                          height = 240,
                          asp = 0.75,
                          photo_col = "photo",
                          title_col = "title",
                          title_size = "large",
                          title_align = "right",
                          number = FALSE,
                          orientation = NULL) {
  if (overedge::check_sf(data)) {
    data <- sf::st_drop_geometry(data)
  }

  if (!is.null(asp)) {
    asp <- overedge::get_asp(asp)
  } else {
    asp <- 0.75
  }

  if (!is.null(orientation)) {
    image_orientation <- orientation

    if (all(c("image_height", "image_width") %in% names(data))) {
      data <- data %>%
        dplyr::mutate(
          orientation = dplyr::case_when(
            (image_width / image_height) > 1 ~ "landscape",
            (image_width / image_height) < 1 ~ "portrait",
            (image_width / image_height) == 1 ~ "square"
          ),
          .after = .data[[photo_col]]
        )
    } else if (!("orientation" %in% names(data))) {
      usethis::ui_stop("Filtering images by orientation requires either an image_width and image_height column or an orientation column in the provided dataframe.")
    }

    data <- data %>%
      dplyr::filter(.data$orientation %in% image_orientation) %>%
      dplyr::select(-c(image_width, image_height, orientation))
  }

  if (number) {
    data <-
      dplyr::mutate(
        data,
        rowid = dplyr::row_number(),
        .before = tidyselect::everything()
      )
  }

  tbl <- gt::gt(data)

  tbl <-
    gt::cols_width(
      tbl,
      tidyselect::all_of(photo_col) ~ gt::px(height / asp)
    )

  if (number) {
    tbl <- tbl %>%
      gt::cols_width(
        "rowid" ~ gt::px((height / asp) * 0.2)
      ) %>%
      gt::tab_style(
        style = gt::cell_text(
          weight = "bolder",
          size = "xlarge",
          v_align = "top",
          align = "center"
        ),
        locations = gt::cells_body(columns = "rowid")
      )
  }

  if (title_col %in% names(data)) {
    tbl <- tbl %>%
      gt::tab_style(
        style = gt::cell_text(
          align = title_align,
          size = title_size,
          v_align = "top"
        ),
        locations = gt::cells_body(columns = tidyselect::all_of(title_col))
      )
  }

  check_photo_url <-
    suppressWarnings(
      all(sapply(data[[photo_col]], check_url))
    )

  if (check_photo_url) {
    tbl <-
      gt::text_transform(
        data = tbl,
        locations = gt::cells_body(columns = tidyselect::all_of(photo_col)),
        fn = function(url) {
          purrr::map(url, ~ gt::web_image(.x, height = height / asp))
        }
      )
  } else {
    tbl <- tbl |>
      gt::text_transform(
        locations = gt::cells_body(photo_col),
        fn = function(x) {
          purrr::map(x, ~ gt::local_image(.x, height = height / asp))
        }
      )
  }

  return(tbl)
}
