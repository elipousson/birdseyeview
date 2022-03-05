
#' Create a numbered photo key table
#'
#' @param data sf object or data frame with photo column containing the file
#'   path or url for photos
#' @param img_width Image width.
#' @param img_height Image height.
#' @param photo_col Photo path/url column name.
#' @param title_col Photo title/description column name.
#' @export
#' @importFrom overedge check_sf
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr mutate row_number case_when filter
#' @importFrom gt gt cols_width px tab_style cell_text cells_body opt_all_caps tab_options text_transform web_image local_image
#' @importFrom tidyselect all_of
#' @importFrom purrr map
tbl_photo_key <- function(data,
                          img_width = 320,
                          asp = 0.75,
                          photo_col = "photo",
                          title_col = "title",
                          number = FALSE,
                          orientation = NULL) {
  if (overedge::check_sf(data)) {
    data <- sf::st_drop_geometry(data)
  }

  if (number) {
    data <- data |>
      dplyr::mutate(
        number = dplyr::row_number(),
        .before = .data[[photo_col]]
      )
  }

  if (!is.null(orientation)) {
    img_orient <- orientation

    data <- data |>
      dplyr::mutate(
        orientation = dplyr::case_when(
          (image_width / image_height) > 1 ~ "landscape",
          (image_width / image_height) < 1 ~ "portrait",
          (image_width / image_height) == 1 ~ "square"
        ),
        .after = .data[[photo_col]]
      ) |>
      dplyr::filter(.data$orientation %in% img_orient)

  }

  tbl <- data |>
    gt::gt()

    if (number) {
      tbl <- tbl |>
        gt::cols_width(
          tidyselect::all_of("number") ~ gt::px(img_width * 0.2)
        ) |>
        gt::tab_style(
          style = gt::cell_text(
            weight = "bolder",
            size = "xlarge",
            v_align = "top",
            align = "center"
          ),
          locations = gt::cells_body(columns = tidyselect::all_of("number"))
        )
    }

    tbl <- tbl |>
      gt::cols_width(
        tidyselect::all_of(photo_col) ~ gt::px(img_width)
      )

  if (title_col %in% names(data)) {
    tbl <- tbl |>
      gt::tab_style(
        style = gt::cell_text(
          align = "right",
          size = "large",
          v_align = "top"
        ),
        locations = gt::cells_body(columns = tidyselect::all_of(title_col))
      )
  }

  tbl <- tbl |>
    gt::opt_all_caps() |>
    gt::tab_options(
      table.width = img_width * 2,
      heading.title.font.weight = "bolder"
    )

  check_photo_url <-
    suppressWarnings(
      all(sapply(data[[photo_col]], check_url))
    )

  if (check_photo_url) {
    tbl <- tbl |>
      gt::text_transform(
        locations = tidyselect::all_of(photo_col),
        fn = function(photo) {
          purrr::map(photo, ~ gt::web_image(.x, height = img_width * asp))
        }
      )
  } else {
    tbl <- tbl |>
    gt::text_transform(
      locations = gt::cells_body(tidyselect::all_of(photo_col)),
      fn = function(x) {
        purrr::map(x, ~ gt::local_image(.x, height = img_width * asp))
      }
    )
  }

  return(tbl)
}
