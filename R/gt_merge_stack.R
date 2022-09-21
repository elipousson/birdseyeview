#' Merge and stack text from two columns in `gt` (variation on `{gtExtras}`)
#'
#' A variation on the [gtExtras::gt_merge_stack] function that implements a
#' horizontal rule parameter separating the top and bottom text and simplifies
#' the requirements for the input variables. `{gtExtras}` was created by Thomas
#' Mock and is available under an MIT License.
#'
#' @param columns Columns to stack, Default: c(1, 2)
#' @param font_color Color(s) to use for stacked text. Default: c("black",
#'   "grey")
#' @inheritParams gtExtras::gt_merge_stack
#' @param h_rule Horizontal rule to separate top and bottom column text. If
#'   `TRUE`, use a 1px line with a 2px margin on top and bottom. If `FALSE`,
#'   include no horizontal rule. To create a custom rule, provide a `<hr>`
#'   thematic break tag or a `<div>` with css styling.
#' @rdname gt_merge_stack
#' @export
#' @importFrom gt text_transform cells_stub everything cells_body cols_hide
#' @importFrom cli cli_abort
#' @importFrom rlang arg_match as_string enexpr
#' @importFrom glue glue
gt_merge_stack <- function(gt_object,
                           columns = c(1, 2),
                           font_color = c("black", "grey"),
                           ...,
                           small_cap = FALSE,
                           font_size = c(14, 10),
                           font_weight = "bold",
                           h_rule = FALSE) {
  sfext:::is_pkg_installed("scales")
  gt:::stop_if_not_gt(gt_object)

  columns <-
    gt:::resolve_cols_c(
      expr = {{ columns }},
      data = gt_object
    )

  if (length(columns) != 2) {
    cli::cli_abort(
      "{.arg columns} must be a length two vector or
      a tidyselect function that returns two columns."
    )
  }

  col1 <- columns[1]
  col2 <- columns[2]

  color <- rep2(font_color, "colors")
  color <- scales::col2hcl(color, ...)

  font_size <- rep2(font_size, "{.arg font_size}")

  if (all(is.numeric(font_size))) {
    font_size <- paste0(font_size, "px")
  }

  if (!all(grepl(x = font_size, pattern = "px"))) {
    cli::cli_abort(
      "{.arg font_size} must be numeric value or a string with {.val px}"
    )
  }

  font_weight <- rep2(font_weight, "{.arg font_weight}")
  font_weight <-
    rlang::arg_match(
      font_weight,
      c("bold", "normal", "lighter"),
      multiple = TRUE
    )

  font_variant <- "normal"

  if (small_cap) {
    font_variant <- "small-caps"
  }

  row_name_var <-
    gt_object[["_boxhead"]][["var"]][
      which(gt_object[["_boxhead"]][["type"]] == "stub")
    ]

  col1_bare <- rlang::as_string(rlang::enexpr(col1))
  col2_data <- gt_index(gt_object, column = {{ col2 }})

  if (is.logical(h_rule)) {
    if (h_rule) {
      h_rule <- #
        "<div style='display:block;margin-before:5px;margin-after:5px;
        margin-start:auto;margin-end:auto;overflow:hidden;border-style:inset;
      border-width:1px'></div>"
    } else {
      h_rule <- NULL
    }
  }

  gt_object %>%
    gt::text_transform(
      locations = if (isTRUE(row_name_var == col1_bare)) {
        gt::cells_stub(rows = gt::everything())
      } else {
        gt::cells_body(columns = {{ col1 }})
      },
      fn = function(x) {
        col1_style <-
          glue::glue("font-weight:{font_weight[1]};font-variant:{font_variant};
                     color:{color[1]};font-size:{font_size[1]}")
        col2_style <-
          glue::glue("font-weight:{font_weight[2]};color:{color[2]};
                     font-size:{font_size[2]}")

        glue::glue(
          "<div style='line-height:{font_size[1]}'><span style='{col1_style}'>
          {x}</span></div>\n{h_rule}\n<div style='line-height:{font_size[2]}'>
          <span style ='{col2_style}'>{col2_data}</span></div>",
          .null = ""
        )
      }
    ) %>%
    gt::cols_hide(columns = {{ col2 }})
}

# from gtExtras 0.4.2.9000
gt_index <- function(gt_object, column, as_vector = TRUE) {
  stopifnot(
    `'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` =
      "gt_tbl" %in% class(gt_object)
  )
  stopifnot(`'as_vector' must be a TRUE or FALSE` = is.logical(as_vector))
  if (length(gt_object[["_row_groups"]]) >= 1) {
    gt_row_grps <- gt_object[["_row_groups"]]
    grp_vec_ord <- gt_object[["_stub_df"]] %>%
      dplyr::mutate(group_id = factor(group_id,
        levels = gt_row_grps
      )) %>%
      dplyr::arrange(group_id) %>%
      dplyr::pull(rownum_i)
    df_ordered <- gt_object[["_data"]] %>% dplyr::slice(grp_vec_ord)
  } else {
    df_ordered <- gt_object[["_data"]]
  }
  if (isTRUE(as_vector)) {
    df_ordered %>% dplyr::pull({{ column }})
  } else {
    df_ordered
  }
}

#' Repeat a length 1 vector to make a length 2 vector or error on vectors
#' greater than length 2
#'
#' @param x A length 1 or 2 vector.
#' @param label A plural label for the attribute corresponding to x.
#' @noRd
rep2 <- function(x, label = NULL) {
  if (length(x) <= 2) {
    if (length(x) == 1) {
      x <- rep(x, 2)
    }
    return(x)
  }
  cli::cli_abort(
    "There must be one or two {label}, not {length(x)}.",
    call = rlang::caller_env()
  )
}
