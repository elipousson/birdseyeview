#' Check if a string is a URL
#'
#' @noRd
check_url <- function(x) {
  grepl(
    "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
    x
  )
}

#' @noRd
check_class <- function(x, check = NULL) {
  any(check %in% class(x))
}

#' Group and sort data by a column
#'
#' Group is col is not NULL and is present in data. Stop if name is not present
#' and data is not NULL.
#'
#' @param data Data to group
#' @param groupname_col Column name to group by, Default: `NULL`
#' @param sort If `TRUE`, sort data by col using [dplyr::arrange]. Defaults to `TRUE`
#' @param ... Parameters passed to [dplyr::group_by] if data is not `NULL` and col is `NULL`.
#' @noRd
#' @importFrom dplyr group_by
#' @importFrom cli cli_abort
group_by_col <- function(data, col = NULL, sort = TRUE, ...) {
  if (!is.null(col) && !is.null(data)) {
    if (sort) {
      data <- dplyr::arrange(data, col)
    }

    if (col %in% names(data)) {
      data <- dplyr::group_by(data, .data[[col]])
    } else {
      cli::cli_abort("The provided data does not have a column matching {usethis::ui_value(col)} to use with group_by().")
    }
  } else if (!is.null(data) && is.null(col)) {
    data <- dplyr::arrange(data, ...)
    data <- dplyr::group_by(data, ...)
  }

  return(data)
}

#' Add column to data if not present
#'
#' @param data Data frame or simple feature object
#' @param col Column name/value
#' @noRd
add_col <- function(data, col = NULL) {
  if (!is.null(col) && !(col %in% names(data)) && any(length(col) %in% c(nrow(data), 1))) {
    # FIXME: This is a non-standard pattern - I like it but it may or may not be appropriate and should be documented
    data[[col]] <- col
  }

  data
}

#' Modify mapping for ggplot2 aesthetics
#'
#' @param mapping aesthetic mapping to modify
#' @param data Data used to determine sf column for geometry aesthetic
#' @param ... Additional parameters with aesthetics to modify and column values
#'   to use, e.g. label = label_col
#' @noRd
modify_mapping <- function(mapping = NULL, data = NULL, ...) {
  if (is.null(mapping)) {
    mapping <-
      ggplot2::aes()
  }

  params <- rlang::list2(...)

  if (!is.null(params)) {
    if (("label" %in% names(params)) && !is.null(params$label)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(label = .data[[params$label]]),
          mapping
        )
    }

    if (("description" %in% names(params)) && !is.null(params$description)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(description = .data[[params$description]]),
          mapping
        )
    }

    if (("fill" %in% names(params)) && !is.null(params$fill)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(fill = .data[[params$fill]]),
          mapping
        )
    }

    if (("size" %in% names(params)) && !is.null(params$size)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(size = .data[[params$size]]),
          mapping
        )
    }

    if (("color" %in% names(params)) && !is.null(params$color)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(color = .data[[params$color]]),
          mapping
        )
    }

    if (("linetype" %in% names(params)) && !is.null(params$linetype)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(linetype = .data[[params$linetype]]),
          mapping
        )
    }
  }

  if (!is.null(data)) {
    mapping <-
      utils::modifyList(
        ggplot2::aes(geometry = .data[[attributes(data)$sf_column]]),
        mapping
      )
  }

  return(mapping)
}

#' Check if package exists and prompt to install if not
#'
#' @param package Name of a package.
#' @param repo GitHub repository to use for the package.
#' @importFrom rlang is_installed check_installed
#' @noRd
check_pkg_installed <- function(package, repo = NULL) {
  if (!rlang::is_installed(pkg = package)) {
    if (!is.null(repo)) {
      package <- repo
    }

    rlang::check_installed(pkg = package)
  }
}

utils::globalVariables(c(
  ".data", " .data", "image_height", "image_width", "numbered"
))
