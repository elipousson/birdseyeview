#' Check if a string is a URL
#'
#' @noRd
check_url <- function(x) {
  grepl(
    "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
    x
  )
}

#' Group by column name if present
#'
#' @noRd
group_by_col <- function(data, groupname_col = NULL) {
  if (!is.null(groupname_col) && (groupname_col %in% names(data))) {
    dplyr::group_by(data, {{ groupname_col }})
  } else {
    data
  }
}

utils::globalVariables(c(
  ".data", " .data", "image_height", "image_width", "numbered"
))

