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
#' Group is groupname_col is not NULL and is present in data. Stop if name is not present and data is not NULL.
#'
#' @param data Data to group
#' @param groupname_col Column name to group by, Default: NULL
#' @noRd
#' @importFrom dplyr group_by
#' @importFrom usethis ui_stop
group_by_col <- function(data, groupname_col = NULL) {
  if (!is.null(groupname_col) && (groupname_col %in% names(data))) {
    dplyr::group_by(data, .data$groupname_col)
  } else if (!is.null(data) && !(groupname_col %in% names(data))) {
    usethis::ui_stop("The provided data does not have a column matching this groupname_col ({usethis::ui_value(groupname_col)}).")
  } else {
    data
  }
}

utils::globalVariables(c(
  ".data", " .data", "image_height", "image_width", "numbered"
))
