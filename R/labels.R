#' Capitalize labels
#'
#' Convert text into title case (first word only)
#'
#' @param x Input object, character, data frame, list, ggplot, etc.
#' @return Returns the same class as input
#' @export
capitalize_labels <- function(x) UseMethod("capitalize_labels")

capitalize_labels_ <- function(x) {
  m <- regexpr("[[:alpha:]]", x)
  n <- tfse::regmatches_first(x, "[[:alpha:]]")
  regmatches(x, m) <- toupper(n)
  x
}

#' @export
capitalize_labels.default <- function(x) x

#' @export
capitalize_labels.character <- function(x) {
  if (tfse::n_uq(x) != length(x)) {
    x <- factor(x, labels = capitalize_labels_(unique(x)))
    return(as.character(x))
  }
  capitalize_labels_(x)
}

#' @export
capitalize_labels.factor <- function(x) {
  factor(x, labels = capitalize_labels_(levels(x)))
}

#' @export
capitalize_labels.data.frame <- function(x) {
  dapr::dapc(x, capitalize_labels)
}

#' @export
capitalize_labels.list <- function(x) {
  dapr::lap(x, capitalize_labels)
}

#' @export
capitalize_labels.quosure <- function(x) {
  tfse:::is_installed("rlang", stop = TRUE)
  rlang::as_quosure(
    as.symbol(capitalize_labels(rlang::quo_name(x))),
    env = rlang::get_env(x)
  )
}

#' @export
capitalize_labels.ggplot <- function(x) {
  x$data <- capitalize_labels(x$data)
  names(x$data) <- capitalize_labels(names(x$data))
  x$plot_env$. <- capitalize_labels(x$plot_env$.)
  names(x$plot_env$.) <- capitalize_labels(names(x$plot_env$.))
  x$mapping[] <- dapr::lap(x$mapping, capitalize_labels)
  x$labels <- dapr::lap(x$labels, capitalize_labels)
  if ("facets" %in% names(x$facet$params)) {
    x$facet$params$facets <- dapr::lap(x$facet$params$facets, capitalize_labels)
  }
  if ("cols" %in% names(x$facet$params)) {
    x$facet$params$cols[] <- dapr::lap(x$facet$params$cols, capitalize_labels)
    x$facet$params$rows[] <- dapr::lap(x$facet$params$rows, capitalize_labels)
    names(x$facet$params$cols) <- capitalize_labels(names(x$facet$params$cols))
    names(x$facet$params$rows) <- capitalize_labels(names(x$facet$params$rows))
  }
  x
}



#' Append integers with appopriate suffix
#'
#' Adds "st", "nd", "rd", or "th" suffix to integers
#'
#' @param x Integer or integerish vector.
#' @return A character vector with integers converted to "th"
#' @examples
#'
#' ## randomly sample ten values from 1:100 and add suffix
#' append_nth(sample(1:100, 10))
#'
#' @export
append_nth <- function(x) {
  append_nth_ <- function(x) {
    if (is.na(x)) return(NA_character_)
    if (grepl("1$", x)) {
      paste0(x, "st")
    } else if (grepl("2$", x)) {
      paste0(x, "nd")
    } else if (grepl("3$", x)) {
      paste0(x, "rd")
    } else {
      paste0(x, "th")
    }
  }
  if (is.numeric(x) && all((x %% 1) == 0)) {
    x <- as.integer(x)
  }
  stopifnot(is.integer(x))
  vapply(x, append_nth_, character(1))
}

