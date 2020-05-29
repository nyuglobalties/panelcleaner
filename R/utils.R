`%||%` <- function(x, y) if (is.null(x)) y else x

# Aliases to common vapply calls
vlapply <- function(x, f, ...) vapply(x, f, logical(1L), ...)
viapply <- function(x, f, ...) vapply(x, f, integer(1L), ...)
vcapply <- function(x, f, ...) vapply(x, f, character(1L), ...)

# Positive integer checking.. good for allowing doubles for indexed situations
is_positive_integer <- function(x) {
  if (!is.numeric(x)) {
    return(FALSE)
  }

  isTRUE(all.equal(rep(0, length(x)), x %% 1)) &&
    all(x > 0)
}

cat_line <- function(x = NULL) {
  cat(x, "\n", sep = "")
}

ui_value <- function(x) {
  paste0("'", x, "'")
}

ui_quote <- function(x) {
  paste0("`", x, "`")
}

tk_err <- function(x, .envir = parent.frame()) {
  msg <- glue(glue_collapse(x), .envir = .envir)

  abort(.subclass = "tk_error", message = msg)
}

tk_warn <- function(x, .envir = parent.frame()) {
  msg <- glue(glue_collapse(x), .envir = .envir)

  warn(.subclass = "tk_warning", message = msg)
}

tk_assert <- function(x, msg = NULL, .envir = parent.frame()) {
  if (is.null(msg)) {
    deparsed <- deparse(substitute(x))
    msg <- glue("Assertion {ui_quote(deparsed)} not met")
  } else {
    msg <- glue(glue_collapse(msg, "\n"), .envir = .envir)
  }

  if (!isTRUE(x)) {
    tk_err(msg)
  }

  invisible()
}

dots_partition <- function(...) {
  dots <- dots_list(...)

  if (is.null(names(dots))) {
    is_named <- rep(FALSE, length(dots))
  } else {
    is_named <- names(dots) != ""
  }

  list(
    named = dots[is_named],
    unnamed = dots[!is_named]
  )
}
