issues <- function(x) {
  UseMethod("issues")
}

empty_issues <- function() {
  structure(
    list(),
    class = "tk_issues"
  )
}

has_issues <- function(x) {
  !identical(issues(x), empty_issues())
}

issues.tk_panel <- function(x) {
  out <- attr(x, "issues")

  if (is.null(out)) {
    out <- empty_issues()
  }

  out
}

print.tk_issues <- function(x) {
  if (identical(x, empty_issues())) {
    cat_line("<no issues>")
    return(invisible())
  }

  for (name in names(x)) {
    cat_line(glue("<issue: {ui_value(name)}>"))
    print(x[[name]])
  }

  invisible()
}

add_issues <- function(x, issues, ...) {
  UseMethod("add_issues")
}

add_issues.tk_panel <- function(x, issues) {
  if (!is.list(issues) || is.data.frame(issues)) {
    tk_err(c(
      "Directly assigning an object to a panel's issues is not allowed.\n",
      "You must use `list(<issue> = <object>)`."
    ))
  }

  if (is.null(names(issues))) {
    tk_err("Please specify issue tag(s) by using a named list.")
  }

  panel_issues <- issues(x)

  for (name in names(issues)) {
    if (name %in% names(panel_issues)) {
      tk_err("{ui_value(name)} already tagged as a issue in panel.")
    }

    panel_issues[[name]] <- issues[[name]]
  }

  attr(x, "issues") <- panel_issues

  invisible(x)
}
