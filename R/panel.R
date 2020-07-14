#' Collect multiple waves of data into a panel
#'
#' @param name The name of the panel data, e.g. an assessment name
#' @param ... The waves of the panel. If unnamed arguments are used, the
#'   parameter order will be used as the wave tags. If named parameters are
#'   used, the names of the parameters will be used as the wave tags.
#' @param .id_col The name of the column that has the observed participant IDs.
#'   Multiple names can be used if multiple columns uniquely identify
#'   participants.
#' @param .waves_col The column name for the wave tags once the panel is
#'   homogenized and bound together into a single `data.frame`
#' @return An `unhomogenized_panel` object
#'
#' @export
enpanel <- function(name,
                    ...,
                    .id_col = "id",
                    .waves_col = "wave") {
  tk_assert(
    is.character(name),
    "Panels must have a character name to enable homogenization."
  )

  dots <- dots_partition(...)

  for (.dotsplit in dots) {
    tk_assert(all(vlapply(.dotsplit, is.data.frame)))
  }

  if (length(dots$unnamed) > 0) {
    names(dots$unnamed) <- as.character(1:length(dots$unnamed))
  }

  if (length(dots$named) > 0 && length(dots$unnamed) > 0) {
    tk_warn(c(
      "Using both indexed-based and string-based wave tags together.\n",
      "This could be a mistake. Either use named parameters or unnamed, but ",
      "not both."
    ))
  }

  data <- c(dots$named, dots$unnamed)
  waves <- c(names(dots$named), names(dots$unnamed))

  structure(
    list(
      name = name,
      data = data,
      waves = waves,
      id_col = .id_col,
      waves_col = .waves_col
    ),
    class = c("unhomogenized_panel", "tk_panel")
  )
}

#' Add a wave of data to a panel
#'
#' Adds a wave of data to a panel, assigned to wave `wave`
#'
#' @param panel A panel object
#' @param data A `data.frame`-like object
#' @param wave The wave tag associated with this dataset
#' @param ... Other arguments passed to methods
#'
#' @export
add_wave <- function(panel, data, wave, ...) {
  UseMethod("add_wave")
}

#' Add multiple waves of data to a panel
#'
#' @param panel A panel object
#' @param data A list of `data.frame`-like objects, with the list names being
#'   wave tags for their respective datasets
#' @param ... Other arguments passed to methods
#'
#' @export
add_waves <- function(panel, data, ...) {
  UseMethod("add_waves")
}

#' @export
add_wave.default <- function(panel, data, wave, ...) {
  tk_err("Only defined for panel data. See `?enpanel` for more information.")
}

#' @export
add_waves.default <- function(panel, data, ...) {
  tk_err("Only defined for panel data. See `?enpanel` for more information.")
}

#' @export
add_wave.unhomogenized_panel <- function(panel, data, wave, ...) {
  if (!is.data.frame(data)) {
    if (is.list(data)) {
      tk_err("add_wave is for a single data.frame. Use add_waves to add multiple waves at once.")
    } else {
      tk_err("add_wave is for a single data.frame. See `?add_wave` for more information.")
    }
  }

  if (wave %in% panel$waves) {
    tk_err("Wave {ui_value(wave)} already exists in panel.")
  }

  panel$data[[wave]] <- data
  panel$waves <- c(panel$waves, wave)

  panel
}

#' @export
add_wave.unhomogenized_panel <- function(panel, data, wave, ...) {
  tk_assert(is.numeric(wave) || is.character(wave))
  tk_assert(length(wave) == 1)

  if (!is.data.frame(data)) {
    if (is.list(data)) {
      tk_err("add_wave is for a single data.frame. Use add_waves to add multiple waves at once.")
    } else {
      tk_err("add_wave is for a single data.frame. See `?add_wave` for more information.")
    }
  }

  if (wave %in% panel$waves) {
    tk_err("Wave {ui_value(wave)} already exists in panel.")
  }

  if (is.numeric(wave) && is.character(panel$waves)) {
    tk_err("Panel {ui_value(panel$name)} waves are character tags, not integers.")
  }

  if (is.character(wave) && is.numeric(panel$waves)) {
    tk_err("Panel {ui_value(panel$name)} waves are integers, not character tags.")
  }

  panel$data[[wave]] <- data
  panel$waves <- c(panel$waves, wave)

  panel
}

#' @export
add_wave.homogenized_panel <- function(panel, data, wave, ...) {
  dots <- rlang::dots_list(...)
  .force <- dots$.force %||% FALSE

  if (!isTRUE(.force)) {
    tk_err(c(
      "Adding waves is by default only allowed for unhomogenized panels,",
      "so that data verification happens in one step.\n",
      "If you are sure your new wave does not already exist in this panel,",
      "use `.force = TRUE` to allow adding the wave."
    ))
  }

  panel$data <- dplyr::bind_rows(panel$data, data)

  panel
}

#' Add a variable mapping to a panel
#'
#' @param panel A panel object
#' @param mapping A `data.frame` to be used to map out variables over each wave
#' @param ... Parameters passed onto other methods
#' @return The input panel with a new mapping
#'
#' @export
add_mapping <- function(panel, mapping, ...) {
  UseMethod("add_mapping")
}


#' @describeIn add_mapping The default method
#' @export
add_mapping.default <- function(panel, mapping, ...) {
  tk_err("Undefined operation")
}

#' @describeIn add_mapping Method for unhomogenized panels
#' @export
add_mapping.unhomogenized_panel <- function(panel, mapping, ...) {
  tk_assert(is.panel_mapping(mapping))

  panel$mapping <- mapping
  panel
}

link_wave_names <- function(data, waves) {
  if (is.character(waves)) {
    if (is.null(names(data))) {
      names(data) <- waves
    } else {
      if (!setequal(names(data), waves)) {
        tk_err("Data list does not match provided wave tags")
      }
    }
  } else {
    if (!is.null(names(data))) {
      warn("Panel data list has names, presumably for waves, but waves are not character data.")
    }
  }

  data
}

#' Extract a wave dataset from a panel object
#'
#' Gets the `data.frame` assigned to `wave_id` from a panel.
#'
#' @param x A panel, currently an unhomogenized panel
#' @param wave_id The wave tag to which the dataset is assigned
#' @param ... Other parameters passed to other methods
#'
#' @export
wave <- function(x, wave_id, ...) {
  UseMethod("wave")
}

#' @export
wave.unhomogenized_panel <- function(x, wave_id, ...) {
  tk_assert(is_positive_integer(wave_id) || is.character(wave_id))

  if (is.null(x$data[[wave_id]])) {
    tk_err("Wave {ui_value(wave_id)} not found.")
  }

  x$data[[wave_id]]
}

#' Modify a wave's dataset
#'
#' @param x A panel, currently an unhomogenized panel
#' @param wave_id The desired wave tag
#' @param wave_db The amended dataset
#' @param ... Other parameters passed to methods
#'
#' @export
amend_wave <- function(x, wave_id, wave_db, ...) {
  UseMethod("amend_wave")
}

#' @export
amend_wave.unhomogenized_panel <- function(x, wave_id, wave_db, ...) {
  tk_assert(is_positive_integer(wave_id) || is.character(wave_id))
  tk_assert(is.data.frame(wave_db))

  if (is.null(x$data[[wave_id]])) {
    tk_err("Wave {ui_value(wave_id)} not found.")
  }

  x$data[[wave_id]] <- wave_db

  x
}

is_unhomogenized_panel <- function(x) {
  inherits(x, "unhomogenized_panel")
}

is_homogenized_panel <- function(x) {
  inherits(x, "homogenized_panel")
}

#' @export
print.unhomogenized_panel <- function(x, ...) {
  cat_line(glue("<Unhomogenized Panel: {ui_value(x$name)}>"))
  cat_line(glue("Waves: [{glue_collapse(ui_value(x$waves), ', ')}]"))

  if (!is.null(x$id_col)) {
    cat_line(glue("ID column: {ui_value(x$id_col)}"))
  } else {
    cat_line(glue("ID column: <NOT SPECIFIED>"))
  }

  if (!is.null(x$waves_col)) {
    cat_line(glue("Wave column: {ui_value(x$waves_col)}"))
  } else {
    cat_line(glue("Wave column: <NOT SPECIFIED>"))
  }

  if (!is.null(x$mapping)) {
    cat_line("Attached mapping content:")
    print(x$mapping)
  } else {
    cat_line("No mapping attached")
  }

  for (wave in x$waves) {
    cat_line()
    cat_line(glue("{ui_value(wave)} content:"))

    print(dplyr::as_tibble(x$data[[wave]]))
  }

  if (has_issues(x)) {
    cat_line()
    cat_line("Has issues. See `issues()` for more information.")
  }

  invisible()
}

#' @export
rbind.unhomogenized_panel <- function(x, allow_issues = FALSE, ...) {
  if (has_issues(x) && !isTRUE(allow_issues)) {
    tk_err(c(
      "Panel {x$name} not able to be homogenized due to issues.\n",
      "If you want to bypass this, use `allow_issues = TRUE`"
    ))
  }

  if (!isTRUE(x$homogenized_names)) {
    tk_err("Names have not been homogenized.")
  }

  if (!isTRUE(x$homogenized_codings)) {
    tk_err("Codings have not been homogenized.")
  }

  for (w in x$waves) {
    wave_db <- wave(x, w)
    wave_db[[x$waves_col]] <- w
    x <- amend_wave(x, w, wave_db)
  }

  dat <- dplyr::bind_rows(!!!x$data)

  # Copy metadata over from unhomogenized_panel
  non_data_params <- names(x)[names(x) != "data"]

  homogenized_panel <- list(data = dat)
  homogenized_panel[non_data_params] <- x[non_data_params]

  structure(
    homogenized_panel,
    class = c("homogenized_panel", "tk_panel")
  )
}

#' @export
print.homogenized_panel <- function(x, ...) {
  cat_line(glue("<Homogenized Panel: {ui_value(x$name)}>"))
  cat_line(glue("Waves: [{glue_collapse(ui_value(x$waves), ', ')}]"))

  if (!is.null(x$id_col)) {
    cat_line(glue("ID column: {ui_value(x$id_col)}"))
  } else {
    cat_line(glue("ID column: <NOT SPECIFIED>"))
  }

  if (!is.null(x$waves_col)) {
    cat_line(glue("Wave column: {ui_value(x$waves_col)}"))
  } else {
    cat_line(glue("Wave column: <NOT SPECIFIED>"))
  }

  if (!is.null(x$mapping)) {
    cat_line("Attached mapping content:")
    print(x$mapping)
  } else {
    cat_line("No mapping attached")
  }

  cat_line()
  cat_line(glue("Content:"))

  print(dplyr::as_tibble(x$data))
}

#' @export
as.data.frame.homogenized_panel <- function(x, ...) {
  structure(
    x$data,
    class = c("mapped_df", class(x$data)),
    mapping = x$mapping,
    id_col = x$id_col,
    waves_col = x$waves_col
  )
}
