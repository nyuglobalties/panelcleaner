enpanel <- function(data,
                    name,
                    waves = NULL,
                    id_col = "id", 
                    waves_col = "wave", 
                    ...) {
  tk_assert(is.character(name), "Panels must have a character name to enable homogenization.")

  tk_assert(
    is.character(waves) || 
      is_positive_integer(waves) ||
      is.null(waves)
  )

  if (is.data.frame(data)) {
    waves <- waves %||% 1L

    tk_assert(length(waves) == 1L)

    dat <- link_wave_names(list(data), waves)

    structure(
      list(
        data = dat,
        name = name,
        waves = waves,
        id_col = id_col,
        waves_col = waves_col,
        ...
      ),
      class = c("unhomogenized_panel", "tk_panel")
    )
  } else if (is.list(data)) {
    waves <- waves %||% names(data) %||% 1:length(data)

    tk_assert(length(waves) == length(data))
    tk_assert(all(vlapply(data, is.data.frame)))

    structure(
      list(
        data = link_wave_names(data, waves),
        name = name,
        waves = waves,
        id_col = id_col,
        waves_col = waves_col,
        ...
      ),
      class = c("unhomogenized_panel", "tk_panel")
    )
  } else {
    tk_err("enpanel only accepts a dataframe or list of dataframes.") 
  }
}

add_wave <- function(panel, data, wave, ...) {
  UseMethod("add_wave")
}

add_waves <- function(panel, data, ...) {
  UseMethod("add_waves")
}

add_wave.default <- function(panel, data, wave) {
  tk_err("Only defined for panel data. See `?enpanel` for more information.")
}

add_waves.default <- function(panel, data, ...) {
  tk_err("Only defined for panel data. See `?enpanel` for more information.")
}

add_wave.unhomogenized_panel <- function(panel, data, wave) {
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

add_wave.unhomogenized_panel <- function(panel, data, wave) {
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

add_wave.homogenized_panel <- function(panel, data, wave, .force = FALSE) {
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

add_mapping <- function(x, mapping, ...) {
  UseMethod("add_mapping")
}

add_mapping.default <- function(x, mapping, ...) {
  tk_err("Underdefined operation")
}

add_mapping.unhomogenized_panel <- function(x, mapping) {
  tk_assert(is.panel_mapping(mapping))

  x$mapping <- mapping
  x
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

wave <- function(x, wave_id, ...) {
  UseMethod("wave")
}

wave.unhomogenized_panel <- function(x, wave_id) {
  tk_assert(is_positive_integer(wave_id) || is.character(wave_id))

  if (is.null(x$data[[wave_id]])) {
    tk_err("Wave {ui_value(wave_id)} not found.")
  }

  x$data[[wave_id]]
}

amend_wave <- function(x, wave_id, wave_db, ...) {
  UseMethod("amend_wave")
}

amend_wave.unhomogenized_panel <- function(x, wave_id, wave_db) {
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

print.unhomogenized_panel <- function(x) {
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

print.homogenized_panel <- function(x) {
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

as.data.frame.homogenized_panel <- function(x, ...) {
  structure(
    x$data,
    class = c("mapped_df", class(x$data)),
    mapping = x$mapping,
    id_col = x$id_col,
    waves_col = x$waves_col
  )
}
