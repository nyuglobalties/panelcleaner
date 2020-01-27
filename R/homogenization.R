homogenize_panel <- function(panel, mapping, allow_issues = FALSE, ...) {
  tk_assert(is_unhomogenized_panel(panel))
  tk_assert(is.panel_mapping(panel))

  context <- list(...)

  panel <- homogenize_names(panel, mapping, ctx = context)

  if (has_issues(panel) && !allow_issues) {
    tk_err(c(
      "Panel has issues and cannot be homogenized into one data.frame.\n",
      "Please address these issues (see `issues(panel)`),\n",
      "or set `allow_issues = TRUE` if they cannot be resolved properly."
    ))
  }
}

homogenize_names <- function(panel,
                             mapping, 
                             ctx = list()) {
  drop_na_homogenized <- ctx$drop_na_homogenized %||% TRUE

  pm_names <- panel_mapping_name_columns(mapping)
  pm_names_vec <- c(pm_names$homogenized_name, pm_names$wave_names)

  map_subset <- long_map_subset(mapping, pm_names_vec, panel$waves)
  attr(map_subset, "schema") <- panel_mapping_schema(mapping)

  if (any(is.na(map_subset[[pm_names$homogenized_name]]))) {
    if (!isTRUE(drop_na_homogenized)) {
      tk_err(c(
        "Not all variables could be homogenized due to missing homogenized names.\n",
        "If you'd like to prevent this in the future,\n",
        "use `drop_na_homogenized = TRUE`."
      ))
    } else {
      bad_mappings <- dplyr::filter(map_subset, is.na(.data[[pm_names$homogenized_name]]))
      bad_mappings <- dplyr::select(
        bad_mappings, 
        c("wave", panel_mapping_schema(mapping)$wave_name)
      )

      panel <- add_issues(panel, list(dropped_missing_homogenized_name = bad_mappings))
      map_subset <- dplyr::filter(map_subset, !is.na(.data[[pm_names$homogenized_name]]))
    }
  }

  for (w in panel$waves) {
    panel <- homogenize_wave_names(panel, w, map_subset, ctx = ctx)
  }

  panel$homogenized_names <- TRUE

  panel
}

homogenize_wave_names <- function(panel, 
                                  w, 
                                  long_map, 
                                  ctx = list()) {
  error_missing_raw_variables <- ctx$error_missing_raw_variables %||% FALSE

  schema <- panel_mapping_schema(long_map)

  name_col <- as.name(schema$wave_name)

  if (!w %in% long_map[["wave"]]) {
    tk_err("Wave {ui_value(w)} not found in mapping waves.")
  }

  long_map <- dplyr::filter(long_map, .data$wave == w)
  long_map <- dplyr::filter(long_map, !is.na(.data[[name_col]]))

  wave_db <- wave(panel, w)

  variables <- long_map[[schema$wave_name]]

  if (any(!variables %in% names(wave_db))) {
    if (isTRUE(error_missing_raw_variables)) {
      tk_err("Some variables present in mapping for {ui_label(w)} are not in the data.")
    } else {
      missing_vars <- dplyr::filter(long_map, .data[[schema$wave_name]] %in% names(wave_db))
      missing_vars <- dplyr::pull(missing_vars, schema$wave_name)

      issue <- list(missing_vars)
      names(issue) <- glue("missing_raw_vars_{w}")

      panel <- add_issues(panel, issue)
    }
  }

  names(variables) <- long_map[[schema$homogenized_name]]

  wave_db <- dplyr::select(wave_db, !!!variables)
  amend_wave(panel, w, wave_db)
}


long_map_subset <- function(mapping, columns, wave_tags) {
  map_subset <- dplyr::select(mapping, columns)

  tidyr::pivot_longer(
    as.data.frame(map_subset),
    cols = dplyr::matches(glue("(.*)_({glue_collapse(wave_tags, '|')})$")),
    names_to = c(".value", "wave"),
    names_pattern = "(.*)_(.*)"
  )
}
