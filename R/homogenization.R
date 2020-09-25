#' Homogenize all waves to consistent structure
#'
#' Once all waves are collected into a single `unhomogenized_panel` object, this
#' will homogenize variable names and, where applicable, categorical codings
#' according to a panel mapping.
#'
#' @param panel An unhomogenized panel
#' @param mapping A panel mapping. If NULL, a panel mapping must be attached
#'   to the `panel` object using `add_mapping()`
#' @param ... Parameters to be used for context, usually for defining a panel
#'   schema
#' @return An `unhomogenized_panel` that is ready to be homogenized using
#'   `bind_waves()`
#'
#' @export
homogenize_panel <- function(panel, mapping = NULL, ...) {
  tk_assert(is_unhomogenized_panel(panel))
  tk_assert(is.panel_mapping(panel) || is.null(mapping))

  if (is.null(mapping) && is.null(panel$mapping)) {
    tk_err(c(
      "No mapping provided for homogenization.\n",
      "Either use `add_mapping()` to attach a mapping to the panel,\n",
      "or use the `mapping` parameter."
    ))
  }

  mapping <- mapping %||% panel$mapping
  context <- list(...)

  # Get subset of mapping just for this panel
  mapping <- mapping[mapping[[panel_mapping_schema(mapping)$panel]] == panel$name, ]

  if (nrow(mapping) < 1) {
    tk_err("Panel {ui_quote(panel$name)} not found in panel mapping.")
  }

  panel <- homogenize_names(panel, mapping, ctx = context)
  panel <- homogenize_codings(panel, mapping, ctx = context)

  panel
}

#' @describeIn homogenize_panel Bind waves into a homogenized panel after
#'   successful homogenization
#' @export
bind_waves <- function(panel, allow_issues = FALSE, ...) {
  tk_assert(
    is_unhomogenized_panel(panel),
    "Only defined for unhomogenized panels"
  )

  if (has_issues(panel) && !allow_issues) {
    tk_err(c(
      "Panel has issues and cannot be homogenized into one data.frame.\n",
      "Please address these issues (see `issues(panel)`),\n",
      "or set `allow_issues = TRUE` if they cannot be resolved properly."
    ))
  }

  rbind(panel, allow_issues = allow_issues, ...)
}

homogenize_names <- function(panel, mapping, ctx = list()) {
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
      browser()
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

homogenize_codings <- function(panel, mapping, ctx = list()) {
  ignore_missing_codings <- ctx$ignore_missing_codings %||% FALSE
  ignore_missing_homogenized_coding <- ctx$ignore_missing_homogenized_coding %||% FALSE

  pm_codings <- panel_mapping_coding_columns(mapping)
  pm_codings_vec <- c(
    pm_codings$homogenized_name,
    pm_codings$homogenized_coding,
    pm_codings$wave_codings
  )

  map_subset <- long_map_subset(mapping, pm_codings_vec, panel$waves)
  attr(map_subset, "schema") <- panel_mapping_schema(mapping)

  # Identify variables that have codings defined but no homogenized coding
  # and vice versa.

  missing_coding <-
    !is.na(map_subset[[pm_codings$homogenized_coding]]) &
    is.na(map_subset[[panel_mapping_schema(mapping)$wave_coding]])

  missing_homogenized_coding <-
    is.na(map_subset[[pm_codings$homogenized_coding]]) &
    !is.na(map_subset[[panel_mapping_schema(mapping)$wave_coding]])

  if (any(missing_coding)) {
    if (!isTRUE(ignore_missing_codings)) {
      tk_err(c(
        "Some variables have missing codings while a homogenized coding is defined.\n",
        "To ignore this error, set `ignore_missing_codings = TRUE`"
      ))
    } else {
      bad_mappings <- dplyr::filter(map_subset, missing_coding)
      bad_mappings <- dplyr::select(
        bad_mappings,
        c("wave", panel_mapping_schema(mapping)$wave_coding)
      )

      panel <- add_issues(panel, list(ignored_missing_codings = bad_mappings))
      map_subset <- dplyr::filter(map_subset, !missing_coding)
    }
  }

  if (any(missing_homogenized_coding)) {
    if (!isTRUE(ignore_missing_homogenized_coding)) {
      tk_err(c(
        "Some variables have a missing homogenized coding while wave codings are defined.\n",
        "To ignore this error, set `ignore_missing_homogenized_coding = TRUE`"
      ))
    } else {
      bad_mappings <- dplyr::filter(map_subset, missing_homogenized_coding)
      bad_mappings <- dplyr::select(
        bad_mappings,
        c("wave", panel_mapping_schema(mapping)$homogenized_coding)
      )

      panel <- add_issues(panel, list(ignored_missing_homogenized_codings = bad_mappings))
      map_subset <- dplyr::filter(map_subset, !missing_homogenized_coding)
    }
  }

  for (w in panel$waves) {
    panel <- homogenize_wave_codings(panel, w, map_subset, ctx = ctx)
  }

  panel$homogenized_codings <- TRUE

  panel
}

homogenize_wave_names <- function(panel, w, long_map, ctx = list()) {
  error_missing_raw_variables <- ctx$error_missing_raw_variables %||% TRUE

  schema <- panel_mapping_schema(long_map)

  name_col <- as.name(schema$wave_name)

  if (!w %in% long_map[["wave"]]) {
    tk_err("Wave {ui_value(w)} not found in mapping waves.")
  }

  long_map <- long_map[long_map$wave == w, ]
  long_map <- long_map[!is.na(long_map[[name_col]]), ]

  wave_db <- wave(panel, w)

  variables <- long_map[[schema$wave_name]]

  if (any(!variables %in% names(wave_db))) {
    missing_vars <- long_map[!long_map[[schema$wave_name]] %in% names(wave_db), ][[schema$wave_name]]

    if (isTRUE(error_missing_raw_variables)) {
      tk_err(c(
        "Some variables present in mapping for {ui_value(w)} are not in the data: [",
        glue_collapse(ui_value(missing_vars), ", "), "]"
      ))
    } else {
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
