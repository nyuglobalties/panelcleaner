#' Homogenize all waves to consistent structure
#'
#' Once all waves are collected into a single `unhomogenized_panel` object, this
#' will homogenize variable names and, where applicable, categorical codings
#' according to a panel mapping.
#'
#' @param panel An unhomogenized panel
#' @param mapping A panel mapping. If NULL, a panel mapping must be attached
#'   to the `panel` object using `add_mapping()`
#' @param allow_issues If `TRUE`, will allow waves to be bound together even if
#'   there are identified issues. Use caution with this!
#' @param ... Parameters to be used for context, usually for defining a panel
#'   schema
#' @return An `unhomogenized_panel` that is ready to be homogenized using
#'   `bind_waves()`
#'
#' @details # Homogenization Steps
#'
#' The first part of the homogenization process is to harmonize wave variable names
#' to the homogenized name. If either there are missing wave variable names and a provided
#' homogenized name _or_ provided variable names and a missing homogenized name, an error
#' will be thrown. The original version of panelcleaner included a notion of "issues"
#' that would allow harmonization with errors, but after continued practice of
#' using panelcleaner, this behavior is deprecated in favor of halting the harmonization
#' process altogether.
#'
#' The next step is to harmonize the codings for categorical data. As panelcleaner was
#' intended to be used in a data processing pipeline before analysis was conducted in
#' Stata, the desired behavior of panelcleaner is to separate values from labels, unlike
#' R's `factor` class. Codings are written using [rcoder::coding()]. The harmonization process
#' is similar to names: errors will be thrown if wave codings and homogenized codings
#' aren't both present or missing, and the codings in all waves will be recoded to the
#' homogenized coding.
#'
#' ## Descriptions
#'
#' The last step is to harmonize variable descriptions. This part is optional. It will
#' only happen if the `homogenized_description` (or custom name specified with a custom
#' panelcleaner schema) is present. The same types will occur for descriptions. The only
#' thing different about harmonizing descriptions is that it doesn't affect the data:
#' it operates by assigning the `bpr.description` attribute for variables. This feature
#' is really only useful if you intend you data to be used in a
#' [blueprintr](https://nyuglobalties.github.io/blueprintr) project.
#'
#' @export
homogenize_panel <- function(panel, mapping = NULL, ...) {
  tk_assert(is_unhomogenized_panel(panel))
  tk_assert(is_panel_mapping(panel) || is.null(mapping))

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

  if (panel_mapping_schema(mapping)$homogenized_description %in% names(mapping)) {
    panel <- homogenize_descriptions(panel, mapping, ctx = context)
  }

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

  map_subset <- long_map_subset(mapping, pm_names_vec)
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

homogenize_codings <- function(panel, mapping, ctx = list()) {
  ignore_missing_codings <- ctx$ignore_missing_codings %||% FALSE
  ignore_missing_homogenized_coding <- ctx$ignore_missing_homogenized_coding %||% FALSE

  pm_codings <- panel_mapping_coding_columns(mapping)
  pm_codings_vec <- c(
    pm_codings$homogenized_name,
    pm_codings$homogenized_coding,
    pm_codings$wave_codings,
    pm_codings$wave_names
  )

  map_subset <- long_map_subset(mapping, pm_codings_vec)
  map_subset <- set_attr(map_subset, "schema", panel_mapping_schema(mapping))

  # Filter rows with missing wave_name as those are probably
  # variables that don't exist or aren't mapping
  map_subset <- map_subset[
    !is.na(map_subset[[panel_mapping_schema(mapping)$wave_name]]),
  ]

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

homogenize_descriptions <- function(panel, mapping, ctx = list()) {
  pm_descs <- panel_mapping_description_columns(mapping)
  pm_descs_vec <- c(
    pm_descs$homogenized_name,
    pm_descs$homogenized_description,
    pm_descs$wave_description,
    pm_descs$wave_names
  )

  map_subset <- long_map_subset(mapping, pm_descs_vec)
  map_subset <- set_attr(map_subset, "schema", panel_mapping_schema(mapping))

  # Filter rows with missing wave_name as those are probably
  # variables that don't exist or aren't mapping
  map_subset <- map_subset[
    !is.na(map_subset[[panel_mapping_schema(mapping)$wave_name]]),
  ]

  missing_description <-
    !is.na(map_subset[[pm_descs$homogenized_description]]) &
      is.na(map_subset[[panel_mapping_schema(mapping)$wave_description]])

  missing_homogenized_description <-
    is.na(map_subset[[pm_descs$homogenized_description]]) &
      !is.na(map_subset[[panel_mapping_schema(mapping)$wave_description]])

  if (any(missing_description)) {
    tk_err("Some variables have missing descriptions while a homogenized description is defined.")
  }

  if (any(missing_homogenized_description)) {
    tk_err(c(
      "Some variables have missing homogenized codings ",
      "while wave codings are defined."
    ))
  }

  for (w in panel$waves) {
    panel <- homogenize_wave_descriptions(panel, w, map_subset, ctx = ctx)
  }

  panel$homogenized_descriptions <- TRUE
  panel
}

#' Add blueprintr descriptions to wave variables
#'
#' Adds the `bpr.description` attribute to a wave's variables.
#' This is really only useful when used in conjunction with blueprintr
#' as the attribute will be identified and propagated in the
#' blueprint provenance.
#'
#' @param panel A `panel` object
#' @param w A wave identifier
#' @param long_map A long-form mapping for the particular panel
#' @param ctx A list of potential context flags
#' @return `panel` with amended `bpr.description` attributes
#' @noRd
homogenize_wave_descriptions <- function(panel, w, long_map, ctx = list()) {
  schema <- panel_mapping_schema(long_map)

  if (!w %in% long_map[["wave"]]) {
    tk_err("Wave {ui_value(w)} not found in mapping waves.")
  }

  long_map <- long_map[long_map$wave == w, ]
  long_map <- long_map[!is.na(long_map[[schema$wave_name]]), ]

  wave_db <- wave(panel, w)
  variables <- long_map[[schema$homogenized_name]]

  for (v in variables) {
    sub_map <- long_map[long_map[[schema$homogenized_name]] == v, ]

    wave_db[[v]] <- set_attr(
      wave_db[[v]],
      "bpr.description",
      sub_map[[schema$homogenized_description]]
    )
  }

  amend_wave(panel, w, wave_db)
}

homogenize_wave_names <- function(panel, w, long_map, ctx = list()) {
  error_missing_raw_variables <- ctx$error_missing_raw_variables %||% TRUE

  schema <- panel_mapping_schema(long_map)

  if (!w %in% long_map[["wave"]]) {
    tk_err("Wave {ui_value(w)} not found in mapping waves.")
  }

  long_map <- long_map[long_map$wave == w, ]
  long_map <- long_map[!is.na(long_map[[schema$wave_name]]), ]

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
      names(issue) <- glue("missing_raw_variables_{w}")

      panel <- add_issues(panel, issue)

      # Subset to known variables
      long_map <- long_map[long_map[[schema$wave_name]] %in% names(wave_db), ]
      variables <- variables[variables %in% names(wave_db)]
    }
  }

  names(variables) <- long_map[[schema$homogenized_name]]

  wave_db <- dplyr::select(wave_db, !!!variables)
  amend_wave(panel, w, wave_db)
}

long_map_subset <- function(mapping, columns) {
  map_subset <- dplyr::select(mapping, dplyr::all_of(columns))
  wave_tags <- panel_mapping_waves(mapping)

  tidyr::pivot_longer(
    as.data.frame(map_subset),
    cols = dplyr::matches(glue("(.*)_({glue_collapse(wave_tags, '|')})$")),
    names_to = c(".value", "wave"),
    names_pattern = "(.*)_(.*)"
  )
}
