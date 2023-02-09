#' View the default panel mapping schema
#'
#' This schema outlines the column names used by default during panel mapping
#' creation, when not overridden with the `.schema` parameter in
#' `panel_mapping()`.
#'
#' @export
default_panel_mapping_schema <- function() {
  list(
    wave_name = "name",
    wave_coding = "coding",
    wave_description = "description",
    panel = "panel",
    homogenized_name = "homogenized_name",
    homogenized_coding = "homogenized_coding",
    homogenized_description = "homogenized_description"
  )
}

make_panel_mapping_schema <- function(schema) {
  out <- default_panel_mapping_schema()

  if (length(schema) > 0 && !is.null(names(schema))) {
    toset <- names(schema)[names(schema) %in% names(out)]
    out[toset] <- schema[toset]
  }

  out
}

#' Create a panel mapping
#'
#' @param df A `data.frame` that has variable mapping information
#' @param waves A vector of all known waves recorded in the mapping file
#' @param .schema A list of column names that outline the structure of the
#'   mapping file. `default_panel_mapping_schema()` has the default settings.
#'
#' @export
panel_mapping <- function(df, waves, .schema = list()) {
  tk_assert(is.data.frame(df))
  tk_assert(is.numeric(waves) || is.character(waves))

  schema <- make_panel_mapping_schema(.schema)

  if (!schema$panel %in% names(df)) {
    tk_err("Panel name column {ui_value(schema$panel)} not found.")
  }

  if (!schema$homogenized_name %in% names(df)) {
    tk_err("Homogenized variable name column {ui_value(schema$homogenized_name)} not found.")
  }

  if (!schema$homogenized_coding %in% names(df)) {
    tk_err("Homogenized variable coding column {ui_value(schema$homogenized_coding)} not found.")
  }

  if (any(is.na(df[[schema$panel]]))) {
    tk_err("{ui_value(schema$panel)} has missing values. Please fix this!")
  }

  for (w in waves) {
    name_col <- paste0(schema$wave_name, "_", w)
    coding_col <- paste0(schema$wave_coding, "_", w)
    description_col <- paste0(schema$wave_description, "_", w)

    if (!name_col %in% names(df)) {
      tk_err("Wave {ui_value(w)} variable name column {ui_value(name_col)} not found.")
    }

    if (!coding_col %in% names(df)) {
      tk_err("Homogenized {ui_value(w)} variable coding column {ui_value(coding_col)} not found.")
    }
  }

  # Map descriptions if homogenized description exists
  if (schema$homogenized_description %in% names(df)) {
    for (w in waves) {
      if (!description_col %in% names(df)) {
        tk_err("Homogenized {ui_value(w)} variable description column {ui_value(description_col)} not found.")
      }
    }
  }

  for (p in unique(df[[schema$panel]])) {
    validate_panel(df, p, waves, schema)
  }

  df <- prep_mapping(df)

  structure(
    df,
    class = c("panel_mapping", class(df)),
    schema = schema,
    waves = waves
  )
}

validate_panel <- function(df, p, waves, schema) {
  df <- df[df[[schema$panel]] == p, ]

  # Don't allow duplicates in names
  homogenized_names <- df[[schema$homogenized_name]]
  homogenized_names <- homogenized_names[!is.na(homogenized_names)]

  if (any(duplicated(homogenized_names))) {
    bad_homogenized_names <- unique(homogenized_names[duplicated(homogenized_names)])
    bad_homogenized_names <- paste0("- ", bad_homogenized_names, "\n")

    tk_err(c("Duplicated homogenized names encountered in {ui_value(p)}:\n", bad_homogenized_names))
  }

  for (w in waves) {
    name_col <- paste0(schema$wave_name, "_", w)

    wave_vars <- df[[name_col]]
    wave_vars <- wave_vars[!is.na(wave_vars)]

    if (any(duplicated(wave_vars))) {
      bad_wave_vars <- unique(wave_vars[duplicated(wave_vars)])
      bad_wave_vars <- paste0("- ", bad_wave_vars, "\n")

      tk_err(c("Duplicated wave names encountered in {ui_value(w)}:\n", bad_wave_vars))
    }
  }
}

panel_mapping_schema <- function(x) {
  attr(x, "schema", exact = TRUE)
}

panel_mapping_waves <- function(x) {
  attr(x, "waves", exact = TRUE)
}

prep_mapping <- function(df) {
  df <- dplyr::as_tibble(df)
  df <- dplyr::mutate_if(df, is.character, function(x) dplyr::na_if(x, ""))

  df
}

panel_mapping_name_columns <- function(panel_mapping) {
  tk_assert(is_panel_mapping(panel_mapping))

  schema <- panel_mapping_schema(panel_mapping)

  list(
    homogenized_name = schema$homogenized_name,
    wave_names = grep(
      paste0("^", schema$wave_name, "_"),
      names(panel_mapping),
      value = TRUE
    )
  )
}

panel_mapping_coding_columns <- function(panel_mapping) {
  tk_assert(is_panel_mapping(panel_mapping))

  schema <- panel_mapping_schema(panel_mapping)

  list(
    homogenized_name = schema$homogenized_name,
    homogenized_coding = schema$homogenized_coding,
    wave_codings = grep(
      paste0("^", schema$wave_coding, "_"),
      names(panel_mapping),
      value = TRUE
    ),
    wave_names = grep(
      paste0("^", schema$wave_name, "_"),
      names(panel_mapping),
      value = TRUE
    )
  )
}

panel_mapping_description_columns <- function(panel_mapping) {
  tk_assert(is_panel_mapping(panel_mapping))

  schema <- panel_mapping_schema(panel_mapping)

  list(
    homogenized_name = schema$homogenized_name,
    homogenized_description = schema$homogenized_description,
    wave_descriptions = grep(
      paste0("^", schema$wave_description, "_"),
      names(panel_mapping),
      value = TRUE
    ),
    wave_names = grep(
      paste0("^", schema$wave_name, "_"),
      names(panel_mapping),
      value = TRUE
    )
  )
}

is_panel_mapping <- function(x) {
  inherits(x, "panel_mapping")
}
