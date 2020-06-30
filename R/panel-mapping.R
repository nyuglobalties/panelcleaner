#' View the default panel mapping schema
#'
#' This schema outlines the column names used by default during panel mapping
#' creation, when not overridden with the `.schema` parameter in
#' `panel_mapping()`.
#'
#' @export
default_panel_mapping_schema <- function() {
  list(
    wave_name          = "Name",
    wave_coding        = "Code",
    panel              = "GROUP",
    homogenized_name   = "HomogenizedName",
    homogenized_coding = "Coding"
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

  for (w in waves) {
    name_col <- paste0(schema$wave_name, "_", w)
    coding_col <- paste0(schema$wave_coding, "_", w)

    if (!name_col %in% names(df)) {
      tk_err("Wave {ui_value(w)} variable name column {ui_value(name_col)} not found.")
    }

    if (!coding_col %in% names(df)) {
      tk_err("Homogenized {ui_value(w)} variable coding column {ui_value(coding_col)} not found.")
    }
  }

  df <- prep_mapping(df)

  structure(
    df,
    class = c("panel_mapping", class(df)),
    schema = schema
  )
}

panel_mapping_schema <- function(x) {
  attr(x, "schema")
}

prep_mapping <- function(df) {
  df <- dplyr::as_tibble(df)
  df <- dplyr::mutate_if(df, is.character, function(x) dplyr::na_if(x, ""))

  df
}

panel_mapping_name_columns <- function(panel_mapping) {
  tk_assert(is.panel_mapping(panel_mapping))

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
  tk_assert(is.panel_mapping(panel_mapping))

  schema <- panel_mapping_schema(panel_mapping)

  list(
    homogenized_name = schema$homogenized_name,
    homogenized_coding = schema$homogenized_coding,
    wave_codings = grep(
      paste0("^", schema$wave_coding, "_"),
      names(panel_mapping),
      value = TRUE
    )
  )
}

is.panel_mapping <- function(x) {
  inherits(x, "panel_mapping")
}
