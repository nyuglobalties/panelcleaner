coding_mapping <- function(mapping) {
  tk_assert(is.panel_mapping(mapping))

  coding_schema_cols <- panel_mapping_coding_columns(mapping)
  coding_schema_cols_names <- unlist(coding_schema_cols)

  cm_subset <- dplyr::select(mapping, unname(coding_schema_cols_names))

  # Convert coding cols to expressions only
  coding_cols <- c(coding_schema_cols$homogenized_coding, coding_schema_cols$wave_codings)
  cm_subset <- dplyr::mutate_at(cm_subset, dplyr::vars(coding_cols), coding_string_to_expr)

  structure(
    cm_subset,
    schema = coding_schema_cols,
    class = c(class(cm_subset), "coding_mapping")
  )
}

homogenize_wave_codings <- function(panel, w, long_map, ctx = list()) {
  long_map <- dplyr::filter(long_map, .data$wave == w)
  long_map <- dplyr::filter(long_map, !is.na(.data[[panel_mapping_schema(long_map)$homogenized_coding]]))

  if (nrow(long_map) < 1) {
    return(panel)
  }

  wave_db <- wave(panel, w)

  for (variable in long_map[[panel_mapping_schema(long_map)$homogenized_name]]) {
    func <- variable_recoding_func(variable, panel, long_map, w)
    wave_db[[variable]] <- func(wave_db[[variable]])
  }

  amend_wave(panel, w, wave_db)
}

variable_recoding_func <- function(variable_name, panel, long_map, wave) {
  long_map <- long_map[
    long_map[[panel_mapping_schema(long_map)$homogenized_name]] == variable_name &
      long_map$wave == wave,
  ]

  homogenized_coding <- long_map[[panel_mapping_schema(long_map)$homogenized_coding]]
  homogenized_coding <- safe_eval_coding(homogenized_coding)

  wave_coding <- long_map[[panel_mapping_schema(long_map)$wave_coding]]
  wave_coding <- safe_eval_coding(wave_coding)

  from_list <- list(wave_coding)
  names(from_list) <- long_map$wave

  linked_codings <- rcoder::link_codings(homogenized_coding, from_list)
  rcoder::make_recode_query(linked_codings, long_map$wave)
}

safe_eval_coding <- function(coding_str) {
  coding_expr <- tryCatch(
    rlang::parse_expr(coding_str),
    error = function(e) tk_err(e$message)
  )

  tryCatch(
    rcoder::eval_coding(coding_expr),
    error = function(e) {
      tk_err("Could not evaluate: {coding_str}")
    }
  )
}

coding_string_to_expr <- function(coding_str) {
  tk_assert(is.character(coding_str))

  lapply(coding_str, function(cs) {
    if (isTRUE(is.na(cs))) {
      quote(coding())
    } else {
      rlang::parse_expr(cs)
    }
  })
}
