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

recoding_tree <- function(cm) {
  tk_assert(inherits(cm, "coding_mapping"))

  schema <- panel_mapping_schema(cm)
  code_cols <- c(schema$homogenized_coding, schema$wave_codings)

  cm <- dplyr::mutate_at(cm, dplyr::vars(code_cols), function(x) lapply(x, rcoder::eval_coding))

  for (cc in code_cols) {
    cm <- dplyr::mutate(!!as.name(paste0(cc, "_empty")) := lapply(!!as.name(cc), rcoder::is_empty_coding))
  }
}

coding_string_to_expr <- function(coding_str) {
  tk_assert(is.character(coding_str))

  lapply(coding_str, function(cs) {
    if (isTRUE(is.na(cs))) {
      bquote(coding())
    } else {
      rlang::parse_expr(cs)
    }
  })
}
