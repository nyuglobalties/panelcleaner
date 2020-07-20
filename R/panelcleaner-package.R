#' @importFrom rlang abort warn .data :=
#' @importFrom glue glue glue_collapse
#' @keywords internal
#' @details
#'
#' The panelcleaner package addresses data structure issues with longitudinal
#' data by defining two data structures: a `tk_panel` class that is a wrapper
#' around the waves of _tabular_ data, and a `panel_map` class which is a type
#' of [`data.frame`][base::data.frame()] that outlines the raw data content
#' (variable names and categorical levels) and a common "homogenized" form that
#' is consistent across all waves.
#'
#' Resources for workflow:
#'   * Defining a panel from waves of data: [enpanel()], [add_wave()]
#'   * Accessing waves of data from an unhomogenized panel: [wave()]
#'   * Defining a panel mapping: [panel_mapping()]
#'   * Adding a panel mapping to a panel: [add_mapping()]
#'   * Homogenize a panel: [homogenize_panel()]
#'   * Combine waves and extract long-form `data.frame`: [bind_waves()]
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
