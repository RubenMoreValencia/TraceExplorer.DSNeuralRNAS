#' Prepare semantic lane data for article comparison plots
#'
#' Each scientific case receives its own lane. This helper does not alter Psi,
#' Gamma, Phi, iteration, or normalized time.
#'
#' @param x te_article_case_set.
#' @return data.frame.
#' @export
te_article_case_set_semantic_lanes <- function(x) {
  d <- te_article_case_set_semantic_timeline(x)

  if (!nrow(d)) {
    d$display_case <- character(0)
    d$lane_id <- integer(0)
    return(d)
  }

  d$display_case <- paste0(
    d$experiment,
    " \u00b7 ",
    d$case_id
  )

  case_levels <- unique(d$display_case)

  d$lane_id <- match(
    d$display_case,
    case_levels
  )

  d
}
