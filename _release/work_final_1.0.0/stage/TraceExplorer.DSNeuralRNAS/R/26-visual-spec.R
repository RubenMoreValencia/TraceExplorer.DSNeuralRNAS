#' Visual specification for TraceExplorer plots
#'
#' Keeps didactic and research rendering consistent without changing evidence.
#'
#' @param mode Either student/didactic or research.
#' @return Named list of visual sizes.
#' @export
te_visual_spec <- function(mode = c("student", "research")) {
  mode <- match.arg(mode)

  if (identical(mode, "student")) {
    return(list(
      mode = "student",
      title_size = 22,
      axis_title_size = 16,
      tick_size = 13,
      legend_size = 13,
      line_width = 3.5,
      marker_size = 8,
      highlight_size = 13,
      gamma_size = 13,
      phi_size = 17,
      boundary_line_width = 2,
      plot_margin = 70
    ))
  }

  list(
    mode = "research",
    title_size = 18,
    axis_title_size = 14,
    tick_size = 11,
    legend_size = 11,
    line_width = 2.5,
    marker_size = 6,
    highlight_size = 11,
    gamma_size = 10,
    phi_size = 14,
    boundary_line_width = 1.5,
    plot_margin = 60
  )
}

#' Plotly axis specification
#'
#' @param title Axis title.
#' @param visual_spec Result of te_visual_spec().
#' @param range Optional numeric range.
#' @return list suitable for plotly layout.
#' @export
te_plotly_axis_spec <- function(title, visual_spec, range = NULL) {
  out <- list(
    title = list(
      text = title,
      font = list(size = visual_spec$axis_title_size)
    ),
    tickfont = list(size = visual_spec$tick_size),
    zeroline = TRUE,
    automargin = TRUE
  )
  if (!is.null(range)) out$range <- range
  out
}
