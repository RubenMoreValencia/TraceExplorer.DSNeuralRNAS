#' Transform article comparison loss for visualization
#'
#' The transformation is descriptive only. Original loss remains untouched.
#'
#' @param x te_article_case_set.
#' @param scale One of absolute, relative, or log_relative.
#' @return data.frame with original loss plus display_value/display_label.
#' @export
te_article_case_set_loss_view <- function(
    x,
    scale = c("absolute", "relative", "log_relative")) {

  scale <- match.arg(scale)

  d <- te_article_case_set_trace_data(x)

  if (!nrow(d)) {
    d$display_value <- numeric(0)
    d$display_label <- character(0)
    d$loss_scale <- character(0)
    return(d)
  }

  d$display_value <- NA_real_

  split_idx <- split(
    seq_len(nrow(d)),
    d$case_key
  )

  for (idx in split_idx) {
    l0 <- d$loss[idx[[1L]]]

    if (scale == "absolute") {
      d$display_value[idx] <- d$loss[idx]
    } else if (scale == "relative") {
      if (!is.finite(l0) || abs(l0) <= .Machine$double.eps) {
        d$display_value[idx] <- NA_real_
      } else {
        d$display_value[idx] <- d$loss[idx] / l0
      }
    } else {
      if (!is.finite(l0) || l0 <= 0) {
        d$display_value[idx] <- NA_real_
      } else {
        ratio <- d$loss[idx] / l0
        d$display_value[idx] <- ifelse(
          is.finite(ratio) & ratio > 0,
          log(ratio),
          NA_real_
        )
      }
    }
  }

  d$loss_scale <- scale
  d$display_label <- switch(
    scale,
    absolute = "P\u00e9rdida L_k",
    relative = "P\u00e9rdida relativa L_k / L_0",
    log_relative = "log(L_k / L_0)"
  )

  d
}

#' Boundary overlay for article comparison loss views
#'
#' Boundary locations are joined to the original trace by exact iteration.
#' No interpolation is used.
#'
#' @param x te_article_case_set.
#' @param scale One of absolute, relative, or log_relative.
#' @return data.frame of existing article boundary events with display values.
#' @export
te_article_case_set_boundary_overlay <- function(
    x,
    scale = c("absolute", "relative", "log_relative")) {

  scale <- match.arg(scale)

  tr <- te_article_case_set_loss_view(
    x,
    scale = scale
  )

  ev <- te_article_case_set_boundaries(x)

  if (!nrow(ev) || !nrow(tr)) {
    return(
      data.frame(
        case_key = character(),
        experiment = character(),
        stage = character(),
        case_id = character(),
        role = character(),
        window_id = numeric(),
        end_iter = numeric(),
        transition = character(),
        from_regime = character(),
        to_regime = character(),
        boundary_present = logical(),
        tau = numeric(),
        loss = numeric(),
        display_value = numeric(),
        loss_scale = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  key <- paste(
    tr$case_key,
    tr$iter,
    sep = "||"
  )
  ev_key <- paste(
    ev$case_key,
    ev$end_iter,
    sep = "||"
  )

  m <- match(ev_key, key)

  ev$tau <- tr$tau[m]
  ev$loss <- tr$loss[m]
  ev$display_value <- tr$display_value[m]
  ev$loss_scale <- scale

  ev
}

#' Descriptive scale summary for article comparison cases
#'
#' Reports final/initial loss ratio without ranking cases.
#'
#' @param x te_article_case_set.
#' @return data.frame.
#' @export
te_article_case_set_loss_summary <- function(x) {
  te_validate_article_case_set(x, error = TRUE)

  idx <- te_article_case_set_index(x)

  idx$final_initial_ratio <- ifelse(
    is.finite(idx$loss_initial) &
      abs(idx$loss_initial) > .Machine$double.eps,
    idx$loss_final / idx$loss_initial,
    NA_real_
  )

  idx$log_final_initial_ratio <- ifelse(
    is.finite(idx$final_initial_ratio) &
      idx$final_initial_ratio > 0,
    log(idx$final_initial_ratio),
    NA_real_
  )

  idx
}
