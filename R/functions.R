#' @title Plot TOST Equivalence Test
#' @description Plot the TOST equivalence test with the null hypothesis.
#' @param tost An object of class `TOSTt`.
#' @param save_file A character string specifying the file name to save the plot. If `NULL` (default), the plot is not saved.
#' @param ... Additional arguments passed to `ggsave` (e.g., width & height)
#' @return A ggplot object
plot_equ_tnull <- function(tost, save_file = NULL, font = "Open Sans", ...) {
  sysfonts::font_add_google("Inter", "font")
  showtext::showtext_opts(dpi = 500)
  showtext::showtext_auto()

  data_eqb <- tibble::tibble(
    type = c("lower", "upper"),
    bounds = c(tost$eqb$low_eq[1], tost$eqb$high_eq[1]),
    df = c(tost$TOST$df[2], tost$TOST$df[3]),
    se = c(tost$TOST$SE[2], tost$TOST$SE[3]),
    alpha = tost$alpha,
    crit_lower = bounds - stats::qnorm(1 - alpha) * se, 
    crit_upper = bounds + stats::qnorm(1 - alpha) * se
  )

  data_est <- tibble::tibble(
    estimate = tost$effsize[1, "estimate"],
    lb = tost$effsize[1, "lower.ci"],
    ub = tost$effsize[1, "upper.ci"]
  )
  
  buffer <- 0.075
  data_text <- tibble::tibble(
    type = c("lower", "upper"),
    bounds = c(tost$eqb$low_eq[1], tost$eqb$high_eq[1]),
    x = dplyr::if_else(type == "lower", bounds + buffer, bounds - buffer),
    label = dplyr::if_else(type == "lower", paste0("&Delta;<sub>L</sub> = ", round(bounds, 2)), paste0("&Delta;<sub>U</sub> = ", round(bounds, 2)))
  ) 
  
  dist_lower <- with(data_eqb[1, ], distributional::dist_student_t(df = df, mu = bounds, sigma = se))
  dist_upper <- with(data_eqb[2, ], distributional::dist_student_t(df = df, mu = bounds, sigma = se))
  
  ggplot2::ggplot(data_eqb, ggplot2::aes(y = 0)) +
    ggdist::stat_slab(
      ggplot2::aes(dist = dist_lower, fill = ggplot2::after_stat(level)),
      alpha = .6,
      color = NA,
      slab_size = 0.5,
      .width = c(.9, 1)
    ) +
    ggdist::stat_slab(
      ggplot2::aes(dist = dist_upper, fill = ggplot2::after_stat(level)),
      alpha = .6,
      color = NA,
      slab_size = 0.5,
      .width = c(.9, 1)
    ) +
    ggplot2::geom_pointrange(
      mapping = ggplot2::aes(x = estimate, xmin = lb, xmax = ub),
      data = data_est,
      linewidth = 1,
      size = 0.3,
      position = ggplot2::position_nudge(y = -0.05)
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "grey80", size = 0.4) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = bounds), linetype = "longdash", linewidth = 0.35) +
    ggtext::geom_richtext(
      data = data_text,
      mapping = ggplot2::aes(y = 1, x = x, label = label), 
      color = NA,
      size = 3,
      text.colour = "black",
      fill = NA,
      family = "font",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(values = c("#051088", "#f7ce4c")) +
    ggplot2::guides(fill = "none") +
    ggplot2::scale_y_continuous(
      name = NULL,
      breaks = NULL,
      limits = c(-0.1, 1.25),
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-.3, .3),
      breaks = seq(-.3, .3, .1),
      labels = scales::label_number(),
      expand = c(0, 0),
      name = "Mean Difference"
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    sjPlot::theme_sjplot() +
    ggplot2::theme(
      plot.margin  = ggplot2::margin(t = 0, 1, 1, 1, unit = "lines"),
      panel.background = ggplot2::element_rect(color = "grey", linewidth = 0.5),
      axis.title = element_text(size = 10),
      text = ggplot2::element_text(family = "font"),
      panel.grid.major = ggplot2::element_line(linewidth = 0.2),
      panel.ontop = FALSE
    ) -> p
  if (!is.null(save_file)) ggplot2::ggsave(save_file, p, dpi = 500, ...)
  return(p)
}
  
#' @title APA-7 formatted Latex Table
#' @description Create a nice looking table for APA-7 formatted Latex
#' @param x A data frame
#' @param digits Number of digits to round to (default = 3)
#' @param caption The table caption
#' @param footnote The table footnote
#' @param align The alignment of the columns (if NULL, the first column is left aligned and the rest are centered)
#' @param col_names The column names
#' @param font_size The font size (default = 12)
#' @return A character vector of the table source code
#' 
nice_table <- function(x, digits = 3, caption, footnote, align = NULL, col_names = NULL, font_size = 12) {
  n_col <- ncol(x)
  center <- paste0(rep("c", n_col - 1), collapse = "")
  if (is.null(align)) align <- paste0("l", center, collape = "")
  if (is.null(col_names)) col_names <- colnames(x)
  knitr::kable(
    x = x, format = "latex", 
    escape = FALSE, booktabs = TRUE,
    align = align, caption = caption, 
    digits = digits, row.names = FALSE, col.names = col_names
  ) |> 
    kableExtra::kable_styling(full_width = TRUE, font_size = font_size) |> 
    kableExtra::footnote(footnote, footnote_as_chunk = TRUE, escape = FALSE, fixed_small_size = TRUE)
}


#' @title Smithson-Verkuilen Transformation for Beta-Regression
#' @param x A numeric vector
#' @description
#' The transformation proposed in Smithson and Verkuilen (2006) is a simple transformation that can be used to transform the response variable in a beta regression model. 
#' The transformation is defined as: 
#' \deqn{y' = \frac{y \times (n - 1) + 0.5}{n}}
#' where \deqn{y} is the response variable and \deqn{n} is the sample size.
#' @return A numeric vector of transformed values
#' @references Smithson, M., & Verkuilen, J. (2006). A better lemon squeezer? Maximum-likelihood regression with beta-distributed dependent variables. Psychological Methods, 11(1), 54-71.
#' 
smithson_verkuilen <- function(x) (x * (length(x) - 1) + 0.5)/length(x)


#' @title APA-7 formatted p-value
#' @description Format a p-value for APA-7
#' @param x A numeric value
#' @param digits Number of digits to round to (default = 3)
#' @param accuracy The accuracy of the p-value (default = 0.001)
#' @return A character vector of the formatted p-value
#' 
pformat <- function(x, stars = FALSE) {
  insight::format_p(x, stars = stars, name = NULL, digits = 3)
}

#' @title Read Latex-File
#' @description Read a Latex file and print it to the console
#' @param file The file path
#' @return The content of the file
#' 
read_tex <- function(file) {
  x <- readLines(file, warn = FALSE)
  cat(x, sep = "\n")
}

#' @title Extract Parameter Data from DE-Optimization
#' @param x A list of the raw optimization data.
#' 
extract_optim <- function(x) {
  best_member <- dplyr::bind_rows(map(x, ~ as.data.frame(t(purrr::pluck(.x, "optim_results", "optim", "bestmem")))), .id = "id_meta")
  colnames(best_member) <- c("id_meta", "phi_n", "mu_n", "mu_d", "sigma2_d", "w_pbs")
  other <- dplyr::bind_rows(purrr::map(x, ~magrittr::extract(purrr::pluck(.x, "optim_results", "optim"), -1)), .id = "id_meta")
  runtime <- dplyr::bind_rows(purrr::map(x, ~ data.frame(runtime = t(purrr::pluck(.x, "runtime")))), .id = "id_meta")
  out <- tibble::as_tibble(purrr::reduce(list(best_member, other, runtime), inner_join, by = "id_meta"))
  return(out)
}

#' @title Extract Evolution Data from DE-Optimization
#' @param x A list of the raw optimization data
#' 
extract_evolution <- function(x) {
  out <- purrr::map(x, ~ {
    purrr::pluck(.x, "optim_results", "member", "storepop") |>
      tibble::enframe(name = "id_iter", value = "data") |>
      dplyr::mutate(data = purrr::map(data, as.data.frame)) |>
      dplyr::mutate(data = purrr::map(data, ~ dplyr::rename_with(.x, ~ c("phi_n", "mu_n", "mu_d", "sigma2_d", "w_pbs"))))
  }) |>
    dplyr::bind_rows(.id = "id_meta") |>
    tidyr::unnest(data)
  return(out)
}

#' Extract Loss Data from DE-Optimization
#' @param x A list of the raw optimization data.
#' 
extract_loss <- function(x) {
  purrr::map(x, ~ {
    loss <- purrr::pluck(.x, "optim_results", "member", "bestvalit")
    id_iter <- seq_along(loss)
    tibble::tibble(id_iter, loss)
  }) |>
    dplyr::bind_rows(.id = "id_meta")
}

#' @title ML-Estimation of Parameters of Negative-Binomial Distribution
#' @start Starting parameters of ML-Estimation.
#' @param data A numeric vector of sample sizes values from the meta-analysis data.
#' 
estimate_nb <- function(start, data) {
  ll <- function(x, data) -sum(stats::dnbinom(data, size = x[1], mu = x[2], log = TRUE))
  stats::optim(start, ll, data = data, method = "BFGS")
}

#' @title ML-Estimation of parameters of Normal Distribution
#' @start Starting parameters of ML-Estimation.
#' @param data A numeric vector of effect size values from the meta-analysis data.
estimate_norm <- function(start, data) {
  ll <- function(x, data) -sum(stats::dnorm(data, mean = x[1], sd = sqrt(x[2]), log = TRUE))
  stats::optim(start, ll, data = data, method = "BFGS")
}

#' @title Report Fit Statistics of Regression Model as String
#' @x Beta-regression Model
#' @yvar Name of dependent variable
report_fit <- function(x, yvar) {
  g <- glance(x) |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(.x, 2)))
  g$mae <-  round(mean(abs(x$model[[yvar]] - predict(x))), 2)
  glue::glue("$LL$ = {{g$logLik}}, $MAE$ = {{g$mae}}, $AIC$ = {{g$AIC}}, $BIC$ = {{g$BIC}}", .open = "{{", .close = "}}")
}