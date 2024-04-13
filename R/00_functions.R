#' Build Dockerfile for Project
#'
#' @param path Path in which the Dockerfile should be created (defaults to root directory of the R-project)
#' @param FROM Base image for the Dockerfile
#' @param lock_file Path to the renv.lock file
#' @param quarto_version Version of Quarto to install
#'
#' @return A Dockerfile in the specified path
#' 
build_dockerfile <- function(path = ".", FROM = "eddelbuettel/r2u:20.04", lock_file = "renv.lock", quarto_version = "0.9.522") {
  # Base Image
  from_statement <- glue::glue("FROM {FROM} AS base")
  # System Requirements
  cli::cli_process_start(msg = "Getting system requirements from {.file ./{lock_file}} file",)
  sysreqs <- glue::glue_collapse(sort(unique(c(getsysreqs::get_sysreqs(lock_file), "curl", "gdebi-core", "snakemake"))), sep = " \\\n    ")
  sysreqs <- glue::glue(
    "RUN apt-get update -qq && \\ \n",
    "  apt-get install -y --no-install-recommends \\\n    ",
    sysreqs,
    "\ && \\\n",
    "  apt-get clean && \\ \n",
    "  rm -rf /var/lib/apt/lists/*",
    .trim = FALSE
  )
  cli::cli_process_done()
  
  # Create and copy folder into analysis/
  cc_analysis <- glue::glue_collapse(
    c(
      "WORKDIR /project",
      "COPY . /project",
      "RUN mkdir renv/.cache",
      "ENV RENV_PATHS_CACHE renv/.cache"
    ),
    sep = "\n"
  )
  
  # Initialize renv package management
  renv_initialize <- glue::glue_collapse(
    c(
      "RUN R -e 'install.packages(\"remotes\")'",
      "RUN R -e 'remotes::install_version(\"renv\", version = \"1.0.3\")'",
      "RUN R -e \'renv::restore()\'"
    ),
    sep = "\n"
  )
  
  # Install Quarto
  download_quarto <- glue::glue_collapse(
    c(
      glue::glue("RUN curl -o quarto-linux-amd64.deb -L https://github.com/quarto-dev/quarto-cli/releases/download/v{quarto_version}/quarto-{quarto_version}-linux-amd64.deb"),
      "RUN gdebi --non-interactive quarto-linux-amd64.deb"
    ),
    sep = "\n"
  )
  
  # Install TinyTex
  install_tinytex <- glue::glue("RUN quarto tools install tinytex")
  
  # Second stage
  second_stage <- glue::glue_collapse(
    c(
      "FROM base",
      "WORKDIR /project",
      "COPY --from=base /project ."
    ),
    sep = "\n"
  )
  
  # CMD
  cmd_build <- glue::glue("CMD [\"./build.sh\"]")
  
  ret <-  purrr::compact(list(
    from_statement,
    sysreqs,
    cc_analysis,
    renv_initialize,
    download_quarto,
    install_tinytex,
    second_stage,
    cmd_build
  ))
  
  readr::write_lines(ret, file = file.path(path, "Dockerfile"))
  cli::cli_alert_success("Dockerfile created successfully! 🐳")
  cli::cli_alert_info("Dockerfile available at: {.file {file.path(path, 'Dockerfile')}}")
  cli::cli_alert_info("For more information see {.file ./README.md}")
}

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
  
  emp_data <- tost$effsize["Raw", ]
  eqb_lb <- subset(tost$eqb, subset = type == "Raw")$low_eq
  eqb_ub <- subset(tost$eqb, subset = type == "Raw")$high_eq
  df <- unique(tost$TOST$df)
  dist <- distributional::dist_student_t(df = df, mu = c(eqb_lb, eqb_ub), sigma = emp_data$SE) 
  buffer <- 0.075
  data_text <- tibble::tibble(
    type = c("lower", "upper"),
    bounds = c(tost$eqb$low_eq[1], tost$eqb$high_eq[1]),
    x = dplyr::if_else(type == "lower", bounds + buffer, bounds - buffer),
    label = dplyr::if_else(type == "lower", paste0("&Delta;<sub>L</sub> = ", round(bounds, 2)), paste0("&Delta;<sub>U</sub> = ", round(bounds, 2)))
  ) 
  
  ggplot2::ggplot(NULL, ggplot2::aes(y = 0, dist = dist)) +
    ggdist::stat_slab(
      ggplot2::aes(fill = ggplot2::after_stat(level)),
      alpha = .8,
      color = NA,
      slab_size = 0.5,
      .width = c(.9, 1)
    ) +
    ggplot2::geom_pointrange(
      mapping = ggplot2::aes(x = estimate, xmin = lower.ci, xmax = upper.ci, y = 0),
      data = emp_data,
      linewidth = 1,
      size = 0.3,
      position = ggplot2::position_nudge(y = -0.05),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "grey80", linewidth = 0.4) +
    ggplot2::geom_vline(data = data_text, ggplot2::aes(xintercept = bounds), linetype = "longdash", linewidth = 0.35) +
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
    ggplot2::scale_fill_manual(values = (c("#051088", "#f7ce4c"))) +
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
nice_table <- function(x, digits = 3, caption, footnote = NULL, align = NULL, col_names = NULL, font_size = 12, full_width = TRUE) {
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
    kableExtra::kable_styling(
      full_width = full_width, font_size = font_size, 
      latex_options = c('HOLD_position', "scale_down")
      ) |> 
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
  invisible(x)
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
  g$r2 <- round(unname(unlist(performance::r2(x))), 3)
  glue::glue("$LL$ = {{g$logLik}}, $MAE$ = {{g$mae}}, $AIC$ = {{g$AIC}}, $BIC$ = {{g$BIC}}, $R^2$ = {{g$r2}}", .open = "{{", .close = "}}")
}

center_text <- function(text) paste0("\\multirow{1}{*}[0pt]{", text, "}")

theme_comparison <- function(...) {
  theme_sjplot() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(vjust = 1, size = 10),
      legend.text = element_text(size = 8),
      legend.box.margin = margin(),
      legend.margin = margin(),
      text = element_text(family = "font"),
      plot.margin = margin(),
      legend.key.size = unit(0.6, "cm"),
      ...
    )
}

#' Calculate R2 statistic from Ferrari & Cribari-Neto (2004)
#' @x Object of class betareg
#' @dependent Character string with name of dependent variable
#' @return A numeric scalar
r2_fcn <- function(x, dependent) cor(qlogis(x$model[[dependent]]), predict(x, type = "link"))**2


