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