if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(
  # Data manipulation
  tidyverse,
  # Shiny
  bslib, shiny,
  # File Management
  here,
  # Visualization
  sjPlot, ggh4x, scales
)

data_confidence <- read_csv(here("data/optim/processed/data_optim_evolution_confidence.csv.gz"))
data_meta <- select(read_csv(here("data/meta/processed/data_lindenhonekopp_proc.csv")), id_meta, type_synthesis)
data_parameter <- read_csv(here("data/optim/processed/data_optim_merged.csv"))

ui <- page_navbar(
  title = "Parameter Covergence of SPEEC Approach",
  sidebar = sidebar(
    title = "Filter Datasets",
    selectInput(
      choices = unique(data_confidence$id_meta),
      inputId = "id_meta",
      label = "ID"
    ),
    selectInput(
      choices = c(
        "Meta-Analysis",
        "Multisite Replications",
        "All"
      ),
      inputId = "type_synthesis",
      label = "Type"
    ),
    actionButton(inputId = "random", label = "Random Draw")
  ),
  nav_panel("Information"),
  nav_panel(
    title = "Convergence Plots",
    column(12, card(
      card_header("Test"),
      fluidRow(plotOutput("plot", height = "700px", width = "70%"), class = "justify-content-center")
      ), 
      class = "justify-content-center")
      ),
  theme = bs_theme(preset = "shiny")
)

server <- function(input, output, session) {
  design <- c(
    "
  AABB
  CCDD
  #EE#
  "
  )

  data_confidence <- left_join(data_confidence, data_meta, by = "id_meta", multiple = "any") 

  # Draw random ID via action button (but within the bounds of type_synthesis)
  observeEvent(input$random, {
    updateSelectInput(session, "id_meta",
      choices = {
        if (input$type_synthesis == "All") {
          unique(data_confidence$id_meta)
        } else if (input$type_synthesis == "Meta-Analysis") {
          unique(data_confidence |> filter(type_synthesis == "ma") |> pull(id_meta))
        } else if (input$type_synthesis == "Multisite Replications") {
          unique(data_confidence |> filter(type_synthesis == "mr") |> pull(id_meta))
        }
      },
      selected = {
        if (input$type_synthesis == "All") {
          sample(unique(data_confidence$id_meta), 1)
        } else if (input$type_synthesis == "Meta-Analysis") {
          sample(unique(data_confidence |> filter(type_synthesis == "ma") |> pull(id_meta)), 1)
        } else if (input$type_synthesis == "Multisite Replications") {
          sample(unique(data_confidence |> filter(type_synthesis == "mr") |> pull(id_meta)), 1)
        }
      }
    )
  })

  observeEvent(input$type_synthesis, {
    if (input$type_synthesis == "All") {
      updateSelectInput(session, "id_meta", choices = unique(data_confidence |> pull(id_meta)))
    } else if (input$type_synthesis == "Meta-Analysis") {
      updateSelectInput(session, "id_meta", choices = unique(data_confidence |> filter(type_synthesis == "ma") |> pull(id_meta)))
    } else if (input$type_synthesis == "Multisite Replications") {
      updateSelectInput(session, "id_meta", choices = unique(data_confidence |> filter(type_synthesis == "mr") |> pull(id_meta)))
    }
  })

  output$plot <- renderPlot({
    data_confidence |>
      mutate(parameter = factor(parameter, levels = c("phi_n", "mu_n", "mu_d", "sigma2_d", "w_pbs"))) |>
      mutate(parameter = fct_recode(
        parameter,
        "widehat(mu)[d]" = "mu_d",
        "widehat(mu)[n]" = "mu_n",
        "widehat(phi)[n]" = "phi_n",
        "widehat(sigma)[d]^{2}" = "sigma2_d",
        "widehat(omega)[PBS]" = "w_pbs"
      )) |>
      filter(id_meta == input$id_meta) |>
      ggplot(aes(x = id_iter, y = mean)) +
      facet_manual(
        facets = vars(parameter),
        scales = "free",
        design = design,
        labeller = label_parsed
      ) +
      geom_ribbon(
        mapping = aes(ymin = lb, ymax = ub, fill = parameter),
        alpha = 0.3
      ) +
      geom_line(mapping = aes(color = parameter)) +
      guides(color = "none", fill = "none") +
      scale_x_continuous(
        name = "Iteration",
        limits = c(0, 1000),
        breaks = seq(0, 1000, 100),
      ) +
      facetted_pos_scales(
        y = list(
          "widehat(phi)[n]" = scale_y_continuous(trans = scales::transform_log2()),
          "widehat(mu)[n]" = scale_y_continuous(trans = scales::transform_log2())
        )
      ) +
      ylab(NULL) +
      coord_cartesian(clip = "off") +
      scale_color_sjplot(palette = "quadro") +
      scale_fill_sjplot(palette = "quadro") +
      ggdist::theme_ggdist() +
      theme(text = element_text(size = 16))
  })
}

shinyApp(ui = ui, server = server, options = list(port = 8080))