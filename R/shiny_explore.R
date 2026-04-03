#' @noRd
shiny_explore_prevalence_results <- function(results_object) {
  # Prepare aggregated data for each result type
  prep_data <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }

    data |>
      dplyr::group_by(.data$analysisId, .data$cohortName, .data$spanLabel) |>
      dplyr::summarise(
        numerator = sum(.data$numerator, na.rm = TRUE),
        denominator = sum(.data$denominator, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        stat = dplyr::if_else(
          .data$denominator > 0,
          (.data$numerator / .data$denominator) * 100000,
          NA_real_
        ),
        analysisId = as.character(.data$analysisId)
      )
  }

  # Prepare all data
  prev_data <- prep_data(results_object$prevalence)
  inc_data <- prep_data(results_object$incidence)
  drug_data <- prep_data(results_object$drugUsage)
  meta_data <- results_object$metaInfo

  # Create analysis metadata lookup (for hover text)
  if (!is.null(meta_data) && nrow(meta_data) > 0) {
    meta_lookup <- meta_data |>
      dplyr::select(
        .data$analysisId,
        dplyr::everything()
      ) |>
      dplyr::mutate(analysisId = as.character(.data$analysisId))
  } else {
    meta_lookup <- NULL
  }

  # Get unique analyses for multi-select
  all_analyses <- c(
    if (!is.null(prev_data)) unique(prev_data$analysisId),
    if (!is.null(inc_data)) unique(inc_data$analysisId),
    if (!is.null(drug_data)) unique(drug_data$analysisId)
  ) |>
    unique() |>
    sort()

  default_analysis <- if (length(all_analyses) > 0) all_analyses[1] else NULL

  # UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Prevalence Analysis Results Explorer"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4("Filters"),
        shiny::selectInput(
          "selected_analyses",
          "Select Analyses to Compare:",
          choices = all_analyses,
          selected = default_analysis,
          multiple = TRUE
        ),
        width = 3
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          id = "result_tabs",
          if (!is.null(prev_data)) {
            shiny::tabPanel(
              "Prevalence",
              shiny::tabsetPanel(
                shiny::tabPanel(
                  "Table",
                  reactable::reactableOutput("prev_table")
                ),
                shiny::tabPanel(
                  "Plot",
                  plotly::plotlyOutput("prev_plot", height = "600px")
                )
              )
            )
          },
          if (!is.null(inc_data)) {
            shiny::tabPanel(
              "Incidence",
              shiny::tabsetPanel(
                shiny::tabPanel(
                  "Table",
                  reactable::reactableOutput("inc_table")
                ),
                shiny::tabPanel(
                  "Plot",
                  plotly::plotlyOutput("inc_plot", height = "600px")
                )
              )
            )
          },
          if (!is.null(drug_data)) {
            shiny::tabPanel(
              "Drug Usage",
              shiny::tabsetPanel(
                shiny::tabPanel(
                  "Table",
                  reactable::reactableOutput("drug_table")
                ),
                shiny::tabPanel(
                  "Plot",
                  plotly::plotlyOutput("drug_plot", height = "600px")
                )
              )
            )
          }
        ),
        width = 9
      )
    )
  )

  # Server
  server <- function(input, output, session) {
    # Reactive filter function
    filter_by_analysis <- function(data) {
      if (is.null(data) || length(input$selected_analyses) == 0) {
        return(NULL)
      }
      data |>
        dplyr::filter(.data$analysisId %in% input$selected_analyses)
    }

    # Format for display in tables
    format_table_data <- function(data) {
      if (is.null(data)) return(NULL)

      data |>
        dplyr::select(
          .data$analysisId,
          .data$cohort,
          .data$spanLabel,
          .data$numerator,
          .data$denominator,
          .data$stat
        ) |>
        dplyr::mutate(stat = round(.data$stat, 2))
    }

    # Prevalence outputs
    if (!is.null(prev_data)) {
      prev_filtered <- shiny::reactive({
        filter_by_analysis(prev_data)
      })

      output$prev_table <- reactable::renderReactable({
        data <- format_table_data(prev_filtered())
        if (is.null(data) || nrow(data) == 0) {
          return(reactable::reactable(data.frame(message = "No data selected")))
        }

        reactable::reactable(
          data,
          columns = list(
            analysisId = reactable::colDef(name = "Analysis"),
            cohort = reactable::colDef(name = "Cohort"),
            spanLabel = reactable::colDef(name = "Period"),
            numerator = reactable::colDef(name = "Numerator", format = reactable::colFormat(separators = TRUE)),
            denominator = reactable::colDef(name = "Denominator", format = reactable::colFormat(separators = TRUE)),
            stat = reactable::colDef(name = "Prevalence (per 100k)")
          ),
          sortable = TRUE,
          filterable = TRUE,
          defaultPageSize = 10
        )
      })

      output$prev_plot <- plotly::renderPlotly({
        data <- prev_filtered()
        if (is.null(data) || nrow(data) == 0) {
          return(plotly::plot_ly() |>
            plotly::add_annotations(
              text = "No data selected",
              showarrow = FALSE,
              font = list(size = 16)
            ))
        }

        # Create hover text with metaInfo
        data <- data |>
          dplyr::mutate(
            hover_text = glue::glue(
              "Analysis: {analysisId}<br>Cohort: {cohort}<br>Period: {spanLabel}<br>Prevalence: {round(stat, 2)}/100k"
            )
          )

        plotly::plot_ly(
          data = data,
          x = ~spanLabel,
          y = ~stat,
          color = ~analysisId,
          type = "scatter",
          mode = "lines+markers",
          hovertext = ~hover_text,
          hoverinfo = "text"
        ) |>
          plotly::layout(
            title = "Prevalence Trends",
            xaxis = list(title = "Period"),
            yaxis = list(title = "Prevalence (per 100,000)"),
            hovermode = "closest",
            plot_bgcolor = "#f5f5f5",
            legend = list(title = list(text = "Analysis"))
          )
      })
    }

    # Incidence outputs
    if (!is.null(inc_data)) {
      inc_filtered <- shiny::reactive({
        filter_by_analysis(inc_data)
      })

      output$inc_table <- reactable::renderReactable({
        data <- format_table_data(inc_filtered())
        if (is.null(data) || nrow(data) == 0) {
          return(reactable::reactable(data.frame(message = "No data selected")))
        }

        reactable::reactable(
          data,
          columns = list(
            analysisId = reactable::colDef(name = "Analysis"),
            cohort = reactable::colDef(name = "Cohort"),
            spanLabel = reactable::colDef(name = "Period"),
            numerator = reactable::colDef(name = "Numerator", format = reactable::colFormat(separators = TRUE)),
            denominator = reactable::colDef(name = "Denominator", format = reactable::colFormat(separators = TRUE)),
            stat = reactable::colDef(name = "Incidence (per 100k)")
          ),
          sortable = TRUE,
          filterable = TRUE,
          defaultPageSize = 10
        )
      })

      output$inc_plot <- plotly::renderPlotly({
        data <- inc_filtered()
        if (is.null(data) || nrow(data) == 0) {
          return(plotly::plot_ly() |>
            plotly::add_annotations(
              text = "No data selected",
              showarrow = FALSE,
              font = list(size = 16)
            ))
        }

        data <- data |>
          dplyr::mutate(
            hover_text = glue::glue(
              "Analysis: {analysisId}<br>Cohort: {cohort}<br>Period: {spanLabel}<br>Incidence: {round(stat, 2)}/100k"
            )
          )

        plotly::plot_ly(
          data = data,
          x = ~spanLabel,
          y = ~stat,
          color = ~analysisId,
          type = "scatter",
          mode = "lines+markers",
          hovertext = ~hover_text,
          hoverinfo = "text"
        ) |>
          plotly::layout(
            title = "Incidence Trends",
            xaxis = list(title = "Period"),
            yaxis = list(title = "Incidence (per 100,000)"),
            hovermode = "closest",
            plot_bgcolor = "#f5f5f5",
            legend = list(title = list(text = "Analysis"))
          )
      })
    }

    # Drug usage outputs
    if (!is.null(drug_data)) {
      drug_filtered <- shiny::reactive({
        filter_by_analysis(drug_data)
      })

      output$drug_table <- reactable::renderReactable({
        data <- format_table_data(drug_filtered())
        if (is.null(data) || nrow(data) == 0) {
          return(reactable::reactable(data.frame(message = "No data selected")))
        }

        reactable::reactable(
          data,
          columns = list(
            analysisId = reactable::colDef(name = "Analysis"),
            cohort = reactable::colDef(name = "Cohort"),
            spanLabel = reactable::colDef(name = "Period"),
            numerator = reactable::colDef(name = "Numerator", format = reactable::colFormat(separators = TRUE)),
            denominator = reactable::colDef(name = "Denominator", format = reactable::colFormat(separators = TRUE)),
            stat = reactable::colDef(name = "Drug Usage (per 100k)")
          ),
          sortable = TRUE,
          filterable = TRUE,
          defaultPageSize = 10
        )
      })

      output$drug_plot <- plotly::renderPlotly({
        data <- drug_filtered()
        if (is.null(data) || nrow(data) == 0) {
          return(plotly::plot_ly() |>
            plotly::add_annotations(
              text = "No data selected",
              showarrow = FALSE,
              font = list(size = 16)
            ))
        }

        data <- data |>
          dplyr::mutate(
            hover_text = glue::glue(
              "Analysis: {analysisId}<br>Cohort: {cohort}<br>Period: {spanLabel}<br>Drug Usage: {round(stat, 2)}/100k"
            )
          )

        plotly::plot_ly(
          data = data,
          x = ~spanLabel,
          y = ~stat,
          color = ~analysisId,
          type = "scatter",
          mode = "lines+markers",
          hovertext = ~hover_text,
          hoverinfo = "text"
        ) |>
          plotly::layout(
            title = "Drug Usage Trends",
            xaxis = list(title = "Period"),
            yaxis = list(title = "Drug Usage (per 100,000)"),
            hovermode = "closest",
            plot_bgcolor = "#f5f5f5",
            legend = list(title = list(text = "Analysis"))
          )
      })
    }
  }

  # Launch app
  shiny::shinyApp(ui = ui, server = server)
}
