transmission_ui <- function(
  id,
  title = "Transmission chain",
  icon = bsicons::bs_icon("diagram-3-fill"),
  opts_btn_lab = "options",
  count_vars_lab = "Indicator",
  full_screen = TRUE
) {
  ns <- shiny::NS(id)

  # check deps are installed
  pkg_deps <- c("epicontacts")
  if (!rlang::is_installed(pkg_deps)) {
    rlang::check_installed(
      pkg_deps,
      reason = "to use the epishiny transmission module."
    )
  }

  bslib::navset_card_tab(
    wrapper = function(...) {
      bslib::card_body(..., padding = 0, class = "person-container")
    },
    full_screen = full_screen,
    id = ns("tabs"),

    title = tags$div(
      class = "d-flex justify-content-start align-items-center",
      tags$span(icon, title, class = "pe-2"),
      bslib::popover(
        trigger = actionButton(
          ns("dropdown"),
          icon = shiny::icon("sliders"),
          label = opts_btn_lab,
          class = "btn-sm btn-light"
        ),
        title = "Visualization Options",
        # General network visualization options
        # numericInput(
        #   ns("node_size"),
        #   "Node Size",
        #   value = 15,
        #   min = 1,
        #   step = 1
        # ),
        selectInput(
          ns("node_color"),
          "Node Color Variable",
          choices = NULL,
          multiple = FALSE
        ),
        # numericInput(
        #   ns("edge_width"),
        #   "Edge Width",
        #   value = 2.5,
        #   min = 0.1,
        #   step = 0.1
        # ),
        selectInput(
          ns("edge_color"),
          "Edge Color Variable",
          choices = NULL,
          multiple = FALSE
        ),
        # numericInput(
        #   ns("arrow_size"),
        #   "Arrow Size",
        #   value = 0.5,
        #   min = 0.1,
        #   max = 2,
        #   step = 0.1
        # ),
        hr(),
        selectInput(
          ns("x_axis"),
          "X-Axis Variable",
          choices = NULL,
          multiple = FALSE
        ),
        conditionalPanel(
          condition = "input.x_axis != 'none'",
          ns = ns,
          # Timeline view toggle
          checkboxInput(
            ns("timeline_view"),
            "Timeline View",
            value = FALSE
          ),

          # Conditional inputs for timeline view
          conditionalPanel(
            condition = "input.timeline_view == true",
            ns = ns,

            selectInput(
              ns("tl_start"),
              "Timeline Start Column",
              choices = NULL
            ),
            selectInput(
              ns("tl_end"),
              "Timeline End Column",
              choices = NULL
            ),
            # numericInput(
            #   ns("tl_end_node_size"),
            #   "Timeline End Node Size",
            #   value = 12,
            #   min = 1,
            #   step = 1
            # ),
            selectInput(
              ns("tl_edge_color"),
              "Timeline Edge Color",
              choices = NULL,
              multiple = FALSE
            ),
            selectInput(
              ns("tl_edge_label"),
              "Timeline Edge Label",
              choices = NULL,
              multiple = FALSE
            )
          ),
          selectInput(
            ns("network_shape"),
            "Network Shape",
            choices = c("rectangle", "branching"),
            selected = "rectangle"
          ),
          numericInput(
            ns("n_date_break"),
            "Number of Date Breaks",
            value = 10,
            min = 1,
            step = 1
          ),
          # textInput(
          #   ns("date_labels"),
          #   "Date Format",
          #   value = "%d\n%B"
          # )
        ),
        # hr(),
        # # Additional visualization options
        # checkboxInput(
        #   ns("label"),
        #   "Show Labels",
        #   value = FALSE
        # ),
        # numericInput(
        #   ns("font_size"),
        #   "Font Size",
        #   value = 15,
        #   min = 8,
        #   step = 1
        # ),
        # numericInput(
        #   ns("height"),
        #   "Plot Height (px)",
        #   value = 1500,
        #   min = 300,
        #   step = 100
        # ),
        # numericInput(
        #   ns("width"),
        #   "Plot Width (px)",
        #   value = 1200,
        #   min = 300,
        #   step = 100
        # )
      )
    ),

    bslib::nav_panel(
      title = shiny::icon("chart-bar"),
      visNetwork::visNetworkOutput(ns("transmission_chain"))
    ),
  )
}


transmission_server <- function(
  id,
  epi_obj,
  id_var
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # On initialization, populate the dropdowns with column names from epi_obj
      observe({
        req(epi_obj)

        # Get column names from linelist
        linelist_cols <- names(epi_obj$linelist)
        contact_cols <- names(epi_obj$contacts)[
          !names(epi_obj$contacts) %in% c("to", "from")
        ]

        # Identify date columns
        date_cols <- sapply(epi_obj$linelist, function(col) {
          inherits(col, "Date") ||
            inherits(col, "POSIXt") ||
            inherits(col, "POSIXct") ||
            inherits(col, "POSIXlt")
        })

        # Identify columns that are non date, numeric, and integer
        cat_cols <- sapply(epi_obj$linelist, function(col) {
          is.factor(col) || is.character(col)
        })

        cat_contact_cols <- sapply(epi_obj$contacts, function(col) {
          is.factor(col) || is.character(col)
        })

        # Filter columns by type
        date_col_names <- linelist_cols[date_cols]
        cat_col_names <- linelist_cols[cat_cols]
        cat_contact_col_names <- contact_cols[cat_contact_cols]

        # Update select inputs for date/numeric fields
        updateSelectInput(
          session,
          "x_axis",
          choices = c("None" = "none", date_col_names)
        )
        updateSelectInput(
          session,
          "tl_start",
          choices = c(date_col_names)
        )
        updateSelectInput(
          session,
          "tl_end",
          choices = c(date_col_names)
        )

        # Update other select inputs with all columns
        updateSelectInput(
          session,
          "node_color",
          choices = c("None" = "none", cat_col_names)
        )
        updateSelectInput(
          session,
          "edge_color",
          choices = c("None" = "none", cat_contact_col_names)
        )
        updateSelectInput(
          session,
          "tl_edge_color",
          choices = c("None" = "none", cat_contact_col_names)
        )
        updateSelectInput(
          session,
          "tl_edge_label",
          choices = c("None" = "none", linelist_cols)
        )
      })

      # Define a reactive function to return NULL if the input is empty
      get_selected_value <- function(input_value) {
        if (input_value == "none") {
          return(NULL)
        }
        return(input_value)
      }

      # Use the function in a reactive expression
      x_axis <- reactive({
        get_selected_value(input$x_axis)
      })
      tl_start <- reactive({
        get_selected_value(input$tl_start)
      })
      tl_end <- reactive({
        get_selected_value(input$tl_end)
      })
      node_color <- reactive({
        get_selected_value(input$node_color)
      })
      edge_color <- reactive({
        get_selected_value(input$edge_color)
      })
      tl_edge_color <- reactive({
        get_selected_value(input$tl_edge_color)
      })
      tl_edge_label <- reactive({
        get_selected_value(input$tl_edge_label)
      })

      output$transmission_chain <- visNetwork::renderVisNetwork({
        plot_transmission_chains(
          epi_obj = epi_obj,
          x_axis = x_axis(),
          node_size = 15,
          node_color = node_color(),
          edge_width = 5,
          edge_color = edge_color(),
          arrow_size = .5,
          timeline_view = input$timeline_view,
          tl_start = tl_start(),
          tl_end = tl_end(),
          tl_edge_width = 5,
          tl_end_node_size = 5,
          tl_edge_color = tl_edge_color(),
          tl_edge_label = tl_edge_label(),
          network_shape = input$network_shape,
          label = label(),
          font_size = 20,
          n_date_break = input$n_date_break,
          date_labels = "%d\n%B",
          #height = input$height,
          #width = input$width,
          selector = id_var
        )
      })
    }
  )
}
