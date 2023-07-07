ui <-tagList(
  tags$head(
    epishiny::use_epishiny()
  ),

  page_navbar(
    title = app_title,
    window_title = app_title,
    id = "tabs",
    inverse = FALSE,
    collapsible = TRUE,

    theme = bs_theme(
      version = 5,
      base_font = font_google(
        app_font,
        wght = c(300, 400, 500, 600, 700, 800),
        ital = c(0, 1)
      ),
      font_scale = 0.8
    ),

    # nav pages
    nav_panel(
      tags$span(shiny::icon("chart-column"), "Demo"),
      layout_sidebar(
        # sidebar
        sidebar = filterUI(
          "filter",
          date_range = date_range,
          date_label = "Notification period"
          # group_vars
        ),
        # main content
        layout_columns(
          col_widths = breakpoints(
            md = c(12, 12, 12),
            lg = c(12, 7, 5),
            xl = c(12, 7, 5)
          ),

          mapUI(
            id = "map",
            geo_data = geo_data,
            group_vars = group_vars
          ),

          epicurveUI(
            id = "curve",
            title = "Time",
            date_vars = date_vars,
            group_vars = group_vars
          ),

          pyramidUI(id = "age_sex")
        )
      )
    ),

    nav_panel(
      tags$span(shiny::icon("info"), "About"),
      tags$div(
        class = "container"
      )
    ),

    # nav images and links
    nav_spacer(),
    nav_item(
      tags$img(
        src = "epishiny/img/epicentre_logo.png",
        height = "40px"
      )
    )
  ),

  waiter::waiter_preloader(html = waiter::spin_3())
)
