ui <-tagList(
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
        sidebar = filter_ui(
          "filter",
          date_range = date_range,
          period_lab = "Notification period"
        ),
        # main content
        layout_column_wrap(
          width = 1 / 2,
          place_ui(
            id = "map",
            geo_data = geo_data,
            group_vars = group_vars
          ),
          layout_column_wrap(
            width = 1,
            time_ui(
              id = "curve",
              title = "Time",
              date_vars = date_vars,
              group_vars = group_vars,
              ratio_line_lab = "Show CFR line?"
            ),
            person_ui(id = "age_sex")
          )
        )
        # layout_columns(
        #   col_widths = breakpoints(
        #     md = c(12, 12, 12),
        #     lg = c(12, 7, 5)
        #   ),

        #   place_ui(
        #     id = "map",
        #     geo_data = geo_data,
        #     group_vars = group_vars
        #   ),

        #   time_ui(
        #     id = "curve",
        #     title = "Time",
        #     date_vars = date_vars,
        #     group_vars = group_vars,
        #     ratio_line_lab = "Show CFR line?"
        #   ),

        #   person_ui(id = "age_sex")
        # )
      )
    ),

    nav_item(
      tags$a(
        tags$span(shiny::icon("info"), "About"),
        href = "https://github.com/epicentre-msf/epishiny",
        target = "_blank"
      )
    ),

    # nav_panel(
    #   tags$span(shiny::icon("info"), "About"),
    #   tags$div(
    #     class = "container"
    #   )
    # ),

    # nav images and links
    nav_spacer(),
    nav_item(
      tags$a(
        tags$img(
          src = "epishiny/img/epicentre_logo.png",
          alt = "Epicentre Logo",
          height = "40px"
        ),
        class = "py-0 d-none d-lg-block",
        title = "Epicentre",
        href = "https://epicentre.msf.org/",
        target = "_blank"
      )
    ),
  ),

  waiter::waiter_preloader(html = waiter::spin_3())
)