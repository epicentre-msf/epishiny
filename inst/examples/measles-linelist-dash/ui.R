ui <- tagList(
  page_navbar(
    title = app_title,
    window_title = app_title,
    id = "tabs",
    inverse = FALSE,
    collapsible = TRUE,

    theme = bs_theme(
      preset = "minty",
      font_scale = .8,
      primary = "#4682B4",
      secondary = "#D37331",
      success = "#94BA3B"
    ),

    # nav pages
    nav_panel(
      class = "bslib-page-dashboard",
      tags$span(shiny::icon("chart-column"), "Demo"),
      layout_sidebar(
        # sidebar
        sidebar = filter_ui(
          "filter",
          group_vars = group_vars,
          # date_range = date_range,
          period_lab = "Notification period"
        ),
        # main content
        layout_column_wrap(
          width = 1 / 2,
          place_ui(
            id = "map",
            tooltip = "Click on a polygon to filter other graphics to this region",
            geo_data = geo_data,
            group_vars = group_vars
          ),
          layout_column_wrap(
            width = 1,
            time_ui(
              id = "curve",
              tooltip = "Click on a bar to filter other graphics to this period",
              date_vars = date_vars,
              group_vars = group_vars,
              ratio_line_lab = "Show CFR line?"
            ),
            person_ui(id = "age_sex")
          )
        )
      )
    ),

    nav_item(
      tags$a(
        tags$span(shiny::icon("info"), "About"),
        href = "https://epicentre-msf.github.io/epishiny/",
        target = "_blank"
      )
    ),

    # nav images and links
    nav_spacer(),
    nav_item(
      tags$a(
        tags$img(
          src = "epishiny/img/epicentre_logo.png",
          alt = "Epicentre Logo",
          height = "35px"
        ),
        class = "py-0 d-none d-lg-block",
        title = "Epicentre",
        href = "https://epicentre.msf.org/en",
        target = "_blank"
      )
    )
  ),
  # start up loading spinner
  waiter::waiter_preloader(
    html = tagList(
      tags$img(
        src = "epishiny/img/epicentre_logo.png",
        width = 600,
        style = "padding: 20px;"
      ),
      tags$br(),
      waiter::spin_3()
    ),
    color = "#FFFFFF"
  )
)
