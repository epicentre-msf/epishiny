#' Place module
#'
#' Visualise geographical distribution across multiple administrative boundaries on an interactive leaflet map.
#'
#' @rdname place
#'
#' @param id Module id. Must be the same in both the UI and server function to link the two.
#' @param geo_data A list of named lists containing spatial sf dataframes and other information for different geographical levels.
#' @param count_vars If data is aggregated, variable name(s) of count variable(s) in data. If more than one is variable provided,
#'  a select input will appear in the options dropdown. If named, names are used as variable labels.
#' @param group_vars Character vector of categorical variable names. If provided, a select input will appear
#'  in the options dropdown allowing for data groups to be visualised on the map in pie charts per geographical unit.
#'  If named, names are used as variable labels.
#' @param title The title for the card.
#' @param icon The icon to be displayed next to the title
#' @param tooltip additional title hover text information
#' @param geo_lab The label for the geographical level selection.
#' @param count_vars_lab text label for the aggregate count variables input.
#' @param groups_lab The label for the group data by selection.
#' @param no_grouping_lab text label for the no grouping option in the grouping input.
#' @param circle_size_lab text label for the circle size slider input.
#' @param opts_btn_lab text label for the dropdown menu button.
#' @param download_lab text label for the download button.
#' @param full_screen Add button to card to with the option to enter full screen mode?
#'
#' @return A [bslib::card] UI element with options and download button and a leaflet map.
#' @export
#' @example inst/examples/docs/app.R
place_ui <- function(
  id,
  geo_data,
  count_vars = NULL,
  group_vars = NULL,
  title = "Place",
  icon = bsicons::bs_icon("geo-fill"),
  tooltip = NULL,
  geo_lab = "Geo boundaries",
  count_vars_lab = "Indicator",
  groups_lab = "Group data by",
  no_grouping_lab = "No grouping",
  circle_size_lab = "Circle size multiplyer",
  opts_btn_lab = "Map options",
  download_lab = "Download image of current map",
  full_screen = TRUE
) {
  ns <- shiny::NS(id)

  # check deps are installed
  pkg_deps <- c("sf", "leaflet", "leaflet.minicharts", "webshot2", "chromote")
  if (!rlang::is_installed(pkg_deps)) {
    rlang::check_installed(pkg_deps, reason = "to use the epishiny place module.")
  }

  if (!inherits(geo_data, "epishiny_geo_layer")) {
    if (!all(purrr::map_lgl(geo_data, ~ inherits(.x, "epishiny_geo_layer")))) {
      cli::cli_abort(c(
        "{.arg geo_data} must be an epishiny geo layer or a list of epishiny geo layers.",
        "i" = "see ?epishiny::geo_layer for details on how to setup your geo data."
      ))
    }
  }

  if (inherits(geo_data, "epishiny_geo_layer")) {
    geo_levels <- geo_data$layer_name
  } else {
    geo_levels <- purrr::map_chr(geo_data, "layer_name")
  }

  if (length(tooltip)) {
    tt <- bslib::tooltip(
      bsicons::bs_icon("info-circle", class = "ms-2 text-primary", size = "1.2em"),
      tooltip
    )
  } else {
    tt <- NULL
  }

  tagList(
    use_epishiny(),
    bslib::card(
      full_screen = full_screen,
      bslib::card_header(
        class = "d-flex align-items-center",
        # title
        tags$div(
          class = "d-flex align-items-center me-auto",
          tags$span(icon, title, class = "pe-2"),
          # Geo boundaries select ==========
          shinyWidgets::radioGroupButtons(
            ns("geo_level"),
            label = NULL, # geo_lab,
            choices = geo_levels,
            size = "sm",
            status = "outline-primary"
          )
        ),
        # options button and dropdown menu
        bslib::popover(
          title = opts_btn_lab,
          id = ns("popover"),
          placement = "left",
          trigger = bsicons::bs_icon(
            "gear",
            title = opts_btn_lab,
            class = "ms-2 text-primary",
            size = "1.2em"
          ),
          # # Geo boundaries select ==========
          # shinyWidgets::radioGroupButtons(
          #   ns("geo_level"),
          #   label = geo_lab,
          #   size = "sm",
          #   status = "outline-primary",
          #   choices = geo_levels
          # ),
          # Choropleth Inputs ==============
          tags$div(
            class = "d-flex",
            tags$h6("Choropleth Layer", class = "fw-bold text-decoration-underline pe-2"),
            bslib::input_switch(
              id = ns("choro_active"),
              label = NULL,
              value = TRUE,
              width = "30px"
            ) |>
              bslib::tooltip(
                "Show/Hide layer",
                id = ns("tt-choro"),
                placement = "top"
              )
          ),
          tags$div(
            # id = ns("choro_inputs"),
            class = "d-flex justify-content-center align-items-start",
            selectInput(
              ns("choro_pal"),
              label = "Palette",
              choices = choro_pals(),
              selected = "Reds",
              multiple = FALSE,
              selectize = FALSE
            )
            # numericInput(
            #   inputId = ns("choro_alpha"),
            #   label = "Alpha",
            #   value = 8,
            #   min = 1,
            #   max = 10,
            #   step = 1,
            #   width = "60px"
            # )
          ),
          bslib::input_switch(
            id = ns("choro_pal_rev"),
            label = "Reverse palette direction",
            value = FALSE
          ),
          tags$div(
            # id = ns("choro_inputs"),
            class = "d-flex justify-content-center align-items-start",
            selectInput(
              ns("choro_breaks"),
              label = "Breaks Method",
              choices = choro_breaks(),
              selected = "quantile",
              multiple = FALSE,
              selectize = FALSE,
              width = 180
            ),
            numericInput(
              inputId = ns("choro_nbreaks"),
              label = "N Breaks",
              value = 5,
              min = 2,
              max = 10,
              step = 1,
              width = "60px"
            )
          ),
          tags$hr(),
          # Symbols Inputs ==================
          tags$div(
            class = "d-flex",
            tags$h6("Symbols Layer", class = "fw-bold text-decoration-underline pe-2"),
            bslib::input_switch(
              id = ns("symbols_active"),
              label = NULL,
              value = TRUE,
              width = "30px"
            ) |>
              bslib::tooltip(id = ns("symbols_tt"), "Show/Hide layer", placement = "top")
          ),
          selectInput(
            ns("count_var"),
            label = count_vars_lab,
            choices = count_vars,
            multiple = FALSE,
            selectize = FALSE
          ),
          selectInput(
            ns("var"),
            label = groups_lab,
            choices = c(purrr::set_names("n", no_grouping_lab), group_vars),
            multiple = FALSE,
            selectize = FALSE
          ),
          sliderInput(
            ns("circle_size_mult"),
            label = circle_size_lab,
            min = 1,
            max = 10,
            value = 6,
            step = 1
          )
        ),
        # only show download button if chrome available
        if (!is.null(chromote::find_chrome())) {
          downloadLink(
            ns("dl"),
            label = bsicons::bs_icon("download", class = "text-primary", size = "1.2em"),
            title = download_lab,
            class = "ms-2"
            # icon = shiny::icon("camera"),
            # class = "btn-sm btn-link pe-2 me-2"
          ) |>
            bslib::tooltip(download_lab)
        },
        # tooltip at the end
        tt
      ),
      bslib::card_body(
        padding = 0,
        leaflet::leafletOutput(ns("map"))
      ),
      bslib::card_footer(uiOutput(ns("footer")))
    ),
    # add script to return dimensions of map back to server
    # to be used in the map export to replicate map on screen
    htmlwidgets::onStaticRenderComplete(
      sprintf(
        "
          const mapDiv = document.getElementById('%s');
          const resizeObserver = new ResizeObserver(entries => {
            for (let entry of entries) {
              if (entry.contentBoxSize) {
                const width = entry.contentBoxSize[0].inlineSize;
                const height = entry.contentBoxSize[0].blockSize;
                Shiny.setInputValue('%s', { width: width, height: height });
              } else {
                const width = entry.contentRect.width;
                const height = entry.contentRect.height;
                Shiny.setInputValue('%s', { width: width, height: height });
              }
            }
          });
          resizeObserver.observe(mapDiv);
        ",
        ns("map"),
        ns("map_dimensions"),
        ns("map_dimensions")
      )
    )
  )
}

#' @param df Data frame or tibble of patient level or aggregated data. Can be either a shiny reactive or static dataset.
#' @param show_parent_borders Show borders of parent boundary levels?
#' @param choro_lab Label for attack rate choropleth (only applicable if `geo_data` contains population data)
#' @param choro_pal Colour palette passed to [`leaflet::colorBin()`] for attack rate choropleth
#'  (only applicable if `geo_data` contains population data). See details of [`leaflet::colorBin()`]
#'  for all palette options.
#' @param choro_opacity Opacity of choropleth colour (only applicable if `geo_data` contains population data)
#' @param export_width The width of the exported map image.
#' @param export_height The height of the exported map image.
#' @param filter_info If contained within an app using [filter_server()], supply the `filter_info` object
#'   returned by that function here wrapped in a [shiny::reactive()] to add filter information to chart exports.
#' @param filter_reset If contained within an app using [filter_server()], supply the `filter_reset` object
#'   returned by that function here wrapped in a [shiny::reactive()] to reset any click event filters that have been set from by module.
#' @param time_filter supply the output of [time_server()] wrapped in a [shiny::reactive()] here to filter
#' the data by click events on the time module bar chart (clicking a bar will filter the data to the period the bar represents)
#'
#' @rdname place
#'
#' @return The server function returns the leaflet map's shape click information as a list.
#'
#' @export
place_server <- function(
  id,
  df,
  geo_data,
  count_vars = NULL,
  group_vars = NULL,
  show_parent_borders = FALSE,
  choro_lab = "Rate /100 000",
  choro_pal = "OrRd",
  choro_opacity = .7,
  export_width = 1200,
  export_height = 650,
  time_filter = shiny::reactiveVal(),
  filter_info = shiny::reactiveVal(),
  filter_reset = shiny::reactiveVal()
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # re-structure geo_data if only 1 layer provided
      if (inherits(geo_data, "epishiny_geo_layer")) {
        geo_data <- list(geo_data)
      }

      # hide inputs if redundant
      if (length(geo_data) < 2) {
        shinyjs::hide("geo_level")
      }

      if (is.null(group_vars)) {
        shinyjs::hide("var")
      }

      if (length(count_vars) < 2) {
        shinyjs::hide("count_var")
      }

      # check for chrome browser for map exports
      chrome_browser <- chromote::find_chrome()
      if (is.null(chrome_browser)) {
        cli::cli_alert_warning("epishiny place module map exports disabled")
        cli::cli_alert_info("To enable exports, install a chrome or chromium browser on your system")
        cli::cli_alert("see `?chromote::find_chrome()` for details")
      }

      # sf settings
      suppressMessages(sf::sf_use_s2(FALSE))

      # loading spinner for map export
      w_map <- waiter::Waiter$new(
        id = ns("map"),
        html = waiter::spin_3(),
        color = waiter::transparent(alpha = 0)
      )

      # ==========================================================================
      # DATA
      # ==========================================================================

      df_mod <- reactive({
        df_out <- force_reactive(df)
        tf <- time_filter()
        if (length(tf)) {
          df_out <- df_out %>% dplyr::filter(dplyr::between(.data[[tf$date_var]], tf$from, tf$to))
        }
        df_out
      })

      # adjust filter info if click event filtering has taken place
      filter_info_out <- reactive({
        fi <- filter_info()
        tf <- time_filter()
        format_filter_info(fi, tf)
      })

      geo_select <- reactive({
        # if geo_data is a single 'epishiny_geo_layer' use that
        # otherwise select with on geo_level input
        if (length(geo_data) == 1) {
          geo_data[[1]]
        } else {
          gd_index <- which(purrr::map_chr(geo_data, "layer_name") == input$geo_level)
          geo_data[[gd_index]]
        }
      })

      # update choropleth switch tooltip on popover open/close
      observe({
        req(geo_select())
        if (is.null(geo_select()$pop_var)) {
          bslib::update_tooltip("tt-choro", "Population data required for choropleth layer")
        } else {
          bslib::update_tooltip("tt-choro", "Show/Hide layer")
        }
      }) |>
        bindEvent(input$popover)

      rv <- reactiveValues()

      # update reactive values whenever inputs change
      observe({
        geo_join <- geo_select()$join_by
        join_cols <- if (rlang::is_named(geo_join)) names(geo_join) else geo_join
        geo_col <- unname(geo_join)
        geo_col_sym <- rlang::sym(geo_col)
        geo_name_col <- geo_select()$name_var
        geo_name_col_sym <- rlang::sym(geo_name_col)
        geo_level_name <- geo_select()$layer_name
        geo_pop_var <- geo_select()$pop_var
        map_var <- input$var
        map_var_sym <- rlang::sym(map_var)
        var_list <- c("n", group_vars)
        map_var_lab <- get_label(map_var, var_list)
        count_var <- input$count_var
        n_lab <- get_label(count_var, count_vars)

        # save as reactive values
        rv$geo_join <- geo_join
        rv$join_cols <- join_cols
        rv$geo_col <- geo_col
        rv$geo_col_sym <- geo_col_sym
        rv$geo_name_col <- geo_name_col
        rv$geo_name_col_sym <- geo_name_col_sym
        rv$geo_level_name <- geo_level_name
        rv$geo_pop_var <- geo_pop_var
        rv$map_var <- map_var
        rv$map_var_sym <- map_var_sym
        rv$map_var_lab <- map_var_lab
        rv$count_var <- count_var
        rv$n_lab <- n_lab
      })

      # filter geo boundaries to only those with incidence + their neighbours
      observe({
        geo_join <- geo_select()$join_by
        geo_col <- unname(geo_join)
        geo_col_sym <- rlang::sym(geo_col)
        sf <- geo_select()$sf
        affected <- sf %>% dplyr::semi_join(df_mod() %>% dplyr::filter(!is.na(!!geo_col_sym)), by = geo_join)
        sf <- suppressMessages(sf::st_filter(sf, affected))
        rv$sf <- sf
      })

      # ==========================================================================
      # MAP
      # ==========================================================================

      # basemap
      output$map <- leaflet::renderLeaflet({
        # bbox <- sf::st_bbox(geo_data[[1]]$sf)
        bbox <- sf::st_bbox(isolate(rv$sf))
        leaf_basemap(bbox, miniMap = TRUE)
      })

      # observe({
      #   print(input$map_dimensions)
      # })

      # manage map click events to return selected regions
      map_click <- reactiveVal(FALSE)
      region_select <- reactiveVal("all")

      # reset region select if a filter reset is passed from filter module
      observe({
        region_select("all")
      }) %>%
        bindEvent(filter_reset(), ignoreInit = TRUE)

      # reset region select if the geo level changes
      observe({
        region_select("all")
      }) %>%
        bindEvent(input$geo_level, ignoreInit = TRUE)

      # if region is selected from map, update region_select value
      observeEvent(input$map_shape_click, {
        map_click(TRUE)
        id <- input$map_shape_click$id
        if (id == region_select()) {
          region_select("all")
        } else {
          region_select(id)
        }
      })

      observeEvent(input$map_click, {
        if (map_click()) {
          map_click(FALSE)
        } else {
          region_select("all")
        }
      })

      # highlight selected region
      observeEvent(region_select(), {
        leaflet::leafletProxy("map", session) %>% leaflet::removeShape("highlight")
        r <- region_select()
        if (r != "all") {
          shp <- rv$sf %>% dplyr::filter(.data[[rv$join_cols]] == r)
          rv$region_select_name <- dplyr::pull(shp, rv$geo_name_col)
          leaflet::leafletProxy("map", session) %>%
            # leaflet::flyTo(lng = shp$lon, lat = shp$lat, zoom = input$map_zoom) %>%
            leaflet::addPolylines(
              data = shp,
              layerId = "highlight",
              stroke = TRUE,
              opacity = 1,
              weight = 3,
              color = "red",
              options = leaflet::pathOptions(pane = "geo_highlight")
            )
        } else {
          rv$region_select_name <- "All"
        }
      })

      # join ll data to boundaries
      df_geo_counts <- reactive({
        # is the data pre-aggregated
        is_agg <- as.logical(length(count_vars))

        df_counts <- get_geo_counts(
          df = df_mod(),
          is_agg = is_agg,
          geo_var = rv$geo_col,
          count_var = rv$count_var,
          count_lab = rv$n_lab
        )

        df_out <- rv$sf %>%
          dplyr::mutate(name = !!rv$geo_name_col_sym) %>%
          dplyr::select(dplyr::any_of(c(rv$join_cols, rv$geo_pop_var, "name", "lon", "lat"))) %>%
          dplyr::left_join(df_counts, by = rv$geo_join) %>%
          dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.double))
        # dplyr::mutate(dplyr::across(dplyr::where(is.double), ~ dplyr::if_else(is.na(.x), 0, .x)))

        # add attack rate if there is population data
        if (!is.null(rv$geo_pop_var)) {
          df_out <- df_out %>%
            dplyr::mutate(
              # attack rate per 100 000
              attack_rate = dplyr::na_if((.data$total / .data[[rv$geo_pop_var]]) * 1e5, 0)
            )
        }

        return(df_out)
      }) %>%
        bindEvent(df_mod(), rv$sf, rv$count_var)

      df_map_circles <- reactive({
        # drop geometry and unneeded cols
        df_geo_counts <- df_geo_counts() %>%
          sf::st_drop_geometry() %>%
          dplyr::select(-dplyr::any_of(c("attack_rate", rv$geo_pop_var)))
        # is the data pre-aggregated
        is_agg <- as.logical(length(count_vars))
        # is a data grouping variable supplied
        is_grouped <- rv$map_var != "n"
        # get df
        df_circles <- get_map_circle_df(
          df = df_mod(),
          is_agg = is_agg,
          is_grouped = is_grouped,
          geo_var = rv$geo_col,
          count_var = rv$count_var,
          group_var = rv$map_var,
          df_geo_counts = df_geo_counts,
          geo_join = rv$geo_join,
          n_lab = rv$n_lab
        )
      }) %>%
        bindEvent(df_geo_counts(), rv$map_var)

      # add polygon boundaries with tooltip data info
      observe({
        req(df_geo_counts())

        leaflet::leafletProxy("map", session) %>%
          leaflet::clearGroup("Boundaries") %>%
          leaflet::clearControls()

        # change group layers depening on if attack rate is available
        if (is.null(geo_select()$pop_var)) {
          ogs <- c("Circles")
          bslib::update_switch("choro_active", value = FALSE)
          shinyjs::disable("choro_active")
          bslib::update_tooltip("tt-choro", "Population data required for choropleth layer")
        } else {
          ogs <- c("Choropleth", "Circles")
          bslib::update_switch("choro_active", value = TRUE)
          shinyjs::enable("choro_active")
          bslib::update_tooltip("tt-choro", "Show/Hide layer")
        }

        leaflet::leafletProxy("map", session) %>%
          leaflet::addLayersControl(
            baseGroups = c("Light", "OSM", "OSM HOT"),
            overlayGroups = ogs,
            position = "topleft"
          )

        # if not first admin level, map borders of parent admin levels
        if (show_parent_borders) {
          gd <- geo_data
          geo_level <- which(purrr::map_chr(gd, "layer_name") == isolate(input$geo_level))
          if (geo_level > 1) {
            lower_levels <- 1:(geo_level - 1)
            purrr::walk(
              lower_levels,
              ~ {
                stroke_width <- (geo_level - .x) + 1
                borders <- suppressMessages(sf::st_filter(gd[[.x]]$sf, df_geo_counts())) |>
                  dplyr::filter(sf::st_is(geometry, c("POLYGON", "MULTIPOLYGON")))
                leaflet::leafletProxy("map", session) %>%
                  leaflet::addPolylines(
                    data = borders,
                    group = "Boundaries",
                    color = "grey",
                    weight = stroke_width
                  )
              }
            )
          }
        }

        # only contunue if layer has polygon geometries
        boundaries <- df_geo_counts() |>
          dplyr::filter(sf::st_is(geometry, c("POLYGON", "MULTIPOLYGON")))

        req(nrow(boundaries) > 0)

        # get bbox for fly to step
        bbox <- sf::st_bbox(boundaries)

        # tooltip hover labels for each polygon
        tt <- make_leaf_tooltip(
          boundaries,
          n_lab = rv$n_lab,
          pop_col = rv$geo_pop_var,
          ar_col = "attack_rate"
        )

        leaflet::leafletProxy("map", session) %>%
          leaflet::addPolygons(
            data = boundaries,
            layerId = boundaries[[rv$join_cols]],
            stroke = TRUE,
            color = "grey",
            weight = 1,
            fillOpacity = 0,
            label = tt,
            group = "Boundaries",
            highlightOptions = leaflet::highlightOptions(bringToFront = TRUE, weight = 3),
            options = leaflet::pathOptions(pane = "boundaries")
          )
        # leaflet::flyToBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
      }) %>%
        bindEvent(df_geo_counts())

      # add/update Choropleth polygons when df_geo_counts() changes
      observe({
        req(df_geo_counts())

        leaflet::leafletProxy("map", session) %>%
          leaflet::clearGroup("Choropleth") %>%
          leaflet::removeControl(layerId = "attack_legend")

        # only plot polygons with incidence
        df_map <- df_geo_counts() %>%
          dplyr::filter(
            sf::st_is(geometry, c("POLYGON", "MULTIPOLYGON")),
            .data$total > 0
          )

        if (isTruthy(nrow(df_map) > 0) && !is.null(rv$geo_pop_var) && input$choro_active) {
          req(!all(is.na(df_map$attack_rate)))
          # lvls <- levels(df_map$ar_bin)
          # pal <- leaflet::colorFactor(cols, levels = lvls, na.color = "transparent", ordered = TRUE)
          req(input$choro_nbreaks)
          safe_breaks <- purrr::safely(mapsf::mf_get_breaks, otherwise = 4)
          bins <- suppressWarnings(safe_breaks(
            df_map$attack_rate,
            nbreaks = input$choro_nbreaks,
            breaks = input$choro_breaks
          ))

          if (!is.null(bins$error)) {
            shiny::showNotification(
              stringr::str_glue("{input$choro_breaks} breaks could not be calculated for this data. Reverting to default breaks."),
              type = "error"
            )
          }

          pal <- leaflet::colorBin(
            palette = input$choro_pal,
            domain = df_map$attack_rate,
            bins = bins$result,
            # alpha = input$choro_alpha / 10,
            reverse = input$choro_pal_rev,
            na.color = "transparent"
          )

          leaflet::leafletProxy("map", session) %>%
            leaflet::addPolygons(
              data = df_map,
              stroke = TRUE,
              color = "grey",
              weight = 1,
              fillColor = ~ pal(attack_rate),
              fillOpacity = choro_opacity,
              highlightOptions = leaflet::highlightOptions(bringToFront = TRUE, weight = 3),
              group = "Choropleth",
              options = leaflet::pathOptions(pane = "choropleth")
            ) %>%
            leaflet::addLegend(
              title = choro_lab,
              data = df_map,
              pal = pal,
              values = ~attack_rate,
              opacity = choro_opacity,
              position = "bottomright",
              group = "Choropleth",
              layerId = "attack_legend"
            )
        }
      }) %>%
        bindEvent(
          df_geo_counts(),
          input$choro_pal,
          input$choro_pal_rev,
          input$choro_breaks,
          input$choro_nbreaks,
          input$choro_active
        )

      # minichart circles/pies
      minicharts_init <- reactiveVal(TRUE)
      minicharts_on <- reactiveVal(TRUE)
      observe({
        req(df_map_circles())
        df_map <- df_map_circles()
        leaflet::leafletProxy("map", session) %>% leaflet.minicharts::clearMinicharts()

        req(nrow(df_map) > 0)

        if (input$symbols_active | minicharts_init()) {
          # isTruthy("Circles" %in% isolate(input$map_groups))
          chart_data <- df_map %>%
            dplyr::select(
              -dplyr::any_of(c(
                rv$join_cols,
                rv$geo_pop_var,
                "attack_rate",
                "name",
                "lon",
                "lat",
                "total"
              ))
            )

          pie_width <- (input$circle_size_mult * 10) * (sqrt(df_map$total) / sqrt(max(df_map$total)))

          leaflet::leafletProxy("map", session) %>%
            leaflet.minicharts::addMinicharts(
              lng = df_map$lon,
              lat = df_map$lat,
              layerId = df_map$name,
              chartdata = chart_data,
              opacity = .7,
              fillColor = epi_pals()$d310[1],
              colorPalette = epi_pals()$d310,
              legend = TRUE,
              showLabels = TRUE,
              type = "pie",
              width = pie_width
            )

          minicharts_init(FALSE)
        }
      }) %>%
        bindEvent(df_map_circles(), input$circle_size_mult)

      # show/hide circles when selected/unselected from map groups
      observeEvent(input$symbols_active, {
        if (!input$symbols_active) {
          # !"Circles" %in% input$map_group
          leaflet::leafletProxy("map", session) %>%
            leaflet.minicharts::clearMinicharts()
          minicharts_on(FALSE)
        } else if (!minicharts_on()) {
          df_map <- df_map_circles()
          req(nrow(df_map) > 0)
          chart_data <- df_map %>%
            dplyr::select(
              -dplyr::any_of(c(
                rv$join_cols,
                rv$geo_pop_var,
                "attack_rate",
                "name",
                "lon",
                "lat",
                "total"
              ))
            )
          pie_width <- (input$circle_size_mult * 10) * (sqrt(df_map$total) / sqrt(max(df_map$total)))
          leaflet::leafletProxy("map", session) %>%
            leaflet.minicharts::addMinicharts(
              lng = df_map$lon,
              lat = df_map$lat,
              layerId = df_map$name,
              chartdata = chart_data,
              opacity = .7,
              fillColor = epi_pals()$d310[1],
              colorPalette = epi_pals()$d310,
              legend = TRUE,
              showLabels = TRUE,
              type = "pie",
              width = pie_width
            )
          minicharts_on(TRUE)
        }
      })

      # Missing data information ==================================================
      missing_text <- reactive({
        df_missing <- df_mod() %>%
          dplyr::anti_join(geo_select()$sf, by = purrr::set_names(rv$join_cols, rv$geo_col))

        if (length(count_vars)) {
          n_missing <- df_missing %>% dplyr::pull(.data[[rv$count_var]]) %>% sum(na.rm = TRUE)
          n_total <- df_mod() %>% dplyr::pull(.data[[rv$count_var]]) %>% sum(na.rm = TRUE)
          pcnt_missing <- n_missing / n_total
        } else {
          n_missing <- nrow(df_missing)
          pcnt_missing <- n_missing / nrow(df_mod())
        }

        if (n_missing == 0) {
          return(NULL)
        } else {
          lab_missing <- glue::glue("{scales::number(n_missing)} ({scales::percent(pcnt_missing, accuracy = .1)})")
          glue::glue("Missing/Unknown {rv$geo_level_name} data for {lab_missing} {tolower(rv$n_lab)}")
        }
      })

      output$footer <- renderUI({
        req(missing_text())
        tags$span(tags$small(
          HTML('<i class="fa fa-exclamation-triangle" style="color:red;"></i>'),
          missing_text()
        ))
      })

      # Map image export ==========================================================
      output$dl <- downloadHandler(
        filename = function() {
          glue::glue("EPI-MAP-{time_stamp()}.png")
        },
        content = function(file) {
          # check for chrome browser before attempting mapshot2
          if (is.null(chrome_browser)) {
            shiny::showModal(
              shiny::modalDialog(
                title = "No Chrome or Chromium browser found",
                paste(
                  "The place module map export requires a Chrome or Chromium browser (Google Chrome, Chromium, Microsoft Edge and others)",
                  "to be installed on the system running the shiny app in order to work."
                )
              )
            )
          }
          req(chrome_browser)

          # show loading spinner and notif and remove when done
          w_map$show()
          ntf <- showNotification(
            "Generating map export. This can take a while...",
            type = "default",
            duration = NULL
          )
          on.exit({
            w_map$hide()
            removeNotification(ntf)
          })

          # rebuild current map shown on dashboard
          missing_data_text <- missing_text()
          if (!is.null(missing_data_text)) {
            missing_data_text <- glue::glue("<b>Missing data</b></br>{missing_data_text}")
          }

          # get the centroid coordinates of current onscreen map view
          # to set the view in export map
          bbox <- sf::st_bbox(
            c(
              xmin = input$map_bounds$east,
              xmax = input$map_bounds$west,
              ymax = input$map_bounds$north,
              ymin = input$map_bounds$south
            ),
            crs = sf::st_crs(4326)
          )
          sv <- dplyr::as_tibble(sf::st_coordinates(suppressWarnings(sf::st_centroid(sf::st_as_sfc(bbox)))))

          leaf_out <- leaflet::leaflet() %>%
            leaflet::setView(sv$X, sv$Y, zoom = input$map_zoom) %>%
            # leaflet::fitBounds(
            #   input$map_bounds$east,
            #   input$map_bounds$south,
            #   input$map_bounds$west,
            #   input$map_bounds$north
            # ) %>%
            leaflet::addMapPane(name = "boundaries", zIndex = 300) %>%
            leaflet::addMapPane(name = "choropleth", zIndex = 310) %>%
            leaflet::addMapPane(name = "circles", zIndex = 410) %>%
            leaflet::addMapPane(name = "region_highlight", zIndex = 420) %>%
            leaflet::addMapPane(name = "place_labels", zIndex = 320) %>%
            leaflet::addMiniMap(toggleDisplay = FALSE, position = "topleft") %>%
            leaflet::addControl(
              html = tags$b(ifelse(rv$map_var_lab == "n", rv$n_lab, rv$map_var_lab)),
              position = "topright"
            ) %>%
            leaflet::addScaleBar(
              position = "bottomright",
              options = leaflet::scaleBarOptions(imperial = FALSE)
            ) %>%
            leaflet::addControl(
              html = shiny::HTML(
                glue::glue_collapse(c(missing_data_text, filter_info_out()), sep = "</br>")
              ),
              className = "leaflet-control-attribution",
              position = "bottomleft"
            )

          # plot boundaries of polygons if there are any in the data
          boundaries <- rv$sf |>
            dplyr::filter(sf::st_is(geometry, c("MULTIPOLYGON", "POLYGON")))

          if (nrow(boundaries) > 0) {
            leaf_out <- leaf_out |>
              leaflet::addPolygons(
                data = boundaries,
                stroke = TRUE,
                color = "grey",
                weight = 1,
                fillOpacity = 0,
                label = boundaries[[rv$geo_name_col]],
                group = "Boundaries",
                options = leaflet::pathOptions(pane = "boundaries")
              )
          }

          if (isTruthy("Circles" %in% input$map_groups)) {
            df_circles <- df_map_circles()
            chart_data <- df_circles %>%
              dplyr::select(
                -dplyr::any_of(c(
                  rv$join_cols,
                  rv$geo_pop_var,
                  "attack_rate",
                  "name",
                  "lon",
                  "lat",
                  "total"
                ))
              )
            # * 7 instead of * 10 like in the app map because
            # circles are coming out larger in the image export
            pie_width <- (input$circle_size_mult * 7) * (sqrt(df_circles$total) / sqrt(max(df_circles$total)))
            leaf_out <- leaf_out %>%
              leaflet.minicharts::addMinicharts(
                lng = df_circles$lon,
                lat = df_circles$lat,
                layerId = df_circles$name,
                chartdata = chart_data,
                opacity = .8,
                fillColor = epi_pals()$d310[1],
                colorPalette = epi_pals()$d310,
                legend = TRUE,
                showLabels = TRUE,
                type = "pie",
                width = pie_width
              )
          }

          if (isTruthy("Choropleth" %in% input$map_groups) && !is.null(rv$geo_pop_var)) {
            df_map <- df_geo_counts() %>%
              dplyr::filter(
                sf::st_is(geometry, c("POLYGON", "MULTIPOLYGON")),
                .data$total > 0
              )

            req(nrow(df_map) > 0)
            req(!all(is.na(df_map$attack_rate)))

            pal <- leaflet::colorBin(
              palette = input$choro_pal,
              domain = df_map$attack_rate,
              bins = 5,
              na.color = "transparent"
            )
            leaf_out <- leaf_out %>%
              leaflet::addPolygons(
                data = df_map,
                stroke = TRUE,
                color = "grey",
                weight = 1,
                fillColor = ~ pal(attack_rate),
                fillOpacity = choro_opacity,
                highlightOptions = leaflet::highlightOptions(bringToFront = TRUE, weight = 3),
                group = "Choropleth",
                options = leaflet::pathOptions(pane = "choropleth")
              ) %>%
              leaflet::addLegend(
                title = choro_lab,
                data = df_map,
                pal = pal,
                values = ~attack_rate,
                opacity = choro_opacity,
                position = "bottomright",
                group = "Choropleth",
                layerId = "attack_legend"
              )
          }

          # if not first admin level, map borders of parent admin levels
          if (show_parent_borders) {
            gd <- geo_data
            geo_level <- which(purrr::map_chr(gd, "layer_name") == isolate(input$geo_level))
            if (geo_level > 1) {
              lower_levels <- 1:(geo_level - 1)
              for (i in lower_levels) {
                stroke_width <- (geo_level - i) + 1
                borders <- suppressMessages(sf::st_filter(gd[[i]]$sf, boundaries))
                leaf_out <- leaf_out %>%
                  leaflet::addPolylines(
                    data = borders,
                    group = "Boundaries",
                    color = "grey",
                    weight = stroke_width,
                    options = leaflet::pathOptions(pane = "boundaries")
                  )
              }
            }
          }

          tiles <- dplyr::recode(
            input$map_groups[[1]],
            "Light" = "CartoDB.PositronNoLabels",
            "OSM" = "OpenStreetMap",
            "OSM HOT" = "OpenStreetMap.HOT"
          )

          if (tiles == "CartoDB.PositronNoLabels") {
            leaf_out <- leaf_out %>%
              leaflet::addProviderTiles(tiles) %>%
              leaflet::addProviderTiles(
                "CartoDB.PositronOnlyLabels",
                options = leaflet::leafletOptions(pane = "place_labels")
              )
          } else {
            leaf_out <- leaf_out %>% leaflet::addProviderTiles(tiles)
          }

          mapshot2(
            leaf_out,
            file = file,
            remove_controls = c(
              "zoomControl",
              "layersControl",
              "homeButton",
              "drawToolbar",
              "easyButton"
            ),
            selfcontained = FALSE,
            vwidth = round(input$map_dimensions$width, 0),
            vheight = round(input$map_dimensions$height, 0),
            zoom = 2,
            delay = 0.5
          )
        }
      )

      # return region select click information to main app
      shiny::reactive({
        if (region_select() == "all") {
          return(NULL)
        } else {
          list(
            region_select = region_select(),
            geo_col = rv$geo_col,
            level_name = rv$geo_level_name,
            region_name = rv$region_select_name
          )
        }
      })
    }
  )
}

#' Build a geo layer to be used in the 'place' module
#'
#' @param layer_name the name of the geo layer, for example 'State', 'Department', 'Admin2' etc.
#'  If providing multiple layers, layer names must be unique.
#' @param sf geographical data of class 'sf' (simple features).
#' @param name_var character string of the variable name in `sf` containing the names of each geographical feature.
#' @param join_by data join specification to join geo layer to a dataset. Should be either a single variable name
#'  present in both datasets or a named vector where the name is the geo layer join variable and the value is the
#'  join variable of the dataset. i.e. `c("pcode" = "place_code")` LHS = geo, RHS = data.
#' @param pop_var character string of the variable name in `sf` containing population data for each feature.
#'  If provided, attack rates will be shown on the map as a choropleth.
#'
#' @return named list of class "epishiny_geo_layer"
#'
#' @examples
#' geo_layer(
#'   layer_name = "Governorate",
#'   sf = sf_yem$adm1,
#'   name_var = "adm1_name",
#'   pop_var = "adm1_pop",
#'   join_by = c("pcode" = "adm1_pcode")
#' )
#' @export
geo_layer <- function(layer_name, sf, name_var, join_by, pop_var = NULL) {
  # check arguments
  rlang::check_required(layer_name)
  rlang::check_required(sf)
  rlang::check_required(name_var)
  rlang::check_required(join_by)
  check_single_string(layer_name)
  check_single_string(name_var)
  if (!is.null(pop_var)) check_single_string(pop_var)
  if (!"sf" %in% class(sf)) {
    cli::cli_abort("{.var {rlang::caller_arg(sf)}} is not an sf object")
  }
  if (!rlang::is_string(join_by) | length(join_by) != 1) {
    cli::cli_abort(c(
      "{.arg join_by} must be a single variable name string or a named string",
      "*" = "if named, the name should be the name of the variable in {.var {rlang::caller_arg(sf)}} to be used for joining to your dataset.",
      "*" = "the value should be the name of the variable in your dataset to be used for joining to {.var {rlang::caller_arg(sf)}}.",
      "*" = "i.e. `c('pcode' = 'place_code')` where `pcode` is in {.var {rlang::caller_arg(sf)}} and `place_code` is in your dataset.",
      "*" = "if not named, the variable name must be present in both {.var {rlang::caller_arg(sf)}} and your dataset."
    ))
  }
  sf_join_col <- if (rlang::is_named(join_by)) names(join_by) else join_by
  if (!sf_join_col %in% colnames(sf)) {
    cli::cli_abort("join column '{sf_join_col}' not found in {.var {rlang::caller_arg(sf)}}")
  }
  # add lon lat coords to sf if not already there
  sf <- add_coords(sf)
  # return named list with epishiny_geo_layer class
  structure(
    tibble::lst(layer_name, sf, name_var, pop_var, join_by),
    class = "epishiny_geo_layer"
  )
}

#' @noRd
check_single_string <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!rlang::is_string(x) | length(x) != 1) {
    cli::cli_abort("{.arg {arg}} must be a character string of length 1.", call = call)
  }
}

#' @noRd
add_coords <- function(sf) {
  if (all(c("lon", "lat") %in% colnames(sf))) {
    sf
  } else {
    coords <- sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(sf::st_zm(sf))))
    sf %>% dplyr::mutate(lon = coords[, 1], lat = coords[, 2])
  }
}

#' @noRd
get_geo_counts <- function(
  df,
  is_agg,
  geo_var,
  count_var,
  count_lab
) {
  if (is_agg) {
    df <- dplyr::count(df, .data[[geo_var]], wt = .data[[count_var]], name = count_lab)
  } else {
    df <- dplyr::count(df, .data[[geo_var]], name = count_lab)
  }
  dplyr::mutate(df, total = .data[[count_lab]])
}

#' @noRd
get_map_circle_df <- function(
  df,
  is_agg,
  is_grouped,
  geo_var,
  count_var,
  group_var,
  df_geo_counts,
  geo_join,
  n_lab
) {
  if (!is_grouped) {
    df <- df_geo_counts
  } else {
    if (is_agg) {
      df <- dplyr::count(df, .data[[geo_var]], .data[[group_var]], wt = .data[[count_var]])
    } else {
      df <- dplyr::count(df, .data[[geo_var]], .data[[group_var]])
    }
    # dplyr::mutate(total = rowSums(dplyr::pick(dplyr::where(is.numeric))))
    df <- df_geo_counts %>%
      dplyr::select(-dplyr::any_of(n_lab)) %>%
      dplyr::left_join(
        df %>% tidyr::pivot_wider(names_from = group_var, values_from = "n"),
        by = geo_join
      ) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.double)) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.double), ~ dplyr::if_else(is.na(.x), 0, .x)))
  }
  df %>% dplyr::filter(.data$total > 0)
}

#' Copy of mapview::mapshot2 with minor changes to avoid the full dependency on mapview
#' Full credit to the mapview authors
#' @noRd
mapshot2 <- function(
  x,
  url = NULL,
  file = NULL,
  remove_controls = c(
    "zoomControl",
    "layersControl",
    "homeButton",
    "scaleBar",
    "drawToolbar",
    "easyButton",
    "control"
  ),
  ...
) {
  stopifnot(requireNamespace("webshot2", quietly = TRUE))

  ## if both 'url' and 'file' are missing, throw an error
  avl_url <- !is.null(url)
  avl_file <- !is.null(file)

  if (!avl_url & !avl_file) {
    stop("Please provide a valid 'url' or 'file' argument (or both).")
  }

  ## normalize path to ensure webshot is working
  if (avl_url) url <- normalizePath(url, mustWork = FALSE)
  if (avl_file) file <- normalizePath(file, mustWork = FALSE)

  ## if no url provided -> set url to tempfile & remove junk
  if (!avl_url) {
    url <- tempfile(fileext = ".html")
    x <- removeMapJunk(x, remove_controls)
  }

  ## prepare arguments for saveWidget & webshot
  args <- list(url = url, file = file, ...)
  sw_ls <- args
  sw_ls[names(sw_ls) == "file"] <- NULL
  names(sw_ls)[which(names(sw_ls) == "url")] <- "file"

  ## the arguments to be passed to saveWidget
  sw_args <- match.arg(names(sw_ls), names(as.list(args(htmlwidgets::saveWidget))), several.ok = TRUE)

  ## the arguments to be passed to webshot
  ws_args <- match.arg(names(args), names(as.list(args(webshot2::webshot))), several.ok = TRUE)

  ## if file was provided
  if (avl_file) {
    ## if no junk to remove -> take webshot straight away & return
    if (is.null(remove_controls)) {
      suppressMessages(
        do.call(webshot2::webshot, args)
      )
      return(invisible())
    }

    ## if we land here, we want both url & file with some junk removed
    tmp_url <- tempfile(fileext = ".html")
    tmp_fls <- paste0(tools::file_path_sans_ext(tmp_url), "_files")

    sw_ls <- utils::modifyList(sw_ls, list("file" = tmp_url))
    args$url <- tmp_url

    x <- removeMapJunk(x, remove_controls)

    do.call(htmlwidgets::saveWidget, append(list(x), sw_ls[sw_args]))
    suppressMessages(
      do.call(webshot2::webshot, args[ws_args])
    )

    return(invisible())
  }
}

#' @noRd
removeMapJunk <- function(map, junk = NULL) {
  if (is.null(junk)) {
    return(map)
  }
  for (jnk in junk) {
    map <- switch(
      jnk,
      "zoomControl" = removeZoomControl(map),
      "layersControl" = leaflet::removeLayersControl(map),
      "homeButton" = removeHomeButtons(map),
      "scaleBar" = removeScalebar(map),
      "drawToolbar" = removeDrawToolbar(map),
      "easyButton" = removeEasyButton(map),
      "control" = removeControl(map),
      NULL = map
    )
  }
  return(map)
}

#' @noRd
removeZoomControl <- function(map) {
  map$x$options <- append(map$x$options, list("zoomControl" = FALSE))
  return(map)
}

#' @noRd
removeHomeButtons <- function(map) {
  idx <- getCallEntryFromMap(map, "addHomeButton")
  if (length(idx) > 0) map$x$calls[idx] <- NULL
  return(map)
}

#' @noRd
removeScalebar <- function(map) {
  idx <- getCallEntryFromMap(map, "addScaleBar")
  if (length(idx) > 0) map$x$calls[idx] <- NULL
  return(map)
}

#' @noRd
removeDrawToolbar <- function(map) {
  idx <- getCallEntryFromMap(map, "addDrawToolbar")
  if (length(idx) > 0) map$x$calls[idx] <- NULL
  return(map)
}

#' @noRd
removeEasyButton <- function(map) {
  idx <- getCallEntryFromMap(map, "addEasyButton")
  if (length(idx) > 0) map$x$calls[idx] <- NULL
  return(map)
}

#' @noRd
removeControl <- function(map) {
  idx <- getCallEntryFromMap(map, "addControl")
  if (length(idx) > 0) map$x$calls[idx] <- NULL
  return(map)
}

#' @noRd
getCallMethods <- function(map) {
  sapply(map$x$calls, "[[", "method")
}

#' @noRd
getCallEntryFromMap <- function(map, call) {
  if (length(call) > 1) {
    call <- paste(call, collapse = "|")
    fixed <- FALSE
  } else {
    fixed <- TRUE
  }
  grep(call, getCallMethods(map), fixed = fixed, useBytes = TRUE)
}
