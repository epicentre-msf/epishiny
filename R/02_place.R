#' Place module
#'
#' Visualise geographical distribution across multiple administrative boundaries on an interactive leaflet map.
#'
#' @rdname place
#'
#' @param id Module id. Must be the same in both the UI and server function to link the two.
#' @param geo_data A list of named lists containing spatial sf dataframes and other information for different geographical levels.
#' @param group_vars Character vector of categorical variable names. If provided, a select input will appear
#'  in the options dropdown allowing for data groups to be visualised on the map in pie charts per geographical unit. 
#'  If named, names are used as variable labels.
#' @param title The title for the card.
#' @param icon The icon to be displayed next to the title
#' @param geo_lab The label for the geographical level selection.
#' @param groups_lab The label for the group data by selection.
#' @param circle_size_lab text label for the circle size slider input.
#' @param opts_btn_lab text label for the dropdown menu button.
#' @param download_lab text label for the download button.
#' @param n_lab The label for the raw count variable.
#' @param full_screen Add button to card to with the option to enter full screen mode?
#'
#' @return A [bslib::card] UI element with options and download button and a leaflet map.
#' @export
#' @example inst/examples/docs/app.R
place_ui <- function(
    id,
    geo_data,
    group_vars = NULL,
    title = "Place",
    icon = bsicons::bs_icon("geo-fill"),
    geo_lab = "Geo boundaries",
    groups_lab = "Group data by",
    circle_size_lab = "Circle size multiplyer",
    opts_btn_lab = "options",
    download_lab = "download",
    n_lab = "N patients",
    full_screen = TRUE
) {
  ns <- shiny::NS(id)

  # check geo_data meets criteria for use
  geo_data <- validate_geo_data(geo_data)
  # use level_name for display names if found, otherwise list item name
  geo_levels <- purrr::set_names(
    names(geo_data),
    purrr::map2_chr(
      unname(geo_data),
      names(geo_data),
      function(x, y) purrr::pluck(x, "level_name", .default = y)
    )
  )

  tagList(
    use_epishiny(),
    bslib::card(
      full_screen = full_screen,
      bslib::card_header(
        class = "d-flex justify-content-start align-items-center",
        tags$span(icon, title, class = "pe-2"),

        # options button and dropdown menu
        bslib::popover(
          title =  tags$span(shiny::icon("sliders"), opts_btn_lab),
          trigger = actionButton(
            ns("dropdown"),
            icon = shiny::icon("sliders"),
            label = opts_btn_lab,
            class = "btn-sm pe-2 me-2"
          ),
          shinyWidgets::radioGroupButtons(
            ns("geo_level"),
            label = geo_lab,
            size = "sm",
            status = "outline-dark",
            choices = geo_levels
          ),
          selectInput(
            ns("var"),
            label = groups_lab,
            choices = c(purrr::set_names("n", n_lab), group_vars),
            multiple = FALSE,
            selectize = FALSE,
            width = 200
          ),
          sliderInput(
            ns("circle_size_mult"),
            label = circle_size_lab,
            min = 0,
            max = 10,
            value = 6,
            step = 1,
            width = 200
          )
        ),
        # only show download button if chrome available
        if (!is.null(chromote::find_chrome())) { 
          downloadButton(
            ns("dl"),
            label = download_lab,
            icon = shiny::icon("camera"),
            class = "btn-sm pe-2 me-2"
          )
        } 
      ),
      bslib::card_body(
        padding = 0,
        leaflet::leafletOutput(ns("map"))
      ),
      bslib::card_footer(uiOutput(ns("footer")))
    )
  )
}

#' @param df_ll Data frame or tibble of patient level linelist data. Can be either a shiny reactive or static dataset.
#' @param show_parent_borders Show borders of parent boundary levels?
#' @param choro_lab Label for attack rate choropleth (only applicable if `geo_data` contains population data)
#' @param choro_pal Colour palette passed to [`leaflet::colorBin()`] for attack rate choropleth 
#'  (only applicable if `geo_data` contains population data)
#' @param choro_opacity Opacity of choropleth colour (only applicable if `geo_data` contains population data)
#' @param export_width The width of the exported map image.
#' @param export_height The height of the exported map image.
#' @param filter_info If contained within an app using [filter_server()], supply the `filter_info` element
#'   returned by that function here as a shiny reactive to add filter information to chart exports.
#' 
#' @importFrom mapview mapshot2
#'
#' @rdname place
#'
#' @return The server function returns the leaflet map's shape click information as a list.
#'
#' @export
place_server <- function(
    id,
    df_ll,
    geo_data,
    group_vars = NULL,
    n_lab = "N patients",
    show_parent_borders = TRUE,
    choro_lab = "Attack rate<br>per 100 000",
    choro_pal = "Reds",
    choro_opacity = .7,
    export_width = 1200,
    export_height = 650,
    filter_info = shiny::reactiveVal()
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      if (is.null(group_vars)) {
        shinyjs::hide("var")
      }

      # check for chrome browser for map exports
      chrome_browser <- chromote::find_chrome()
      if (is.null(chrome_browser)) {
        cli::cli_alert_warning("epishiny place module map exports disabled")
        cli::cli_alert_info("To enable exports, install a chrome or chromium browser on your system")
        cli::cli_alert("see ?chromote::find_chrome() for details")
      }

      # sf settings
      # sf::sf_use_s2(FALSE)

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
        force_reactive(df_ll)
      })

      # add point on surface lon lat coords if not already present
      # required for circle/pie position on map
      geo_data <- purrr::map(geo_data, function(x) {
        x$sf <- add_coords(x$sf)
        return(x)
      })

      geo_select <- reactive({
        gd <- force_reactive(geo_data)
        gd[[input$geo_level]]
      })

      rv <- reactiveValues()

      # update reactive values whenever inputs change
      observe({
        geo_join <- geo_select()$join_by
        join_cols <- dplyr::if_else(rlang::is_named(geo_join), names(geo_join), unname(geo_join))
        geo_col <- unname(geo_join)
        geo_col_sym <- rlang::sym(geo_col)
        geo_name_col <- geo_select()$name_var
        geo_name_col_sym <- rlang::sym(geo_name_col)
        geo_level_name <- geo_select()$level_name
        geo_pop_var <- geo_select()$pop_var
        map_var <- input$var
        map_var_sym <- rlang::sym(map_var)
        var_list <- c(purrr::set_names("n", n_lab), group_vars)
        map_var_lab <- ifelse(
          is.null(names(var_list[var_list == map_var])),
          map_var,
          names(var_list[var_list == map_var])
        )
        
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
        bbox <- sf::st_bbox(geo_data[[1]]$sf)
        leaf_basemap(bbox, miniMap = TRUE)
      })

      # manage map click events to return selected regions
      map_click <- reactiveVal(FALSE)
      region_select <- reactiveVal("all")

      observeEvent(input$geo_level, ignoreInit = TRUE, {
        region_select("all")
      })

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
        if (rv$map_var == "n") {
          var_lab <- n_lab
          df_counts <- df_mod() %>%
            dplyr::count(.data[[rv$geo_col]], name = var_lab) %>%
            dplyr::mutate(total = .data[[var_lab]])
        } else {
          df_counts <- df_mod() %>%
            janitor::tabyl(.data[[rv$geo_col]], .data[[rv$map_var]], show_missing_levels = FALSE) %>%
            janitor::adorn_totals("col", name = "total")
        }

        df_out <- rv$sf %>%
          dplyr::mutate(name = !!rv$geo_name_col_sym) %>%
          dplyr::select(dplyr::any_of(c(rv$join_cols, rv$geo_pop_var, "name", "lon", "lat"))) %>%
          dplyr::left_join(df_counts, by = rv$geo_join) %>%
          dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.double)) %>%
          dplyr::mutate(dplyr::across(dplyr::where(is.double), ~ dplyr::if_else(is.na(.x), 0, .x)))

        # add attack rate if there is population data
        if (!is.null(rv$geo_pop_var)) {
          df_out <- df_out %>%
            dplyr::mutate(
              attack_rate = dplyr::na_if((.data$total / .data[[rv$geo_pop_var]]) * 1e5, 0)
            ) # attack rate per 100 000
        }

        return(df_out)
      }) %>% bindEvent(df_mod(), rv$sf, rv$map_var)

      # add polygon boundaries with tooltip data info
      observe({
        req(df_geo_counts())
        boundaries <- df_geo_counts()

        leaflet::leafletProxy("map", session) %>%
          leaflet::clearGroup("Boundaries") %>% 
          leaflet::clearControls()
        
        # change group layers depening on if attack rate is available
        if (is.null(geo_select()$pop_var)) {
          ogs <- c("Boundaries", "Circles")
        } else {
          ogs <- c("Boundaries", "Attack rate", "Circles")
        }

        leaflet::leafletProxy("map", session) %>%
          leaflet::addLayersControl(
            baseGroups = c("Light", "OSM", "OSM HOT"),
            overlayGroups = ogs,
            position = "topleft"
          )

        req(nrow(boundaries) > 0)

        # if not first admin level, map borders of parent admin levels
        if (show_parent_borders) {
          gd <- force_reactive(geo_data)
          geo_level <- which(names(gd) == isolate(input$geo_level))
          if (geo_level > 1) {
            lower_levels <- 1:(geo_level - 1)
            purrr::walk(lower_levels, ~ {
              stroke_width <- (geo_level - .x) + 1
              borders <- suppressMessages(sf::st_filter(gd[[.x]]$sf, boundaries))
              leaflet::leafletProxy("map", session) %>%
                leaflet::addPolylines(
                  data = borders,
                  group = "Boundaries",
                  color = "grey",
                  weight = stroke_width
                )
            })
          }
        }

        # get bbox for fly to step
        bbox <- sf::st_bbox(boundaries)

        # tooltip hover labels for each polygon
        tt <- make_leaf_tooltip(
          boundaries,
          n_lab = n_lab,
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
          ) %>%
          leaflet::flyToBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
      }) %>% bindEvent(df_geo_counts())

      # add/update attack rate polygons when df_geo_counts() changes
      observe({
        req(df_geo_counts())

        leaflet::leafletProxy("map", session) %>%
          leaflet::clearGroup("Attack rate") %>%
          leaflet::removeControl(layerId = "attack_legend")

        # only plot polygons with incidence
        df_map <- df_geo_counts() %>% dplyr::filter(.data$total > 0)

        if (isTruthy(nrow(df_map) > 0) & !is.null(rv$geo_pop_var)) {
          # lvls <- levels(df_map$ar_bin)
          # pal <- leaflet::colorFactor(cols, levels = lvls, na.color = "transparent", ordered = TRUE)
          pal <- leaflet::colorBin(
            palette = choro_pal,
            domain = df_map$attack_rate,
            bins = 5,
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
              group = "Attack rate",
              options = leaflet::pathOptions(pane = "choropleth")
            ) %>%
            leaflet::addLegend(
              title = choro_lab,
              data = df_map,
              pal = pal,
              values = ~attack_rate,
              opacity = choro_opacity,
              position = "bottomright",
              group = "Attack rate",
              layerId = "attack_legend"
            )
        }
      }) %>% bindEvent(df_geo_counts())

      # minichart circles/pies 
      minicharts_init <- reactiveVal(TRUE)
      minicharts_on <- reactiveVal(TRUE)
      observe({
        req(df_geo_counts())
        df_map <- sf::st_drop_geometry(df_geo_counts())
        leaflet::leafletProxy("map", session) %>% leaflet.minicharts::clearMinicharts()

        req(nrow(df_map) > 0)

        if (isTruthy("Circles" %in% isolate(input$map_groups)) | minicharts_init()) {
          chart_data <- df_map %>%
            dplyr::select(-dplyr::any_of(c(
              rv$join_cols,
              rv$geo_pop_var,
              "attack_rate",
              "name",
              "lon",
              "lat",
              "total"
            )))

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
              labelStyle = htmltools::css(font_family = "'Roboto Mono',sans-serif"),
              type = "pie",
              width = pie_width
            )

          minicharts_init(FALSE)
        }
      }) %>% bindEvent(df_geo_counts(), input$circle_size_mult)

      # show/hide circles when selected/unselected from map groups
      observeEvent(input$map_groups, {
        if (!"Circles" %in% input$map_groups) {
          leaflet::leafletProxy("map", session) %>%
            leaflet.minicharts::clearMinicharts()
          minicharts_on(FALSE)
        } else if (!minicharts_on()) {
          df_map <- sf::st_drop_geometry(df_geo_counts())
          req(nrow(df_map) > 0)
          chart_data <- df_map %>%
            dplyr::select(-dplyr::any_of(c(
              rv$join_cols,
              rv$geo_pop_var,
              "attack_rate",
              "name",
              "lon",
              "lat",
              "total"
            )))
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
              labelStyle = htmltools::css(font_family = "'Roboto Mono',sans-serif"),
              type = "pie",
              width = pie_width
            )
          minicharts_on(TRUE)
        }
      })

      # Missing data information ==================================================
      missing_text <- reactive({
        df_missing <- df_mod() %>%
          dplyr::summarise(
            N = dplyr::n(),
            n = sum(is.na(.data[[rv$geo_col]]))
          ) %>%
          dplyr::mutate(percent = .data$n / .data$N)

        if (df_missing$n == 0) {
          return(NULL)
        } else {
          n_missing <- glue::glue("{scales::number(df_missing$n)} ({scales::percent(df_missing$percent, accuracy = 1)})")
          glue::glue("{rv$geo_level_name} data missing/unknown for {n_missing} patients")
        }
      })

      output$footer <- renderUI({
        req(missing_text())
        tags$span(
          HTML('<i class="fa fa-exclamation-triangle" style="color:red;"></i>'),
          missing_text()
        )
      })

      # Map image export ==========================================================
      output$dl <- downloadHandler(
        filename = function() {
          glue::glue("EPI-MAP-{time_stamp()}.png")
        },
        content = function(file) {
          # show loading spinner
          w_map$show()
          on.exit(w_map$hide())

          # check for chrome browser before attempting mapview::mapshot2
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

          showNotification(
            "Generating map export. This can take up to a minute...",
            type = "default",
            duration = 8
          )

          # rebuild current map shown on dashboard
          boundaries <- rv$sf
          df_map <- df_geo_counts()
          chart_data <- sf::st_drop_geometry(df_map) %>% 
            dplyr::select(-dplyr::any_of(c(
              rv$join_cols,
              rv$geo_pop_var,
              "attack_rate",
              "name",
              "lon",
              "lat",
              "total"
            )))

          # * 7 instead of * 10 like in the app map because
          # circles are coming out larger in the image export
          pie_width <- (input$circle_size_mult * 7) * (sqrt(df_map$total) / sqrt(max(df_map$total)))

          missing_data_text <- missing_text()
          if (!is.null(missing_data_text)) {
            missing_data_text <- glue::glue("<b>Missing data</b></br>{missing_data_text}")
          } 

          # get the centroid coordinates of current onscreen map view 
          # to set the view in export map
          bbox <- sf::st_bbox(c(
            xmin = input$map_bounds$east,
            xmax = input$map_bounds$west,
            ymax = input$map_bounds$north,
            ymin = input$map_bounds$south
          ), crs = sf::st_crs(4326))
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
              html = tags$b(rv$map_var_lab),
              position = "topright"
            ) %>%
            leaflet::addScaleBar(
              position = "bottomright",
              options = leaflet::scaleBarOptions(imperial = FALSE)
            ) %>%
            leaflet::addControl(
              html = shiny::HTML(
                glue::glue_collapse(c(missing_data_text, filter_info()), sep = "</br>")
              ),
              className = "leaflet-control-attribution",
              position = "bottomleft"
            ) %>%
            leaflet::addPolygons(
              data = boundaries,
              stroke = TRUE,
              color = "grey",
              weight = 1,
              fillOpacity = 0,
              label = boundaries[[rv$geo_name_col]],
              group = "Boundaries",
              options = leaflet::pathOptions(pane = "boundaries")
            ) %>%
            leaflet.minicharts::addMinicharts(
              lng = df_map$lon,
              lat = df_map$lat,
              layerId = df_map$name,
              chartdata = chart_data,
              opacity = .8,
              fillColor = epi_pals()$d310[1],
              colorPalette = epi_pals()$d310,
              legend = TRUE,
              showLabels = TRUE,
              labelStyle = htmltools::css(font_family = "'Roboto Mono',sans-serif"),
              type = "pie",
              width = pie_width
            )
          
          if (!is.null(rv$geo_pop_var)) {
            pal <- leaflet::colorBin(
              palette = choro_pal,
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
                layerId = ~pcode,
                group = "Attack rate",
                options = leaflet::pathOptions(pane = "choropleth")
              ) %>%
              leaflet::addLegend(
                title = choro_lab,
                data = df_map,
                pal = pal,
                values = ~attack_rate,
                opacity = choro_opacity,
                position = "bottomright",
                group = "Attack rate",
                layerId = "attack_legend"
              )
          }

          # if not first admin level, map borders of parent admin levels
          if (show_parent_borders) {
            gd <- force_reactive(geo_data)
            geo_level <- which(names(gd) == isolate(input$geo_level))
            if (geo_level > 1) {
              lower_levels <- 1:(geo_level - 1)
              for (i in lower_levels) {
                stroke_width <- (geo_level - i) + 1
                borders <- sf::st_filter(gd[[i]]$sf, boundaries)
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

          mapview::mapshot2(
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
            vwidth = export_width,
            vheight = export_height,
            zoom = 2,
            delay = 0.5
          )
        }
      )

      # return region select click information to main app
      shiny::reactive({
        list(
          region_select = region_select(),
          geo_col = rv$geo_col,
          level_name = rv$geo_level_name,
          region_name = rv$region_select_name
        )
      })
    }
  )
}

#' @noRd
validate_geo_data <- function(geo_data) {
  gdv <- all(
    # check geo_data is a list of named lists
    rlang::is_list(geo_data),
    rlang::is_named(geo_data),
    all(purrr::map_lgl(geo_data, rlang::is_list)),
    # check each item
    purrr::map_lgl(geo_data, validate_geo_data_item)
  )
  if (gdv) {
    geo_data
  } else {
    cli::cli_abort(c(
      "x" = "`geo_data` does not meet the required specification.
      It must be a list of named lists each containing the following named items:",
      ">" = "`sf`: geographical data of class 'sf' (simple features)",
      ">" = "`name_var`: character string of the variable name in `sf` containing the names of each geographical feature",
      ">" = "`join_by`: named character vector of variable names to use when joining to linelist data.
      For example `c(''pcode' = 'adm1_pcode')` where 'pcode' is a variable in `sf` to be joined with 'adm1_pcode'
      a variable in the linelist data.",
      " " = "Other optional items include:",
      ">" = "`layer_name`: the name of the geographical boundary, for example 'State', 'Department' etc",
      ">" = "`pop_var`: character string of the variable name in `sf` containing population data for each feature.
      If provided, attack rates will be shown on the map as a choropleth",
      "See ?epishiny::place_server for examples"
    ))
  }
}

#' @noRd 
validate_geo_data_item <- function(gd) {
  all(
    # is an sf object
    "sf" %in% class(gd$sf),
    # name var is a character length 1
    rlang::is_character(gd$name_var),
    length(gd$name_var) == 1,
    gd$name_var %in% colnames(gd$sf),
    # join_by is a named character vector
    rlang::is_character(gd$join_by),
    rlang::is_named(gd$join_by),
    # join cols are present in sf data
    all(names(gd$join_by) %in% colnames(gd$sf))
  )
}

#' @noRd 
add_coords <- function(sf) {
  if (all(c("lon", "lat") %in% colnames(sf))) {
    sf
  } else {
    coords <- sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(sf::st_zm(sf))))
    sf %>% dplyr::mutate(lon = coords[, 1], lat = coords[, 2], .before = dplyr::last_col())
  }
}