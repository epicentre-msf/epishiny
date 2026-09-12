#' Place module
#'
#' Visualise geographical distribution across multiple administrative boundaries on an interactive leaflet map.
#'
#' @rdname place
#'
#' @param id Module id. Must be the same in both the UI and server function to link the two.
#' @param geo_data An epishiny geo layer object or a list of epishiny geo layer objects created with [geo_layer()].
#'   Each layer will be available as a selectable option in the map options to visualise data across different administrative levels.
#'   See `?geo_layer` for details on how to set up your geo data.
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
#' @param choro_pal_default RColorBrewer or viridis palette name to be used as the default option of the choropleth layer palette input.
#' @param full_screen Add button to card to with the option to enter full screen mode?
#' @param use_sidebar Logical. If TRUE, displays options in a sidebar instead of popover button. Default TRUE.
#' @param sidebar_title String. Title for the sidebar. Only used if use_sidebar = TRUE. Default "Map options".
#' @param sidebar_width Numeric. Width of sidebar in pixels. Only used if use_sidebar = TRUE. Default 280.
#'
#' @return A [bslib::card] UI element with options (in popover or sidebar) and a leaflet map.
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
  choro_pal_default = "Reds",
  full_screen = TRUE,
  use_sidebar = TRUE,
  sidebar_title = NULL,
  sidebar_width = 250
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

  inputs_ui <- place_options_ui(
    ns = ns,
    count_vars = count_vars,
    group_vars = group_vars,
    no_grouping_lab = no_grouping_lab,
    count_vars_lab = count_vars_lab,
    groups_lab = groups_lab,
    circle_size_lab = circle_size_lab,
    pal_default = choro_pal_default
  )

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
        # tooltip if provided
        tt,
        # only show download button if chrome available
        if (!is.null(chromote::find_chrome())) {
          downloadLink(
            ns("dl"),
            label = bsicons::bs_icon("download", class = "text-primary", size = "1.2em"),
            title = download_lab,
            class = "ms-2"
          ) |>
            bslib::tooltip(download_lab)
        },
        # options button - popover or sidebar toggle
        if (!use_sidebar) {
          # Popover mode
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
            inputs_ui
          )
        } else {
          # Sidebar mode - gear icon toggles sidebar
          actionLink(
            ns("toggle_sidebar"),
            label = bsicons::bs_icon("gear", size = "1.2em"),
            class = "ms-2 text-primary"
          ) |>
            bslib::tooltip(opts_btn_lab)
        }
      ),
      # Conditional card body based on layout mode
      if (use_sidebar) {
        # Sidebar layout
        bslib::card_body(
          padding = 0,
          bslib::layout_sidebar(
            padding = 0,
            gap = 0,
            sidebar = bslib::sidebar(
              id = ns("place_sidebar"),
              title = sidebar_title,
              width = sidebar_width,
              position = "right",
              open = "closed",
              inputs_ui
            ),
            leaflet::leafletOutput(ns("map"))
          )
        )
      } else {
        # Popover layout - just map in body
        bslib::card_body(
          padding = 0,
          leaflet::leafletOutput(ns("map"))
        )
      },
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
#' @param choro_lab_rate Label for attack rate choropleth (only used if `geo_data` contains population data).
#' @param choro_opacity Opacity of choropleth colour.
#' @param base_maps A character vector of base map providers to include in the map. See [leaflet::providers] for available options.
#' @param export_width The width of the exported map image.
#' @param export_height The height of the exported map image.
#' @param filter_info If contained within an app using [filter_server()], supply the `filter_info` object
#'   returned by that function here to add filter information to chart exports.
#' @param filter_reset If contained within an app using [filter_server()], supply the `filter_reset` object
#'   returned by that function here to reset any click event filters that have been set from by module.
#' @param time_filter supply the output of [time_server()] here to filter
#'   the data by click events on the time module bar chart (clicking a bar
#'   will filter the data to the period the bar represents)
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
  choro_lab_rate = "Rate /100 000",
  choro_opacity = .7,
  base_maps = c("Esri.WorldGrayCanvas", "OpenStreetMap", "OpenStreetMap.HOT"),
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

      # determine once whether data is pre-aggregated
      is_agg <- length(count_vars) > 0

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

      # Toggle sidebar when gear icon clicked
      observeEvent(input$toggle_sidebar, {
        bslib::sidebar_toggle("place_sidebar")
      })

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
      # observe({
      #   req(geo_select())
      #   if (is.null(geo_select()$pop_var)) {
      #     bslib::update_tooltip("tt-choro", "Population data required for choropleth layer")
      #   } else {
      #     bslib::update_tooltip("tt-choro", "Show/Hide layer")
      #   }
      # }) |>
      #   bindEvent(input$popover)

      # run once on launch to set choropleth switch value based on
      # population data availability in default geo layer
      observe({
        req(geo_select())
        has_pop <- !is.null(geo_select()$pop_var)
        if (has_pop) {
          bslib::update_switch("choro_active", value = TRUE)
        } else {
          bslib::update_switch("choro_active", value = FALSE)
        }
      }) |>
        bindEvent(geo_select(), once = TRUE)

      # disable choropleth layer if geo_select has no polygons
      observe({
        n_polys <- rv$sf %>%
          dplyr::filter(
            sf::st_is(sf::st_geometry(rv$sf), c("POLYGON", "MULTIPOLYGON"))
          ) |>
          nrow()
        has_polys <- n_polys > 0
        bslib::update_switch("choro_active", value = if (has_polys && input$choro_active) TRUE else FALSE)
        shinyjs::toggleState(id = "choro_active", condition = has_polys)
        bslib::update_tooltip(
          "tt-choro",
          if (has_polys) "Show/Hide layer" else "No polygon geometries available for choropleth layer"
        )
      }) |>
        bindEvent(rv$sf)

      # manage choropleth variable options based on population data availability
      observe({
        req(geo_select())
        has_pop <- !is.null(geo_select()$pop_var)
        shinyjs::toggle(id = "choro_var", condition = has_pop)

        # Update display selector choices and visibility
        if (has_pop) {
          # Population available - show Rates and Counts options
          updateSelectInput(
            session,
            "choro_var",
            choices = c("Rates" = "attack_rate", "Counts" = "total"),
            selected = input$choro_var %||% "attack_rate"
          )
        } else {
          # No population data - only counts option and hidden
          updateSelectInput(
            session,
            "choro_var",
            choices = c("Counts" = "total"),
            selected = "total"
          )
        }
      }) %>%
        bindEvent(geo_select())

      rv <- reactiveValues()

      # update reactive values whenever inputs change
      observe({
        geo_join <- geo_select()$join_by
        join_cols <- if (rlang::is_named(geo_join)) names(geo_join) else geo_join
        geo_col <- unname(geo_join)
        geo_col_sym <- rlang::sym(geo_col)
        geo_name_col <- geo_select()$name_var
        geo_level_name <- geo_select()$layer_name
        geo_pop_var <- geo_select()$pop_var
        map_var <- input$var %||% "n"
        var_list <- c("n", group_vars)
        map_var_lab <- get_label(map_var, var_list)
        count_var <- input$count_var
        n_lab <- get_label(count_var, count_vars)

        # Choropleth layer selections
        choro_indicator <- input$choro_indicator %||% unname(count_vars)[1]
        choro_display <- input$choro_var %||% "attack_rate"

        # Determine which column to visualize in choropleth
        choro_col <- if (choro_display == "attack_rate") {
          if (is_agg && length(count_vars) > 1) paste0("attack_rate_", choro_indicator) else "attack_rate"
        } else {
          if (is_agg) choro_indicator else "total"
        }

        # # Get label for legend
        choro_lab <- if (choro_display == "attack_rate") {
          paste(get_label(choro_indicator, count_vars), choro_lab_rate)
        } else {
          get_label(choro_indicator, count_vars)
        }

        # save as reactive values
        rv$geo_join <- geo_join
        rv$join_cols <- join_cols
        rv$geo_col <- geo_col
        rv$geo_col_sym <- geo_col_sym
        rv$geo_name_col <- geo_name_col
        rv$geo_level_name <- geo_level_name
        rv$geo_pop_var <- geo_pop_var
        rv$map_var <- map_var
        rv$map_var_lab <- map_var_lab
        rv$count_var <- count_var
        rv$n_lab <- n_lab
        rv$choro_indicator <- choro_indicator
        rv$choro_display <- choro_display
        rv$choro_col <- choro_col
        rv$choro_lab <- choro_lab
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
        leaf_basemap(bbox, baseMaps = base_maps, miniMap = TRUE)
      })

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

      # join data to boundaries and compute attack rates
      df_geo_counts <- reactive({
        prepare_geo_data(
          df = df_mod(),
          sf = rv$sf,
          geo_var = rv$geo_col,
          geo_join = rv$geo_join,
          join_cols = rv$join_cols,
          geo_name_col = rv$geo_name_col,
          geo_pop_var = rv$geo_pop_var,
          count_vars = if (is_agg) count_vars else NULL
        )
      }) %>%
        bindEvent(df_mod(), rv$sf)

      df_map_circles <- reactive({
        get_map_circle_df(
          df_raw = df_mod(),
          df_geo = df_geo_counts(),
          geo_var = rv$geo_col,
          geo_join = rv$geo_join,
          count_var = if (is_agg) rv$count_var else NULL,
          group_var = if (rv$map_var != "n") rv$map_var else NULL
        )
      }) %>%
        bindEvent(df_geo_counts(), rv$map_var, rv$count_var)

      # add polygon boundaries with tooltip data info
      observe({
        req(df_geo_counts())

        # Determine which count and attack rate columns to show in tooltip
        choro_indicator <- rv$choro_indicator
        if (is_agg && !is.null(choro_indicator)) {
          tt_n_col <- choro_indicator
          tt_n_lab <- get_label(choro_indicator, count_vars)
          tt_ar_col <- if (length(count_vars) > 1) {
            paste0("attack_rate_", choro_indicator)
          } else {
            "attack_rate"
          }
        } else {
          tt_n_col <- "total"
          tt_n_lab <- rv$n_lab
          tt_ar_col <- "attack_rate"
        }

        map_proxy <- leaflet::leafletProxy("map", session) %>%
          leaflet::clearGroup("Boundaries") %>%
          leaflet::clearControls()

        # Add parent borders if enabled
        if (show_parent_borders) {
          geo_level <- which(purrr::map_chr(geo_data, "layer_name") == isolate(input$geo_level))
          map_proxy <- add_parent_borders(map_proxy, geo_data, geo_level, df_geo_counts())
        }

        # Add boundary polygons using helper function
        add_map_boundaries(
          map_proxy,
          df_geo_counts(),
          rv$geo_name_col,
          rv$join_cols,
          n_lab = tt_n_lab,
          geo_pop_var = rv$geo_pop_var,
          n_col = tt_n_col,
          ar_col = tt_ar_col
        )
      }) %>%
        bindEvent(df_geo_counts(), rv$choro_indicator)

      # add/update Choropleth polygons when df_geo_counts() changes
      observe({
        req(df_geo_counts())

        map_proxy <- leaflet::leafletProxy("map", session) %>%
          leaflet::clearGroup("Choropleth") %>%
          leaflet::removeControl(layerId = "attack_legend")

        if (input$choro_active) {
          # Create choropleth settings object
          choro_settings <- list(
            variable = rv$choro_col,
            palette = input$choro_pal %||% "Reds",
            reverse_palette = input$choro_pal_rev %||% FALSE,
            breaks_method = input$choro_breaks %||% "quantile",
            n_breaks = input$choro_nbreaks %||% 5,
            opacity = choro_opacity,
            legend_position = "bottomright"
          )

          add_choropleth_layer(map_proxy, df_geo_counts(), choro_settings, rv$choro_lab)
        }
      }) %>%
        bindEvent(
          df_geo_counts(),
          rv$choro_indicator,
          rv$choro_display,
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

        map_proxy <- leaflet::leafletProxy("map", session) %>%
          leaflet.minicharts::clearMinicharts()

        if (input$symbols_active | minicharts_init()) {
          # Create symbols settings object
          symbols_settings <- list(
            size_multiplier = input$circle_size_mult %||% 6,
            base_multiplier = 10, # 10 for interactive map
            opacity = 0.7,
            color_palette = epi_pals()$d310,
            show_legend = TRUE,
            show_labels = TRUE
          )

          add_symbols_layer(map_proxy, df_map_circles(), symbols_settings)
          minicharts_init(FALSE)
        }
      }) %>%
        bindEvent(df_map_circles(), input$circle_size_mult)

      # show/hide circles when selected/unselected from map groups
      observeEvent(input$symbols_active, {
        if (!input$symbols_active) {
          leaflet::leafletProxy("map", session) %>%
            leaflet.minicharts::clearMinicharts()
          minicharts_on(FALSE)
        } else if (!minicharts_on()) {
          symbols_settings <- list(
            size_multiplier = input$circle_size_mult %||% 6,
            base_multiplier = 10, # 10 for interactive map
            opacity = 0.7,
            color_palette = epi_pals()$d310,
            show_legend = TRUE,
            show_labels = TRUE
          )
          leaflet::leafletProxy("map", session) |>
            add_symbols_layer(df_map_circles(), symbols_settings)
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

          # Add boundaries using helper function
          boundaries <- rv$sf
          choro_indicator <- rv$choro_indicator
          if (is_agg && !is.null(choro_indicator)) {
            exp_n_col <- choro_indicator
            exp_n_lab <- get_label(choro_indicator, count_vars)
            exp_ar_col <- if (length(count_vars) > 1) {
              paste0("attack_rate_", choro_indicator)
            } else {
              "attack_rate"
            }
          } else {
            exp_n_col <- "total"
            exp_n_lab <- rv$n_lab
            exp_ar_col <- "attack_rate"
          }
          leaf_out <- add_map_boundaries(
            leaf_out,
            df_geo_counts(),
            rv$geo_name_col,
            rv$join_cols,
            n_lab = exp_n_lab,
            geo_pop_var = rv$geo_pop_var,
            n_col = exp_n_col,
            ar_col = exp_ar_col
          )

          # Add symbols layer using helper function
          if (input$symbols_active) {
            symbols_settings <- list(
              size_multiplier = input$circle_size_mult %||% 6,
              base_multiplier = 7, # 7 instead of 10 for export (circles appear larger)
              opacity = 0.8, # slightly higher opacity for export
              color_palette = epi_pals()$d310,
              show_legend = TRUE,
              show_labels = TRUE
            )

            leaf_out <- add_symbols_layer(leaf_out, df_map_circles(), symbols_settings)
          }

          # Add choropleth layer using helper function
          if (input$choro_active) {
            choro_settings <- list(
              variable = rv$choro_col,
              palette = input$choro_pal %||% "Reds",
              reverse_palette = input$choro_pal_rev %||% FALSE,
              breaks_method = input$choro_breaks %||% "quantile",
              n_breaks = input$choro_nbreaks %||% 5,
              opacity = choro_opacity,
              legend_position = "bottomright"
            )

            leaf_out <- add_choropleth_layer(
              leaf_out,
              df_geo_counts(),
              choro_settings,
              rv$choro_lab
            )
          }

          # Add parent borders using helper function
          if (show_parent_borders) {
            geo_level <- which(purrr::map_chr(geo_data, "layer_name") == isolate(input$geo_level))
            leaf_out <- add_parent_borders(leaf_out, geo_data, geo_level, boundaries)
          }

          # Add base tiles using helper function
          base_tiles <- input$map_groups[[1]] %||% "Esri.WorldGrayCanvas"
          if (grepl("CartoDB", base_tiles)) {
            leaf_out <- addCartoTiles(leaf_out, style = base_tiles)
          } else {
            leaf_out <- leaf_out %>% leaflet::addProviderTiles(base_tiles)
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

# =============================================================================
# HELPER FUNCTION FOR OPTIONS UI
# =============================================================================

#' Generate place module options UI
#' @noRd
place_options_ui <- function(
  ns,
  count_vars,
  group_vars,
  no_grouping_lab,
  count_vars_lab,
  groups_lab,
  circle_size_lab,
  pal_default = "Reds"
) {
  bslib::accordion(
    open = FALSE,
    multiple = FALSE,
    bslib::accordion_panel(
      value = "choropleth",
      title = "Choropleth Layer",
      bslib::input_switch(
        id = ns("choro_active"),
        label = "Show choropleth layer",
        value = TRUE
      ) |>
        bslib::tooltip(id = ns("tt-choro"), "Show/Hide layer"),
      # Indicator input - only shown if count_vars provided (aggregate data)
      if (length(count_vars) > 0) {
        selectInput(
          ns("choro_indicator"),
          label = "Indicator",
          choices = count_vars,
          selected = unname(count_vars)[1],
          multiple = FALSE,
          selectize = FALSE
        )
      },
      selectInput(
        ns("choro_var"),
        label = "Display",
        choices = c("Rates" = "attack_rate", "Counts" = "total"),
        selected = "attack_rate",
        multiple = FALSE,
        selectize = FALSE
      ),
      selectInput(
        ns("choro_pal"),
        label = "Palette",
        choices = choro_pals(),
        selected = pal_default,
        multiple = FALSE,
        selectize = FALSE
      ),
      bslib::input_switch(
        id = ns("choro_pal_rev"),
        label = "Reverse palette",
        value = FALSE
      ),
      tags$div(
        class = "d-flex justify-content-start align-items-start",
        selectInput(
          ns("choro_breaks"),
          label = "Breaks Method",
          choices = choro_breaks(),
          selected = "quantile",
          multiple = FALSE,
          selectize = FALSE
        ),
        numericInput(
          inputId = ns("choro_nbreaks"),
          label = "N Breaks",
          value = 5,
          min = 2,
          max = 10,
          step = 1,
          width = "100px"
        )
      )
    ),
    bslib::accordion_panel(
      value = "symbols",
      title = "Symbols Layer",
      bslib::input_switch(
        id = ns("symbols_active"),
        label = "Show symbols layer",
        value = TRUE
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
    )
  )
}

# =============================================================================
# HELPER FUNCTIONS FOR MAP LAYER RENDERING
# =============================================================================

#' Add boundary polygons to leaflet map
#' @noRd
add_map_boundaries <- function(
  map,
  boundaries,
  geo_name_col,
  join_cols,
  n_lab,
  geo_pop_var,
  n_col = "total",
  ar_col = "attack_rate"
) {
  if (nrow(boundaries) == 0) {
    return(map)
  }

  # Filter to polygon geometries only
  boundaries <- boundaries %>%
    dplyr::filter(sf::st_is(sf::st_geometry(boundaries), c("POLYGON", "MULTIPOLYGON")))

  if (nrow(boundaries) == 0) {
    return(map)
  }

  # Create tooltip hover labels
  tt <- make_leaf_tooltip(
    boundaries,
    n_col = n_col,
    n_lab = n_lab,
    pop_col = geo_pop_var,
    ar_col = ar_col
  )

  map %>%
    leaflet::addPolygons(
      data = boundaries,
      layerId = boundaries[[join_cols]],
      stroke = TRUE,
      color = "grey",
      weight = 1,
      fillOpacity = 0,
      label = tt,
      group = "Boundaries",
      highlightOptions = leaflet::highlightOptions(bringToFront = TRUE, weight = 3),
      options = leaflet::pathOptions(pane = "boundaries")
    )
}

#' Add choropleth layer to leaflet map
#' @noRd
add_choropleth_layer <- function(map, df_map, choro_settings, lab) {
  # n_lab, rate_lab
  if (nrow(df_map) == 0) {
    return(map)
  }

  # Filter to polygons with data
  df_map <- df_map %>%
    dplyr::filter(
      sf::st_is(sf::st_geometry(df_map), c("POLYGON", "MULTIPOLYGON")),
      .data$total > 0
    )

  if (nrow(df_map) == 0) {
    return(map)
  }

  # Get choropleth values
  choro_values <- df_map[[choro_settings$variable]]
  if (all(is.na(choro_values))) {
    return(map)
  }

  # Calculate breaks
  safe_breaks <- purrr::safely(classInt::classIntervals, otherwise = 4)
  bins <- suppressWarnings(safe_breaks(
    var = choro_values,
    n = choro_settings$n_breaks,
    style = choro_settings$breaks_method
  ))

  if (!is.null(bins$error)) {
    shiny::showNotification(
      stringr::str_glue("{choro_settings$breaks_method} breaks could not be calculated for this data. Reverting to default breaks."),
      type = "error"
    )
  } else {
    brks <- unique(bins$result$brks)
    # some break styles (e.g. "sd") can produce breaks below the data minimum,
    # even negative values, which is invalid for case counts / attack rates.
    # clamp breaks to the data range so they start at the minimum observed value.
    rng <- range(choro_values, na.rm = TRUE)
    brks <- brks[brks > rng[1] & brks < rng[2]]
    bins$result <- sort(unique(c(rng[1], brks, rng[2])))
  }

  # Create color palette
  pal <- leaflet::colorBin(
    palette = choro_settings$palette,
    domain = choro_values,
    bins = bins$result,
    reverse = choro_settings$reverse_palette,
    na.color = "transparent"
  )

  # Dynamic legend title
  # legend_title <- switch(
  #   choro_settings$variable,
  #   "total" = n_lab,
  #   "attack_rate" = paste(n_lab, rate_lab),
  #   choro_settings$variable
  # )

  map %>%
    leaflet::addPolygons(
      data = df_map,
      stroke = TRUE,
      color = "grey",
      weight = 1,
      fillColor = ~ pal(get(choro_settings$variable)),
      fillOpacity = choro_settings$opacity,
      highlightOptions = leaflet::highlightOptions(bringToFront = TRUE, weight = 3),
      group = "Choropleth",
      options = leaflet::pathOptions(pane = "choropleth")
    ) %>%
    leaflet::addLegend(
      title = lab,
      data = df_map,
      pal = pal,
      values = stats::as.formula(paste0("~", choro_settings$variable)),
      opacity = choro_settings$opacity,
      position = choro_settings$legend_position,
      group = "Choropleth",
      layerId = "attack_legend"
    )
}

#' Add symbols/circles layer to leaflet map
#' @noRd
add_symbols_layer <- function(map, df_circles, symbols_settings) {
  if (nrow(df_circles) == 0) {
    return(map)
  }

  # Prepare chart data from chart_cols attribute
  chart_cols <- attr(df_circles, "chart_cols")
  chart_data <- df_circles[, chart_cols, drop = FALSE]

  # Calculate symbol sizes
  # Use different multiplier for export vs interactive (7 vs 10)
  base_multiplier <- symbols_settings$base_multiplier %||% 10
  pie_width <- (symbols_settings$size_multiplier * base_multiplier) *
    (sqrt(df_circles$total) / sqrt(max(df_circles$total, na.rm = TRUE)))

  map %>%
    leaflet.minicharts::addMinicharts(
      lng = df_circles$lon,
      lat = df_circles$lat,
      layerId = df_circles$name,
      chartdata = chart_data,
      opacity = symbols_settings$opacity,
      fillColor = symbols_settings$color_palette[1],
      colorPalette = symbols_settings$color_palette,
      legend = symbols_settings$show_legend,
      showLabels = symbols_settings$show_labels,
      type = "pie",
      width = pie_width
    )
}

#' Add parent administrative borders to leaflet map
#' @noRd
add_parent_borders <- function(map, geo_data, current_level, boundaries) {
  if (current_level <= 1) {
    return(map)
  }

  lower_levels <- 1:(current_level - 1)

  for (i in lower_levels) {
    stroke_width <- (current_level - i) + 1
    borders <- suppressMessages(sf::st_filter(geo_data[[i]]$sf, boundaries))
    borders <- dplyr::filter(
      borders,
      sf::st_is(sf::st_geometry(borders), c("POLYGON", "MULTIPOLYGON"))
    )
    if (nrow(borders) > 0) {
      map <- map %>%
        leaflet::addPolylines(
          data = borders,
          group = "Boundaries",
          color = "grey",
          weight = stroke_width,
          options = leaflet::pathOptions(pane = "boundaries")
        )
    }
  }
  map
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
  if (!is.null(pop_var)) {
    check_single_string(pop_var)
  }
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
  geo_var,
  count_vars = NULL
) {
  if (length(count_vars) > 0) {
    # Aggregated data: sum all count_vars by geography
    count_col_names <- unname(count_vars)
    result <- df %>%
      dplyr::summarise(
        .by = dplyr::all_of(geo_var),
        dplyr::across(dplyr::all_of(count_col_names), ~ sum(.x, na.rm = TRUE))
      )
    # 'total' = first count_var, used for circle sizing and default choropleth
    result$total <- result[[count_col_names[1]]]
  } else {
    # Linelist: count rows per geography
    result <- dplyr::count(df, .data[[geo_var]], name = "total")
  }
  result
}

#' Aggregate data, join to spatial boundaries, and compute attack rates
#' @noRd
prepare_geo_data <- function(
  df,
  sf,
  geo_var,
  geo_join,
  join_cols,
  geo_name_col,
  geo_pop_var = NULL,
  count_vars = NULL
) {
  df_counts <- get_geo_counts(df, geo_var, count_vars)

  df_out <- sf %>%
    dplyr::mutate(name = .data[[geo_name_col]]) %>%
    dplyr::select(dplyr::any_of(c(join_cols, geo_pop_var, "name", "lon", "lat"))) %>%
    dplyr::left_join(df_counts, by = geo_join) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.double))

  # compute attack rates if population data is available
  if (!is.null(geo_pop_var)) {
    if (length(count_vars) > 0) {
      # Aggregated: attack rate for each count_var
      count_col_names <- unname(count_vars)
      df_out <- df_out |>
        dplyr::mutate(
          dplyr::across(
            .cols = dplyr::all_of(count_col_names),
            .fns = ~ dplyr::na_if((.x / .data[[geo_pop_var]]) * 1e5, 0),
            .names = "attack_rate_{.col}"
          )
        )
      # default attack_rate column from first count_var
      df_out$attack_rate <- df_out[[paste0("attack_rate_", count_col_names[1])]]
    } else {
      # Linelist: single attack_rate column
      df_out <- df_out %>%
        dplyr::mutate(
          attack_rate = dplyr::na_if((.data$total / .data[[geo_pop_var]]) * 1e5, 0)
        )
    }
  }

  df_out
}

#' @noRd
get_map_circle_df <- function(
  df_raw,
  df_geo,
  geo_var,
  geo_join,
  count_var = NULL,
  group_var = NULL
) {
  is_agg <- !is.null(count_var)
  is_grouped <- !is.null(group_var)

  # drop geometry and attack rate columns (not needed for circles)
  df_geo <- df_geo %>%
    sf::st_drop_geometry() %>%
    dplyr::select(-dplyr::contains("attack_rate"), -dplyr::any_of("attack_rate"))

  if (!is_grouped) {
    df <- df_geo
    if (is_agg) {
      df <- df %>% dplyr::mutate(n = .data[[count_var]])
    } else {
      df <- df %>% dplyr::mutate(n = .data$total)
    }
    chart_cols <- "n"
  } else {
    if (is_agg) {
      df_grouped <- dplyr::count(df_raw, .data[[geo_var]], .data[[group_var]], wt = .data[[count_var]])
    } else {
      df_grouped <- dplyr::count(df_raw, .data[[geo_var]], .data[[group_var]])
    }

    df_pivoted <- df_grouped %>%
      tidyr::pivot_wider(names_from = dplyr::all_of(group_var), values_from = "n")

    chart_cols <- setdiff(names(df_pivoted), geo_var)

    # drop count_var column before join to avoid conflict with group columns
    if (is_agg) {
      df_geo <- df_geo %>% dplyr::select(-dplyr::any_of(count_var))
    }

    df <- df_geo %>%
      dplyr::left_join(df_pivoted, by = geo_join) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.double)) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.double), ~ dplyr::if_else(is.na(.x), 0, .x)))
  }

  df <- df %>% dplyr::filter(.data$total > 0)
  attr(df, "chart_cols") <- chart_cols
  df
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
  if (avl_url) {
    url <- normalizePath(url, mustWork = FALSE)
  }
  if (avl_file) {
    file <- normalizePath(file, mustWork = FALSE)
  }

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
  if (length(idx) > 0) {
    map$x$calls[idx] <- NULL
  }
  return(map)
}

#' @noRd
removeScalebar <- function(map) {
  idx <- getCallEntryFromMap(map, "addScaleBar")
  if (length(idx) > 0) {
    map$x$calls[idx] <- NULL
  }
  return(map)
}

#' @noRd
removeDrawToolbar <- function(map) {
  idx <- getCallEntryFromMap(map, "addDrawToolbar")
  if (length(idx) > 0) {
    map$x$calls[idx] <- NULL
  }
  return(map)
}

#' @noRd
removeEasyButton <- function(map) {
  idx <- getCallEntryFromMap(map, "addEasyButton")
  if (length(idx) > 0) {
    map$x$calls[idx] <- NULL
  }
  return(map)
}

#' @noRd
removeControl <- function(map) {
  idx <- getCallEntryFromMap(map, "addControl")
  if (length(idx) > 0) {
    map$x$calls[idx] <- NULL
  }
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

# =============================================================================
# PLACE MODULE HELPER FUNCTIONS
# =============================================================================

#' Get choropleth color palette names
#' @noRd
choro_pals <- function() {
  list(
    `ColorBrewer Diverging` = c(
      "BrBG",
      "PiYG",
      "PRGn",
      "PuOr",
      "RdBu",
      "RdGy",
      "RdYlBu",
      "RdYlGn",
      "Spectral"
    ),
    `ColorBrewer Sequential` = c(
      "Blues",
      "BuGn",
      "BuPu",
      "GnBu",
      "Greens",
      "Greys",
      "Oranges",
      "OrRd",
      "PuBu",
      "PuBuGn",
      "PuRd",
      "Purples",
      "RdPu",
      "Reds",
      "YlGn",
      "YlGnBu",
      "YlOrBr",
      "YlOrRd"
    ),
    Viridis = c(
      "magma",
      "inferno",
      "plasma",
      "viridis",
      "cividis",
      "rocket",
      "mako",
      "turbo"
    )
  )
}

#' Get choropleth break methods
#' @noRd
choro_breaks <- function() {
  c(
    "fixed",
    "sd",
    "equal",
    "pretty",
    "quantile",
    "kmeans",
    "hclust",
    "bclust",
    "fisher",
    "jenks",
    "dpih",
    "q6",
    "Q6",
    "geom",
    "arith",
    "em",
    "msd",
    "ckmeans"
  )
}

#' Create leaflet basemap with standard layers
#' @noRd
leaf_basemap <- function(
  bbox,
  baseMaps = c("Esri.WorldGrayCanvas", "OpenStreetMap", "OpenStreetMap.HOT"),
  overlayGroups = character(0),
  miniMap = TRUE
) {
  lf <- leaflet::leaflet() %>%
    leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) %>%
    leaflet::addMapPane(name = "choropleth", zIndex = 310) %>%
    leaflet::addMapPane(name = "place_labels", zIndex = 320) %>%
    leaflet::addMapPane(name = "circles", zIndex = 410) %>%
    leaflet::addMapPane(name = "boundaries", zIndex = 420) %>%
    leaflet::addMapPane(name = "geo_highlight", zIndex = 430) %>%
    leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = FALSE))

  for (tile in baseMaps) {
    if (grepl("CartoDB", tile)) {
      lf <- addCartoTiles(lf, style = tile)
    } else {
      lf <- lf %>% leaflet::addProviderTiles(tile, group = tile)
    }
  }

  if (length(baseMaps) > 1) {
    lf <- lf %>%
      leaflet::addLayersControl(
        baseGroups = baseMaps,
        overlayGroups = overlayGroups,
        position = "topleft"
      )
  }

  if (miniMap) {
    lf <- lf %>% leaflet::addMiniMap(toggleDisplay = TRUE, position = "bottomleft")
  }

  return(lf)
}

addCartoTiles <- function(map, style = "CartoDB.Positron") {
  # Map basemap names to CARTO tile URLs
  basemap_urls <- list(
    "CartoDB.Positron" = c(
      "https://basemaps.cartocdn.com/rastertiles/light_nolabels/{z}/{x}/{y}.png",
      "https://basemaps.cartocdn.com/rastertiles/light_only_labels/{z}/{x}/{y}.png"
    ),
    "CartoDB.DarkMatter" = c(
      "https://basemaps.cartocdn.com/rastertiles/dark_nolabels/{z}/{x}/{y}.png",
      "https://basemaps.cartocdn.com/rastertiles/dark_only_labels/{z}/{x}/{y}.png"
    ),
    "CartoDB.Voyager" = c(
      "https://basemaps.cartocdn.com/rastertiles/voyager_nolabels/{z}/{x}/{y}.png",
      "https://basemaps.cartocdn.com/rastertiles/voyager_only_labels/{z}/{x}/{y}.png"
    )
  )

  # Validate tile type
  if (!style %in% names(basemap_urls)) {
    cli::cli_abort(
      c(
        "x" = "Invalid CARTO basemap style: {.code {style}}",
        "i" = "Available styles: {.code {paste(names(basemap_urls), collapse = ', ')}}"
      )
    )
  }
  # Get the URL for the requested basemap
  url <- basemap_urls[[style]]

  # Append API key if available
  api_key <- Sys.getenv("CARTO_API_KEY")
  if (nzchar(api_key)) {
    url <- paste0(url, "?key=", api_key)
  } else {
    rlang::warn(
      paste(
        "CARTO API key not detected. Visit https://carto.com/basemaps/apikey/ to obtain a free API key,",
        "then set the CARTO_API_KEY environment variable to render CARTO tiles without the watermark."
      ),
      .frequency = "once",
      .frequency_id = "carto_api_key"
    )
  }

  # Standard CARTO attribution
  attribution <- paste(
    '&copy; <a href="https://carto.com/attributions">CARTO</a>,',
    '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
  )

  # Add tiles to the map and return
  map |>
    # basemap
    leaflet::addTiles(
      urlTemplate = url[1],
      attribution = attribution,
      group = style
    ) |>
    # labels above data layers
    leaflet::addTiles(
      urlTemplate = url[2],
      group = style,
      options = leaflet::leafletOptions(pane = "place_labels")
    )
}

#' Generate HTML Tooltip for Leaflet
#'
#' This function creates an HTML tooltip for leaflet maps.
#' The tooltip displays information about the name, number of patients, population,
#' and attack rate, if available.
#'
#' @param df A data frame containing the data.
#' @param name_col A string specifying the column name for the names (default is "name").
#' @param n_col A string specifying the column name for the counts (default is "total").
#' @param n_lab A string specifying the label for the counts (default is "N patients").
#' @param pop_col A string specifying the column name for the population (default is NULL).
#' @param pop_lab A string specifying the label for the population (default is "Population").
#' @param ar_col A string specifying the column name for the attack rate (default is NULL).
#' @param ar_lab A string specifying the label for the attack rate (default is "Attack rate").
#'
#' @return A list of HTML elements to be used as tooltips in a leaflet map.
#' @importFrom scales number
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom shiny HTML
#' @noRd
#'
#' @examples
#' df <- data.frame(
#'   name = c("Location A", "Location B"),
#'   total = c(100, NA),
#'   population = c(1000, 2000),
#'   attack_rate = c(10, NA)
#' )
#' make_leaf_tooltip(df, pop_col = "population", ar_col = "attack_rate")
make_leaf_tooltip <- function(
  df,
  name_col = "name",
  n_col = "total",
  n_lab = "N patients",
  pop_col = NULL,
  pop_lab = "Population",
  ar_col = NULL,
  ar_lab = "Rate"
) {
  counts <- ifelse(is.na(df[[n_col]]), "No data", scales::number(df[[n_col]], accuracy = 1))
  if (all(!is.null(pop_col), !is.null(ar_col))) {
    pop <- ifelse(is.na(df[[pop_col]]), "No data", scales::number(df[[pop_col]], accuracy = 1))
    ar <- ifelse(is.na(df[[ar_col]]), "No data", scales::number(df[[ar_col]], accuracy = .1))
    glue::glue(
      "<b>{df[[name_col]]}</b><br>
       {n_lab}: <b>{counts}</b><br>
       {pop_lab}: <b>{pop}</b><br>
       {ar_lab}: <b>{ar}</b> / 100 000<br>"
    ) %>%
      purrr::map(shiny::HTML)
  } else {
    glue::glue(
      "<b>{df[[name_col]]}</b><br>
       {n_lab}: <b>{counts}</b><br>"
    ) %>%
      purrr::map(shiny::HTML)
  }
}
