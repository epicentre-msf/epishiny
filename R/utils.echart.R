# Fonction echart
# --------------------------------------------------------------------------------------

weekLabels <- function(lang = "en") {
  lang <- match.arg(lang, c("en", "fr"), several.ok = FALSE)
  week_lab <- recode(lang, "en" = "W", "fr" = "S")
  htmlwidgets::JS(
    glue::glue("function (value, index) {
          var date = new Date(value);
          var year = date.getWeekYear();
          var week = date.getWeek();
          return year + '-{{week_lab}}' + week;
      }", .open = "{{", .close = "}}")
  )
}

echartDefault <- function(...,
                          dispose = FALSE,
                          reorder = FALSE,
                          grid = TRUE,
                          grid_margin_top = 50,
                          grid_margin_left = 30,
                          grid_margin_bottom = 50,
                          grid_margin_right = 10,
                          grid_margins = c(
                            grid_margin_left,
                            grid_margin_top,
                            grid_margin_right,
                            grid_margin_bottom
                          ),
                          legend = FALSE,
                          legend_orient = 'horizontal',
                          legend_left = 'center',
                          toolbox = TRUE,
                          toolbox_orientation = 'horizontal',
                          toolbox_save = TRUE,
                          toolbox_zoom = FALSE,
                          toolbox_magic_type = FALSE,
                          toolbox_magic_types = list('line', 'bar', 'stack'),
                          renderer = 'canvas') {
  ec <- echarts4r::e_charts(
    renderer = renderer,
    dispose = dispose,
    reorder = reorder,
    ...
  )
  if (grid) {
    ec %<>%
      echarts4r::e_grid(
        left = '10%',
        top = '15%',
        right = '8%',
        bottom = '12%',
        containLabel = FALSE
      ) %>%
      echarts4r::e_x_axis(
        splitLine = splitLine(),
        minorSplitLine = minorSplitLine(),
        axisLine = list(show = F),
        axisTick = list(show = F),
        axisLabel =
          list(fontSize = 10, color = '#666666', showMaxLabel = FALSE),
        type = 'category'
      ) %>%
      echarts4r::e_y_axis(
        allowDecimals = FALSE,
        splitLine = splitLine(),
        minorSplitLine = minorSplitLine(),
        axisLine = list(show = F),
        axisTick = list(show = F),
        axisLabel =
          list(fontSize = 10, color = '#666666', showMaxLabel = FALSE)
      )
  }
  ec %<>%
    echarts4r::e_animation(duration = 100) %>%
    echarts4r::e_legend(
      show = legend
    ) %>%
    echarts4r::e_title(
      textStyle = list(fontSize = 8),
      left = 'center'
    ) %>%
    echarts4r::e_tooltip(
      backgroundColor = '#e1e1decc',
      textStyle = list(fontSize = 10, color = '#333333'),
      trigger = 'axis',
      axisPointer = list(
        type = 'shadow',
        shadowStyle = list(
          opacity = 0.7
        )
      )
    ) %>%
    echarts4r::e_axis_pointer(
      link = list(xAxisIndex = 'all')
    )
  if (toolbox) {
    ec %<>%
      echarts4r::e_toolbox(orient = toolbox_orientation)
    if (toolbox_save) {
      ec %<>%
        echarts4r::e_toolbox_feature(
          feature = 'saveAsImage',
          title = 'Sauver (*.png)',
          pixelRatio = 8,
          excludeComponents = list('toolbox', 'markLine')
        )
    }
    if (toolbox_zoom) {
      ec %<>%
        echarts4r::e_toolbox_feature(
          feature = 'dataZoom',
          title = list(
            zoom = 'Zoom',
            back = 'Restaure zoom'
          ),
          yAxisIndex = 'none'
        ) %>%
        echarts4r::e_toolbox_feature(
          feature = 'restore',
          title = 'Restaurer'
        )
    }
    if (toolbox_magic_type) {
      ec %<>% echarts4r::e_toolbox_feature(feature = 'magicType', type = toolbox_magic_types)
    }
  }
  return(ec)
}

e_x_axis_category <- function(...) {
  echarts4r::e_x_axis(
    splitLine = splitLine(),
    minorSplitLine = minorSplitLine(),
    axisLine = list(show = F),
    axisTick = list(show = F),
    axisLabel =
      list(fontSize = 10, color = '#666666', showMaxLabel = FALSE),
    type = 'category',
    ...
  )
}

e_y_axis_category <- function(...) {
  echarts4r::e_y_axis(
    splitLine = splitLine(),
    minorSplitLine = minorSplitLine(),
    axisLine = list(show = F),
    axisTick = list(show = F),
    axisLabel =
      list(fontSize = 10, color = '#666666', showMaxLabel = FALSE),
    type = 'category',
    ...
  )
}

e_x_axis_month <- function(e, datazoom = FALSE, ...) {
  e %<>% echarts4r::e_x_axis(...,
    boundaryGap = FALSE,
    splitLine = minorSplitLine(),
    axisLabel = list(
      fontSize = 10,
      formatter = htmlwidgets::JS("
        function(value) {
          return moment(value).format('MMM');
        }"),
      color = '#666666'
    ),
    type = 'category'
  )

  e$x$opts$xAxis[[2]] <- e$x$opts$xAxis[[1]]
  e$x$opts$xAxis[[2]]$splitLine <- splitLine()
  e$x$opts$xAxis[[2]]$axisLabel$interval <- htmlwidgets::JS('
    function(index, value) {
      var date = new Date(value);
      return date.getMonth() === 0;
    }')
  e$x$opts$xAxis[[2]]$axisLabel$formatter <- htmlwidgets::JS("
    function(value) {
      return moment(value).format('YYYY');
    }")
  e$x$opts$xAxis[[2]]$position <- 'bottom'
  e$x$opts$xAxis[[2]]$offset <- 15

  if (datazoom) {
    e %<>%
      echarts4r::e_datazoom(
        type = 'slider',
        xAxisIndex = list(0, 1),
        labelFormatter = htmlwidgets::JS("
          function(value, valueStr) {
            return echarts.format.formatTime('yyyy-MM', valueStr);
          }")
      )

    e$x$opts$toolbox <- list(
      e$x$opts$toolbox,
      e$x$opts$toolbox
    )

    e$x$opts$toolbox[[2]]$left <- 0
    e$x$opts$toolbox[[2]]$itemSize <- 16
    e$x$opts$toolbox[[2]]$feature <- list(myZoom = list())

    e$x$opts$toolbox[[2]]$feature$myZoom06$show <- TRUE
    e$x$opts$toolbox[[2]]$feature$myZoom06$option <- list()
    e$x$opts$toolbox[[2]]$feature$myZoom06$title <- 'Last 6 months'
    e$x$opts$toolbox[[2]]$feature$myZoom06$icon <- 'image://assets/icons/calendar-06-22.svg'
    e$x$opts$toolbox[[2]]$feature$myZoom06$onclick <- htmlwidgets::JS("
          function(e){
            console.log_debug(e)
            Shiny.setInputValue('mod_cse-zoom', {month: 6, nonce: Math.random()});
          }")

    e$x$opts$toolbox[[2]]$feature$myZoom12$show <- TRUE
    e$x$opts$toolbox[[2]]$feature$myZoom12$option <- list()
    e$x$opts$toolbox[[2]]$feature$myZoom12$title <- 'Last 12 months'
    e$x$opts$toolbox[[2]]$feature$myZoom12$icon <- 'image://assets/icons/calendar-12-22.svg'
    e$x$opts$toolbox[[2]]$feature$myZoom12$onclick <- htmlwidgets::JS("
          function(e){
            console.log_debug(e)
            Shiny.setInputValue('mod_cse-zoom', {month: 12, nonce: Math.random()});
          }")

    e$x$opts$toolbox[[2]]$feature$myZoom18$show <- TRUE
    e$x$opts$toolbox[[2]]$feature$myZoom18$option <- list()
    e$x$opts$toolbox[[2]]$feature$myZoom18$title <- 'Last 18 months'
    e$x$opts$toolbox[[2]]$feature$myZoom18$icon <- 'image://assets/icons/calendar-18-22.svg'
    e$x$opts$toolbox[[2]]$feature$myZoom18$onclick <- htmlwidgets::JS("
          function(e){
            console.log_debug(e)
            Shiny.setInputValue('mod_cse-zoom', {month: 18, nonce: Math.random()});
          }")


    e$x$opts$toolbox[[2]]$feature$myZoomAll$show <- TRUE
    e$x$opts$toolbox[[2]]$feature$myZoomAll$option <- list()
    e$x$opts$toolbox[[2]]$feature$myZoomAll$title <- 'All datas'
    e$x$opts$toolbox[[2]]$feature$myZoomAll$icon <- 'image://assets/icons/all.svg'
    e$x$opts$toolbox[[2]]$feature$myZoomAll$onclick <- htmlwidgets::JS("
          function(e){
            console.log_debug(e)
            Shiny.setInputValue('mod_cse-zoom', {month: 0, nonce: Math.random()});
          }")

  }

  e
}

e_x_axis_integer <- function(e, ...) {
  e %<>% echarts4r::e_x_axis(
    allowDecimals = FALSE,
    splitLine = splitLine(),
    minorSplitLine = minorSplitLine(),
    axisLine = list(show = F),
    axisTick = list(show = F),
    axisLabel =
      list(fontSize = 10, color = '#666666', showMaxLabel = FALSE),
    type = 'value',
    ...
  )
}

e_x_axis_time <- function(...) {
  echarts4r::e_x_axis(
    boundaryGap = TRUE,
    splitLine = splitLine(),
    minorSplitLine = minorSplitLine(),
    min = as.Date('2015-01-01'),
    # minInterval= 3600 * 1000 * 24 * 28,
    # maxInterval= 3600 * 1000 * 24 * 31,
    axisLabel =
      list(
        fontSize = 10,
        # formatter = htmlwidgets::JS("function(value){return echarts.format.formatTime('yyyy-MM', value);}"),
        color = '#666666'
      ),
    axisTick =
      list(
        alignWithLabel = TRUE
      ),
    type = 'time',
    ...
  )
}

splitLine <- function(interval = 'auto', ...) {
  list(
    show = TRUE,
    alignWithLabel = TRUE,
    interval = interval,
    lineStyle = list(
      color = '#e6e6e6'
    ),
    ...
  )
}

minorSplitLine <- function(...) {
  list(
    show = TRUE,
    lineStyle = list(
      color = '#f2f2f2'
    ),
    ...
  )
}

e_group <- function(ec, series, names, colors) {
  level_n <- length(names)

  for (i in seq_len(level_n)) {
    ec$x$opts$legend$data[[i]] <- names[i]
    ec$x$opts$series[[i]]$name <- names[i]
    ec$x$opts$series[[i]]$itemStyle$color <- colors[i]
  }

  ec$x$opts$legend$selected <- as.list(
    setNames(series %in% names, series)
  )

  ec
}

e_color_values <- function(ec) {
  for (i in 1:length(ec$x$data)) {
    serie_datas <- ec$x$data[[i]]
    for (j in seq_len(nrow(serie_datas))) {
      ec$x$opts$series[[i]]$data[[j]]$itemStyle$color <- serie_datas$color[j]
    }
  }
  ec
}
