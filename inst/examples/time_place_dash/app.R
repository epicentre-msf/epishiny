library(shiny)
library(bslib)
pkgload::load_all()

update_data <- FALSE

if (update_data) {
  library(readr)
  library(rnaturalearth)
  library(dplyr)
  library(sf)

  df_who_covid <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
  df_who_covid_2020 <- df_who_covid %>% filter(Date_reported < as.Date("2021-01-01"))

  world_map <- ne_countries(scale = "small", type = "countries", returnclass = "sf") %>%
    st_transform(crs = 4326) %>%
    select(iso_a2_eh, name, pop_est)
}

# setup the geo layer for epishiny
geo_data <- geo_layer(
  layer_name = "Country",
  sf = world_map,
  name_var = "name",
  pop_var = "pop_est",
  join_by = c("iso_a2_eh" = "Country_code")
)

count_vars <- c("Cases" = "New_cases", "Deaths" = "New_deaths")
date_vars <- "Date_reported"
group_vars <- "WHO_region"

ui <- page_sidebar(
  title = "epishiny covid19 dashboard",
  sidebar = sidebar(
    radioButtons(
      inputId = "indicator",
      label = "Indicator",
      choices = count_vars
    )
  ),
  layout_columns(
    # col_widths = 12,
    time_ui(
      id = "time",
      date_vars = date_vars,
      count_vars = count_vars,
      group_vars = group_vars,
      date_intervals = c("week", "month", "year")
    ),
    place_ui(
      id = "place",
      geo_data = geo_data,
      count_vars = count_vars
    )
  )
)

server <- function(input, output, session) {

  bar_click <- time_server(
    id = "time",
    df = df_who_covid,
    date_vars = date_vars,
    count_vars = count_vars,
    group_vars = group_vars,
    show_ratio = TRUE,
    ratio_lab = "CFR",
    ratio_numer = "New_deaths",
    ratio_denom = "New_cases",
    place_filter = reactive(map_click())
  )

  map_click <- place_server(
    id = "place",
    df = df_who_covid,
    geo_data = geo_data,
    count_vars = count_vars,
    time_filter = reactive(bar_click())
  )

}

if (interactive()) shinyApp(ui, server)
