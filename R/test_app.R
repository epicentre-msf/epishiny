library(shiny)

source("R/05_delays.R")
source("R/get_delay_df.R")
source("R/plot_delay_bar.R")
source("R/plot_delay_timeline.R")


# Define a simple test app
ui <- fluidPage(
  delay_ui(id = "delay_module")
)

server <- function(input, output, session) {
  date_vars <- c(
    "date_onset",
    "date_consultation",
    "date_admission",
    "date_outcome"
  )

  # Sample delay data (replace with actual data)
  linelist <- reactive({
    episimdata::moissala_linelist_clean_EN
  })

  # Call the delay module
  delay_server(id = "delay_module", linelist(), date_vars = date_vars)
}

shinyApp(ui = ui, server = server)
