library(shiny)

source("R/05_delays.R")
source("R/get_delay_df.R")
source("R/plot_delay_bar.R")
source("R/plot_delay_timeline.R")

ui <- fluidPage(
  delay_ui(id = "delay_module")
)

server <- function(input, output, session) {
  date_variables <- c(
    "Onset" = "date_onset",
    "Consultation" = "date_consultation",
    "Admission" = "date_admission",
    "Outcome" = "date_outcome"
  )

  linelist <- reactive({
    episimdata::moissala_linelist_clean_EN
  })

  delay_server(id = "delay_module", linelist(), date_vars = date_variables)
}

shinyApp(ui = ui, server = server)
