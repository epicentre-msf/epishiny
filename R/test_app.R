library(shiny)
library(highcharter)
library(tidyverse)

source("R/05_delays.R")
source("R/03_person.R")
source("R/get_delay_df.R")
source("R/plot_delay_bar.R")
source("R/plot_delay_timeline.R")
source("R/utils.R")

ui <- fluidPage(
  delay_ui(id = "delay_module"),
  person_ui(id = "person_module")
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

  group_variables <- c("Sex" = "sex", "Age group" = "age_group")

  delay_server(
    id = "delay_module",
    linelist(),
    date_vars = date_variables,
    group_vars = group_variables
  )

  person_server(
    id = "person_module",
    linelist(),
    sex = "sex",
    male_level = "m",
    female_level = "f",
    age_group_var = "age_group"
  )
}

shinyApp(ui = ui, server = server)
