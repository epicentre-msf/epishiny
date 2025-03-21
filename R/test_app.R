library(shiny)
library(epicontacts)
library(tidyverse)

source("R/06_transmission.R")
source("R/03_person.R")
source("R/plot_transmission.R")
source("R/utils.R")

ui <- bslib::page_fluid(
  use_epishiny(),
  transmission_ui(id = "transmission_module"),
  person_ui(id = "person_module")
)

server <- function(input, output, session) {
  linelist <- reactive({
    episimdata::evd_data$evd_linelist
  })

  contact <- reactive({
    episimdata::evd_data$evd_chain
  })

  epicontacts <-
    reactive({
      epicontacts::make_epicontacts(
        linelist(),
        contacts = contact(),
        id = "id",
        to = "to",
        from = "from",
        directed = TRUE
      )
    })

  person_server(
    id = "person_module",
    linelist(),
    sex = "sex",
    male_level = "male",
    female_level = "female",
    age_var = "age"
  )

  transmission_server(
    id = "transmission_module",
    epicontacts(),
    id_var = "id"
  )
}

shinyApp(ui = ui, server = server)

#
# linelist <- episimdata::moissala_linelistclean_EN
# group_variables <- c("Sex" = "sex", "Age group" = "age_group")
# date_variables <- c(
#   "Onset" = "date_onset",
#   "Consultation" = "date_consultation",
#   "Admission" = "date_admission",
#   "Outcome" = "date_outcome"
# )
#
# epishiny::launch_module(
#   "delay",
#   linelist,
#   date_vars = date_variables,
#   group_vars = group_variables
# )
