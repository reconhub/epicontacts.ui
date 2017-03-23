library(shiny)
library(visNetwork)
library(shiny)

shinyUI(navbarPage(
  "EpiContacts.ui",
  tabPanel(
    "Data",
    fluidRow(
      column(6,
             h2("Load data"),
             h3("Linelist data"),
             shinyHelpers::dataimportUI("import_linelist"),
             h3("Contact data"),
             shinyHelpers::dataimportUI("import_contact_data"),
             checkboxInput("directed", "Is This A Directed Network ?", value = TRUE)),
      column(6,
             h2("Subset data"),
             checkboxInput("subset_filter_data", "Enable subsetting", value = FALSE),
             uiOutput("ui1"),
             uiOutput("ui2"),
             actionButton("subset", "Subset Data Based on Inputs Above"))
    )
  ),
  tabPanel("Linelist",
           DT::dataTableOutput("linelisttab")),
  tabPanel("Contact List",
           DT::dataTableOutput("contactstab")),
  tabPanel(
    "Pairwise viewer",
    fluidRow(column(
      12,
      selectInput(
        "pairwise_dist_col",
        "Linelist attribute",
        choices = character(0)
      ),
      sliderInput(
        "pairwise_dist_histogram_bins",
        "Number of bins",
        min = 5,
        max = 100,
        value = 30
      )
    )),
    fluidRow(column(
      6, plotOutput("pairwise_distribution_histogram")
    ),
    column(6, plotOutput("pairwise_boxplot"))),
    hr(),
    p(
      "Sample mean: ",
      textOutput("pairwise_sample_mean", inline = TRUE)
    ),
    p(
      "Sample standard deviation: ",
      textOutput("pairwise_sample_sd", inline = TRUE)
    ),
    hr(),
    h3("Estimate distribution parameters"),
    checkboxInput("pairwise_est_density", "Estimate a distribution parameters", value = FALSE),
    selectInput("pairwise_est_density_fun", "Distribution", choices = c("Discretized Exponential" = "dexp", 
                                                    "Discretized Gamma" = "dgamma")),
    verbatimTextOutput("pairwise_dist_optim_output")
  ),
  tabPanel("Network plot",
           visNetworkOutput("netplot"))
))