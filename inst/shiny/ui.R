library(shiny)
library(visNetwork)
library(shiny)
np <- navbarPage(
  "EpiContacts.ui",
  theme = "css/style.css",
  tabPanel(
    "Data",
    fluidRow(
      column(6,
             h2("Load data"),
             h3("Linelist data"),
             shinyHelpers::dataimportUI("import_linelist", sampleDatasets = linelist_examples),
             h3("Contact data"),
             shinyHelpers::dataimportUI("import_contact_data", sampleDatasets = contacts_examples),
             checkboxInput("directed", "Is This A Directed Network ?", value = TRUE)),
      column(6,
             h2("Subset data"),
             checkboxInput("subset_filter_data", "Enable subsetting", value = FALSE),
             uiOutput("ui1"),
             uiOutput("ui2"),
             actionButton("subset", "Subset Data Based on Inputs Above"),
             hr(),
             verbatimTextOutput("summary_data"))
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
    )
  ),
  tabPanel("Degree",
    selectInput("degree_type", "Type of degree", selected = "out", 
                choices = c("in", "out", "both")),
    checkboxInput("degree_onlylinelist", "Use only linelist", value = FALSE),
    hr(),
    plotOutput("degree_histogram"),
    p("Histrogram data"),
    verbatimTextOutput("degree_table")),
  tabPanel("Network plot",
           visNetworkOutput("netplot"))
)
#htmltools::tagAppendAttributes(np[[2]], class = paste0(htmltools::tagGetAttribute(np[[2]], "class"), 
#                                                  "navbar-custom"))
nav_element <- np[[3]][[1]]
old_class <- htmltools::tagGetAttribute(nav_element, "class")
np[[3]][[1]] <- htmltools::tagAppendAttributes(nav_element, 
                                               class = "navbar-custom")
np
