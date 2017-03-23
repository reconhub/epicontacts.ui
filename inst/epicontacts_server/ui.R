library(shiny)
library(visNetwork)
library(shiny)

shinyUI(fluidPage(

    titlePanel("Contact Tracing"),

    sidebarLayout(
        sidebarPanel(
            shinyHelpers::dataimportUI("import_linelist", label = "Linelist data"),
            shinyHelpers::dataimportUI("import_contact_data", label = "Contact data"),
            checkboxInput("directed", "Is This A Directed Network ?", value = TRUE),
            uiOutput("ui1"),
            uiOutput("ui2"),
            actionButton("subset", "Subset Data Based on Inputs Above")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel('Line List',
                    DT::dataTableOutput("linelisttab")),
                tabPanel('Contact List',
                    DT::dataTableOutput("contactstab")),
                tabPanel(
                  "Pairwise viewer",
                  fluidRow(
                    column(12, 
                      selectInput("pairwise_dist_histogram_col", "Linelist attribute", choices = character(0)),
                      sliderInput("pairwise_dist_histogram_bins", "Number of bins", min = 5, max = 100, value = 30))),
                  fluidRow(
                    column(6, plotOutput("pairwise_distribution_histogram")),
                    column(6, plotOutput("pairwise_boxplot"))
                  ),
                  hr(),
                  p(
                    "Sample mean: ", textOutput("pairwise_sample_mean", inline = TRUE)
                  ),
                  p(
                    "Sample standard deviation: ", textOutput("pairwise_sample_sd", inline = TRUE)
                  )
                ),
                tabPanel('Network plot', 
                         visNetworkOutput("netplot"))
            )
        )
    )
))