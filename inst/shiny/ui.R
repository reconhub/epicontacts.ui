library(shiny)
library(visNetwork)
library(shiny)
recon.ui::reconNavbarPage("EpiContacts.ui",
                          tabPanel("Data",
                                   fluidRow(
                                     column(
                                       6,
                                       h2("Load data"),
                                       h3("Linelist data"),
                                       shinyHelpers::dataimportUI("import_linelist", sampleDatasets = linelist_examples),
                                       checkboxInput("directed", "Is This A Directed Network ?", value = TRUE)
                                     ),
                                     column(
                                       6,
                                       h3("Contact data"),
                                       shinyHelpers::dataimportUI("import_contact_data", sampleDatasets = contacts_examples),
                                       hr(),
                                       verbatimTextOutput("summary_data")
                                     )
                                   )),
                          tabPanel("Analysis", {
                            fluidRow(
                              column(
                                4,
                                h2("Subset data"),
                                uiOutput("ui1"),
                                hr(),
                                checkboxInput("subset_filter_data", "Enable subsetting", value = FALSE),
                                uiOutput("ui2"),
                                actionButton("subset", "Subset Data Based on Inputs Above")
                              ),
                              column(
                                8,
                                tabsetPanel(
                                  tabPanel("Linelist",
                                           DT::dataTableOutput("linelisttab")),
                                  tabPanel("Contact List",
                                           DT::dataTableOutput("contactstab")),
                                  tabPanel("Network plot",
                                           visNetworkOutput("netplot")),
                                  tabPanel(
                                    "Pairwise viewer",
                                    fluidRow(column(
                                      12,
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
                                    column(6, plotOutput(
                                      "pairwise_rightplot"
                                    ))),
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
                                  tabPanel(
                                    "Degree",
                                    selectInput(
                                      "degree_type",
                                      "Type of degree",
                                      selected = "out",
                                      choices = c("in", "out", "both")
                                    ),
                                    checkboxInput("degree_onlylinelist", "Use only linelist", value = FALSE),
                                    hr(),
                                    plotOutput("degree_histogram"),
                                    p("Histrogram data"),
                                    verbatimTextOutput("degree_table")
                                  )
                                )
                              )
                            )
                          }))
