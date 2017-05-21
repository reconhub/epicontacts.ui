library(shiny)
library(visNetwork)
library(shiny)
recon.ui::reconNavbarPage(
  "EpiContacts.ui",
  tabPanel("Data",
           fluidRow(
             column(
               6,
               h2("Load data"),
               h3("Linelist data"),
               shinyHelpers::dataimportUI("import_linelist", sampleDatasets = linelist_examples),
               uiOutput("choose_id_column"),
               checkboxInput("directed", "Is This A Directed Network ?", value = TRUE)
             ),
             column(
               6,
               h3("Contact data"),
               shinyHelpers::dataimportUI("import_contact_data", sampleDatasets = contacts_examples),
               uiOutput("choose_from_column"),
               uiOutput("choose_to_column")
             )
           )),
  tabPanel("Analysis", {
    fluidRow(
      column(
        2,
        ## h2("Subset data"),
        ## uiOutput("ui_subset"),
        ## hr(),
        ## checkboxInput("subset_filter_data", "Enable subsetting", value = FALSE),
        ## uiOutput("ui_filter"),
        ## actionButton("subset", "Subset Data Based on Inputs Above"),
        ## class = "well",
        h2("Plot options"),
        ## actionButton("update_plot", "Update plot"),
        ## class = "well",
        uiOutput("ui_node_color"),
        uiOutput("ui_node_shape"),
        uiOutput("ui_shapes"),
        uiOutput("ui_edge_color"),
        uiOutput("ui_label"),
        uiOutput("ui_edge_label"),
        uiOutput("ui_annot")
      ),
      column(
        10,
        tabsetPanel(
          tabPanel("Summary",
                   verbatimTextOutput("summary_data")),
          tabPanel("Linelist",
                   DT::dataTableOutput("linelisttab")),
          tabPanel("Contact List",
                   DT::dataTableOutput("contactstab")),
          tabPanel("Network plot",
                   visNetworkOutput("netplot")),
          ## tabPanel(
          ## "Pairwise viewer",
          ## fluidRow(column(
          ##   12,
          ##   sliderInput(
          ##     "pairwise_dist_histogram_bins",
          ##     "Number of bins",
          ##     min = 5,
          ##     max = 100,
          ##     value = 30
          ##   )
          ## )),
          ##   fluidRow(
          ##     column(12, plotOutput(
          ##       "pairwise_rightplot"
          ##     ))),
          ##   hr(),
          ##   p(
          ##     "Sample mean: ",
          ##     textOutput("pairwise_sample_mean", inline = TRUE)
          ##   ),
          ##   p(
          ##     "Sample standard deviation: ",
          ##     textOutput("pairwise_sample_sd", inline = TRUE)
          ##   )
          ## ),
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
