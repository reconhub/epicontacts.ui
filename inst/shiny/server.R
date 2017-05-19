library(shiny)
library(epicontacts)
library(visNetwork)
library(ggplot2)
library(shinyHelpers)
# based on the original shiny app in package epicontacts

# a variable to quickload data during development
dev <- FALSE

# converts types of a data.frame
guess_coltypes <- function(df) {
  if (!is.data.frame(df)) {
    return(df)
  }
  for(col in colnames(df)) {
    col_v <- df[[col]]
    parser <- readr::guess_parser(col_v)
    if (parser == "integer") {
      df[[col]] <- as.integer(df[[col]])
    }
    if (parser == "date") {
      df[[col]] <- lubridate::as_date(df[[col]])
    }
    if (parser == "double") {
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  df
}
shinyServer(function(input, output, session) {
  
  linelist_data <- shinyHelpers::dataimportServer("import_linelist", sampleDatasets = linelist_examples)
  contact_data <- shinyHelpers::dataimportServer("import_contact_data", sampleDatasets = contacts_examples)
  
  base_data <- reactive({
    linelist <- guess_coltypes(linelist_data())
    contacts <- guess_coltypes(contact_data())
    if (!is.null(linelist) && nrow(linelist) > 0 && 
        !is.null(contacts) && nrow(contacts) > 0) {
      make_epicontacts(linelist, contacts, directed = input$directed)
    } else if (dev) {
      make_epicontacts(outbreaks::mers_korea_2015$linelist, outbreaks::mers_korea_2015$contacts, directed = TRUE)      
    }
  })
  
  current_data <- reactive({
    dat <- base_data()
    input$subset # trigger if clicked
    if (input$subset_filter_data) {
      # build arguments for subsetting
      subsetarglist <- list()
      interact <- isolate(input$interact)
      dynamic <- isolate(input$dynamic)
      if (inherits(dat$linelist[, interact], "Date")) {
        subsetarglist[[1]] <-
          c(as.Date(dynamic[1]), as.Date(dynamic[2]))
      } else if (inherits(dat$linelist[, interact], "numeric")) {
        subsetarglist[[1]] <- dynamic
      } else {
        subsetarglist[[1]] <- dynamic
      }
      names(subsetarglist)[1] <- interact
      
      # call epicontacts method for subsetting
      subset(dat, node.attribute = subsetarglist)
    } else {
      dat
    }
  })
  
  output$summary_data <- renderPrint({
    summary(current_data())
  })
  
  output$ui1 <- renderUI({
    # create list of attributes from linelist minus the id column
    dat <- base_data()$linelist
    datcols <- colnames(dat)[-1]
    selectInput("interact", "Linelist Attributes", choices = datcols)
  })
  
  output$ui2 <- renderUI({
    req(input$interact)
    
    dat <- base_data()$linelist
    
    # define character of factor checking function
    factchar <- function(x) {
      is.character(x) | is.factor(x)
    }
    
    # create list of input options based on class of columm
    numcols <- names(dat[, sapply(dat, inherits, "numeric")])
    datecols <- names(dat[, sapply(dat, inherits, "Date")])
    factorcols <- names(dat[, sapply(dat, factchar)])
    
    switch(input$interact,
           if (input$interact %in% factorcols) {
             radioButtons(
               "dynamic",
               input$interact,
               choices = levels(as.factor(dat[, input$interact])),
               selected = NULL
             )
           } else if (input$interact %in% numcols) {
             numericInput("dynamic", input$interact, value = median(dat[, input$interact]))
           } else if (input$interact %in% datecols) {
             dateRangeInput("dynamic", input$interact)
           } else {
             textInput("dynamic", input$interact)
           })
  })
  
  output$netplot <- renderVisNetwork ({
    req(input$interact)
    plot(current_data(),
         annot = TRUE,
         editor = TRUE)
    
  })
  
  output$linelisttab <- DT::renderDataTable ({
    req(input$interact)
    current_data()$linelist
  })
  
  output$contactstab <- DT::renderDataTable ({
    req(input$interact)
    current_data()$contacts
  })
  
  # pairwise
  
  valid_pairwise_cols <- reactive({
    unlist(Filter(function(x) {
      col_value <- unlist(base_data()$linelist[[x]])
      (is.numeric(col_value) ||
          inherits(col_value, "Date") ||
          is.character(col_value)) && !all(is.na(col_value))
    }, colnames(base_data()$linelist)[-1]))
  })
  
  pairwise_dist <- reactive({
    column <- input$interact
    is_valid_column <- column %in% valid_pairwise_cols()
    if (is_valid_column) {
      get_pairwise(current_data(), column)
    }
  })
  
  pairwise_plot_data <- reactive({
    data.frame(value = pairwise_dist(),
               stringsAsFactors = FALSE)
  })
  
  output$pairwise_sample_mean <- renderText({
    mean(pairwise_dist(), na.rm = TRUE)
  })
  
  output$pairwise_sample_sd <- renderText({
    sd(pairwise_dist(), na.rm = TRUE)
  })
  
  output$pairwise_rightplot <- renderPlot({
    column <- input$interact
    plot_data <- pairwise_plot_data()
    ggplot(plot_data, aes(value)) +
      xlab(column) +
      geom_density(color = "red") +
      geom_histogram(aes(y = ..density..), 
                     alpha = 0.4, bins = input$pairwise_dist_histogram_bins) +
      ggtitle(paste0("Density plot of pairwise distances of column '", column, "'")) + 
      ylab("value")
  })
  
  # degree
  degree_result <- reactive({
    get_degree(current_data(), 
               type = input$degree_type,
               only_linelist = input$degree_onlylinelist)
  })
  
  output$degree_table <- renderPrint({
    table(degree_result())
  })
  
  output$degree_histogram <- renderPlot({
    barplot(table(degree_result()), main = "Degree distribution")
  })
})
