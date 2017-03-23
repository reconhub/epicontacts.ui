library(shiny)
library(epicontacts)
library(visNetwork)
library(ggplot2)
library(shinyHelpers)
# based on the original shiny app in package epicontacts
shinyServer(function(input, output, session) {
  
  linelist_data <- shinyHelpers::dataimportServer("import_linelist")
  contact_data <- shinyHelpers::dataimportServer("import_contact_data")
  current_data <- reactive({
    linelist <- tibble::as_tibble(linelist_data())
    contacts <- tibble::as_tibble(contact_data())
    if (!is.null(linelist) && nrow(linelist) > 0 && 
        !is.null(contacts) && nrow(contacts) > 0) {
      make_epicontacts(linelist, contacts, directed = TRUE)
    }
  })
  
  subset_data <- eventReactive(input$subset, {
    dat <- current_data()
    
    # build arguments for subsetting
    subsetarglist <- list()
    if (inherits(dat$linelist[, input$interact], "Date")) {
      subsetarglist[[1]] <-
        c(as.Date(input$dynamic[1]), as.Date(input$dynamic[2]))
      
    } else if (inherits(dat$linelist[, input$interact], "numeric")) {
      subsetarglist[[1]] <- input$dynamic
    } else {
      subsetarglist[[1]] <- input$dynamic
    }
    names(subsetarglist)[1] <- input$interact
    
    # call epicontacts method for subsetting
    subset(dat, node.attribute = subsetarglist)
  })
  
  output$ui1 <- renderUI({
    # create list of attributes from linelist minus the id column
    datcols <- names(current_data()$linelist)[-1]
    selectInput("interact", "Linelist Attributes", choices = datcols)
  })
  
  output$ui2 <- renderUI({
    req(input$interact)
    
    dat <- current_data()$linelist
    
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
    
    if (input$subset) {
      dat <- subset_data()
    } else {
      dat <- current_data()
    }
    
    plot(dat,
         annot = TRUE,
         editor = TRUE,
         group = input$interact)
    
  })
  
  output$linelisttab <- DT::renderDataTable ({
    req(input$interact)
    if (input$subset) {
      subset_data()$linelist
    } else {
      current_data()$linelist
    }
    
  })
  
  output$contactstab <- DT::renderDataTable ({
    req(input$interact)
    if (input$subset) {
      subset_data()$contacts
    } else {
      current_data()$contacts
    }
  })
  
  # pairwise
  
  valid_pairwise_cols <- reactive({
    unlist(Filter(function(x) {
      col_value <- unlist(current_data()$linelist[[x]])
      print(col_value)
      (is.numeric(col_value) ||
          inherits(col_value, "Date") ||
          is.character(col_value)) && !all(is.na(col_value))
    }, colnames(current_data()$linelist)[-1]))
  })
  
  observe({
    updateSelectInput(session, "pairwise_dist_histogram_col",
                      choices = valid_pairwise_cols())
  })
  
  pairwise_dist <- reactive({
    column <- input$pairwise_dist_histogram_col
    is_valid_column <- column %in% valid_pairwise_cols()
    if (is_valid_column) {
      get_pairwise(current_data(), column)
    }
  })
  pairwise_plot_data <- reactive({
    data.frame(value = pairwise_dist(),
               stringsAsFactors = FALSE)
  })
  output$pairwise_distribution_histogram <- renderPlot({
    column <- input$pairwise_dist_histogram_col
    plot_data <- pairwise_plot_data()
    mean_dist <- mean(plot_data$value, na.rm = TRUE)
    ggplot(plot_data, aes(value)) +
      geom_histogram(bins = input$pairwise_dist_histogram_bins) +
      geom_vline(xintercept = mean_dist) +
      xlab(column) +
      ggtitle(paste0("Histogram pairwise distances of column '", column, "'")) + 
      ylab("value")
  })
  
  output$pairwise_sample_mean <- renderText({
    mean(pairwise_dist(), na.rm = TRUE)
  })
  
  output$pairwise_sample_sd <- renderText({
    sd(pairwise_dist(), na.rm = TRUE)
  })
  
  output$pairwise_boxplot <- renderPlot({
    column <- input$pairwise_dist_histogram_col
    plot_data <- pairwise_plot_data()
    ggplot(plot_data, aes(value, value)) +
      geom_boxplot() +
      xlab(column) +
      ggtitle(paste0("Boxplot pairwise distances of column '", column, "'")) + 
      ylab("value")
  })
})
