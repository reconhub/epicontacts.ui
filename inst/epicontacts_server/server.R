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
  
  linelist_data <- shinyHelpers::dataimportServer("import_linelist")
  contact_data <- shinyHelpers::dataimportServer("import_contact_data")
  
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
         editor = TRUE,
         group = input$interact)
    
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
  
  observe({
    updateSelectInput(session, "pairwise_dist_col",
                      choices = valid_pairwise_cols())
  })
  
  pairwise_dist <- reactive({
    column <- input$pairwise_dist_col
    is_valid_column <- column %in% valid_pairwise_cols()
    if (is_valid_column) {
      get_pairwise(current_data(), column)
    }
  })
  
  pairwise_plot_data <- reactive({
    data.frame(value = pairwise_dist(),
               stringsAsFactors = FALSE)
  })
  
  pairwise_dgamma_est <- reactive({
    # distcrete gamma ML estimation based on Rich FitzJohn's distcrete package. License MIT 2016
    data <- pairwise_dist()
    data <- data[!is.na(data)]
    deviance <- function(param) {
      d <- distcrete("gamma", interval = 1L, param[1], param[2])$d
      -2 * sum(d(data, log = TRUE))
    }
    optim(c(1,1), deviance)
  })
  
  pairwise_dexp_est <- reactive({
    # distcrete exp ML estimation based on Rich FitzJohn's distcrete package. License MIT 2016
    data <- pairwise_dist()
    data <- data[!is.na(data)]
    ll <- function(param) {
      d <- distcrete("exp", interval = 1L, param)$d
      sum(d(data, log = TRUE))
    }
    optimise(ll, c(0, 20), maximum = TRUE)
  })
  
  pairwise_density_fun <- reactive({
    fun_key <- input$pairwise_est_density_fun
    if (fun_key == "dgamma") {
      est <- pairwise_dgamma_est()
      distcrete("gamma", interval = 1L, est$par[1], est$par[2])$d
    } else if (fun_key == "dexp") {
      est <- pairwise_dexp_est()
      distcrete("exp", interval = 1L, est$maximum)$d
    }
  })
  
  output$pairwise_distribution_histogram <- renderPlot({
    column <- input$pairwise_dist_col
    plot_data <- pairwise_plot_data()
    mean_dist <- mean(plot_data$value, na.rm = TRUE)
    p <- ggplot(plot_data, aes(value)) +
      geom_vline(xintercept = mean_dist) + 
      xlab(column) +
      ggtitle(paste0("Histogram pairwise distances of column '", column, "'")) + 
      ylab("value")
    if (input$pairwise_est_density) {
      dgam <- pairwise_density_fun()
      p <- p + 
        stat_function(fun = dgam, colour = "red") +                    
        geom_histogram(aes(y = ..density..), alpha = 0.4, bins = input$pairwise_dist_histogram_bins)
    } else {
      p <- p + geom_histogram(bins = input$pairwise_dist_histogram_bins)
    }
    p
  })
  
  output$pairwise_dist_optim_output <- renderPrint({
    if (input$pairwise_est_density) {
      fun_key <- input$pairwise_est_density_fun
      if (fun_key == "dgamma") {
        pairwise_dgamma_est()
      } else if (fun_key == "dexp") {
        pairwise_dexp_est()
      }
    }
  })
  
  output$pairwise_sample_mean <- renderText({
    mean(pairwise_dist(), na.rm = TRUE)
  })
  
  output$pairwise_sample_sd <- renderText({
    sd(pairwise_dist(), na.rm = TRUE)
  })
  
  output$pairwise_boxplot <- renderPlot({
    column <- input$pairwise_dist_col
    plot_data <- pairwise_plot_data()
    ggplot(plot_data, aes(value, value)) +
      geom_boxplot() +
      xlab(column) +
      ggtitle(paste0("Boxplot pairwise distances of column '", column, "'")) + 
      ylab("value")
  })
})
