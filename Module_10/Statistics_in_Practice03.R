# Statistics in Practice 03

# The objective of this work is to demonstrate the use and interpretation of 
# some important statistical tests (parametric and non-parametric). Five tests 
# will be addressed in this work:

# One-Sample t-Test (parametric)
# Two-Sample t-Test (parametric)
# Wilcoxon Test (non-parametric)
# Shapiro-Wilk Test (non-parametric)
# Kolmogorov-Smirnov Test (non-parametric)

# For each test, there will be a description explaining the test, its application, 
# and the associated null hypothesis (H0). The tests will be executed with data 
# generated within the automation dashboard itself.

# In non-parametric tests, the hypotheses are not about population parameters 
# (for example, μ=50 or μ1=μ2). Instead, the null hypothesis is more general. 
# For instance, when comparing two independent groups in terms of a continuous 
# outcome, the null hypothesis in a parametric test is H0: μ1=μ2. In a 
# non-parametric test, the null hypothesis is that the two populations are equal, 
# which is often interpreted as the two populations being equal in terms of their 
# central tendency.

# Packages
install.packages("shiny")
install.packages("shinyjs")
install.packages("shinyvalidate")
install.packages("shinycssloaders")
install.packages("tidyverse")
install.packages("broom")
install.packages("bslib")
install.packages("thematic")
install.packages("DT")
install.packages("plotly")
library(shiny)
library(shinyjs)
library(shinyvalidate)
library(shinycssloaders)
library(tidyverse)
library(broom)
library(bslib)
library(thematic)
library(DT)
library(plotly)

options(warn = -1)

# Load the file with the test descriptions
dados_desc_te <- read.csv("desc_testes_estatisticos.csv")

# UI - User Interface

# Create the navigation page
ui <- navbarPage(
  
  # Create the shinyjs instance
  shinyjs::useShinyjs(),
  
  # Dashboard color theme
  theme = bs_theme(version = 5,
                   bootswatch = "flatly",
                   primary = "#3f7928",
                   secondary = "#5340f7",
                   success = "#dadeba"
  ),
  
  # Style
  tags$style(type = 'text/css', '#nometestedesc {white-space: pre-wrap;}'),
  
  # Dashboard Title
  title = "Shiny Dashboard for Statistical Test Automation",
  tabPanel(
    title = "Home",
    sidebarLayout(
      
      # Sidebar
      sidebarPanel(
        width = 4,
        
        # Input
        selectInput(
          inputId = "nometeste",
          label = "Select the Desired Statistical Test:",
          choices = c("One-Sample t Test",
                      "Two-Sample t Test",
                      "Wilcoxon Signed Rank Test",
                      "Shapiro-Wilk Test",
                      "Kolmogorov-Smirnov Test"
          ),
          selected = "One-Sample t Test"
        ),
        textInput(
          inputId = "primeira_amostra",
          label = "Enter a list of numeric values (separated by commas) or use the button to generate random data:"
        ),
        uiOutput("vector"),
        h5(
          actionButton(
            inputId = "randomnum",
            label = "Generate Random Data"
          ),
          align = "center"
        ),
        uiOutput("samplemean"),
        uiOutput("confidencelevel"),
        h5(
          actionButton(
            inputId = "generate",
            label = "Run the Test"
          ),
          align = "center"
        )
      ),
      
      # Main Panel
      mainPanel(
        fluidRow(
          column(
            width = 6,
            h4(textOutput("testresulttitle")),
            withSpinner(DTOutput("testresult"), type = 7),
            align = "center"
          ),
          column(
            width = 6,
            h4(textOutput("histogramtitle")),
            withSpinner(plotlyOutput("hist", width = "100%"), type = 7),
            align = "center"
          )
        ),
        fluidRow(br()),
        fluidRow(h4(textOutput("descriptiontitle")), align = "center"),
        fluidRow(
          withSpinner(verbatimTextOutput("nometestedesc"), type = 7)
        )
      )
    )
  ),
  nav_item(a(href = "https://www.datascienceacademy.com.br", "Support"))
)

# Server

# Create the server function
server <- function(input, output, session) {
  
  # Shiny theme
  thematic::thematic_shiny()
  
  # Generate 20 random numbers following a normal distribution
  randomnumx <- eventReactive(input$randomnum, {randomnum <- rnorm(n = 20)})
  
  # Generate 20 random numbers following a normal distribution
  randomnumy <- eventReactive(input$randomnum, {randomnum <- rnorm(n = 20)})
  
  # Vector for one-sample tests
  output$vector <- renderUI({
    onevector <- c("One-Sample t Test", "Wilcoxon Signed Rank Test", "Shapiro-Wilk Test")
    
    # If the selected test is not in the above list, show the second sample box
    if(!input$nometeste %in% onevector){
      textInput(
        inputId = "segunda_amostra",
        label = "Enter the second list of numeric values (separated by commas) or use the button to generate random data:"
      )
    }
  })
  
  # Vector for tests that require the sample mean
  output$samplemean <- renderUI({
    samplemean <- c("One-Sample t Test", "Wilcoxon Signed Rank Test", "Two-Sample t Test")
    
    # If the selected test is in the above list, show the box asking for the sample mean
    if(input$nometeste %in% samplemean){
      numericInput(
        inputId = "mu",
        label = "Sample Mean",
        value = 0
      )
    }
  })
  
  # Vector for tests that require a confidence interval
  output$confidencelevel <- renderUI({
    confidencelevel <- c("One-Sample t Test", "Wilcoxon Signed Rank Test", "Two-Sample t Test")
    
    # If the selected test is in the above list, show the box asking for the confidence interval
    if(input$nometeste %in% confidencelevel){
      selectInput(
        inputId = "conf.level",
        label = "Select Confidence Interval:",
        choices = list("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
        selected = 0.90
      )
    }
  })
  
  # First sample data
  observe({updateTextInput(session, "primeira_amostra", value = paste(randomnumx(), collapse = ", "))})
  
  # Second sample data
  observe({updateTextInput(session, "segunda_amostra", value = paste(randomnumy(), collapse = ", "))})
  
  # Sample validation
  iv <- InputValidator$new()
  iv$add_rule("primeira_amostra", sv_required())
  iv$add_rule("segunda_amostra", sv_required())
  iv$add_rule("primeira_amostra",function(value) {
    if(is.na(sum(as.numeric(unlist(str_split(value, pattern = ",")))))) {
      "The data must be numeric and separated by commas"
    }
  })
  iv$add_rule("segunda_amostra", function(value) {
    if(is.na(sum(as.numeric(unlist(str_split(value, pattern = ",")))))) {
      "The data must be numeric and separated by commas"
    }
  })
  
  iv$enable()
  
  # Validates the samples
  observe({
    onevector <- c("One-Sample t Test", "Wilcoxon Signed Rank Test", "Shapiro-Wilk Test")
    if (input$nometeste %in% onevector) {
      shinyjs::toggleState("generate", !is.null(input$primeira_amostra) && input$primeira_amostra != "")
    } else {
      shinyjs::toggleState(
        "generate",
        !is.null(input$primeira_amostra) && input$primeira_amostra != ""
        && !is.null(input$segunda_amostra) && input$segunda_amostra != ""
      )
    }
  })
  
  # Executes the statistical test
  stat_test <- eventReactive(input$generate, {
    
    # Data
    primeira_amostra <- as.numeric(unlist(str_split(input$primeira_amostra, pattern = ",")))
    segunda_amostra <- as.numeric(unlist(str_split(input$segunda_amostra, pattern = ",")))
    conf.level <- as.numeric(input$conf.level)
    
    # Executes the selected test
    if(input$nometeste == "One-Sample t Test") {
      test_result <- t.test(primeira_amostra, mu = input$mu, conf.level = conf.level) %>% tidy()
    }
    else if (input$nometeste == "Two-Sample t Test") {
      test_result <- t.test(x = primeira_amostra, y = segunda_amostra, mu = input$mu, conf.level = conf.level) %>% tidy()
    }
    else if (input$nometeste == "Wilcoxon Signed Rank Test") {
      test_result <- wilcox.test(primeira_amostra, mu = input$mu, conf.level = conf.level) %>% tidy()
    }
    else if (input$nometeste == "Shapiro-Wilk Test") {
      test_result <- shapiro.test(primeira_amostra) %>% tidy()
    }
    else if (input$nometeste == "Kolmogorov-Smirnov Test") {
      test_result <- ks.test(x = primeira_amostra, y = segunda_amostra) %>% tidy()
    }
    
    # Organizes the test result
    test_result_tidy <- test_result %>%
      mutate(result = ifelse(p.value <= 0.05, "Statistically Significant, Reject H0",
                             "Statistically Insignificant, Fail to Reject H0")) %>%
      t() %>%
      tibble(Parameter = rownames(.), Value = .[,1]) %>%
      select(-1) %>%
      mutate(Parameter = str_to_title(Parameter))
    
    return(test_result_tidy)
    
  })
  
  # Test result table
  output$testresult <- renderDT({
    datatable(
      stat_test(),
      rownames = FALSE,
      options = list(
        dom = 't',
        columnDefs = list(
          list(
            className = "dt-center",
            targets = "_all"
          )
        )
      )
    )
  })
  
  # Prepares data for the histogram
  hist_vector <- eventReactive(input$generate, {
    
    # Density function
    primeira_amostra <- density(as.numeric(unlist(str_split(input$primeira_amostra, pattern = ","))))
    
    return(primeira_amostra)
    
  })
  
  # Histogram plot
  output$hist <- renderPlotly({
    hist_vector <- hist_vector()
    plot_ly(x = ~hist_vector$x,
            y = ~hist_vector$y,
            type = "scatter",
            mode = "lines",
            fill = "tozeroy") %>%
      layout(xaxis = list(title = "Data"),
             yaxis = list(title = "Density"),
             title = "Histogram of the First Sample"
      )
  })
  
  # Test description
  output$nometestedesc <- renderPrint({
    req(input$nometeste)
    dados_desc_te %>%
      filter(nome == input$nometeste) %>%
      pull(descricao) %>%
      cat()
  })
  
  # Description title
  output$descriptiontitle <- renderText({
    req(input$nometeste)
    paste("Description of the ", input$nometeste)
  })
  
  # Test result title
  output$testresulttitle <- renderText({
    req(input$nometeste)
    paste("Results of the ", input$nometeste)
  })
  
  # Histogram title
  output$histogramtitle <- renderText({
    req(input$nometeste)
    paste("Histogram for the ", input$nometeste)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)