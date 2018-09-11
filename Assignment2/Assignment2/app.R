library(shiny)
library(ggplot2)
# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
  
    repl <- switch(input$repl,
                  truet = TRUE,
                   falset = FALSE,
                   TRUE)
    
    if(input$dist == 'unif')
    sample(input$minn:input$maxn, input$n, replace=repl)
    else if(input$dist == 'norm')
    rnorm(input$n, mean=input$mean, sd=input$sd)
    else if(input$dist == 'exp')
    rexp(input$n, rate=input$ratex)
    })
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(d(),main = paste("r", dist, "(", n, ")", sep = ""),col = "#75AADB", border = "white")
    #qplot(d(), geom="histogram", fill=I("lightblue"), col=I("red"))
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(d())
  })
  
  # Generate an HTML table view of the head of the data ----
  output$table <- renderTable({
    head(data.frame(x = d()))
  })
  
}

# Create Shiny app ----
shinyApp(ui = htmlTemplate("www/index.html"), server)
