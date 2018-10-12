library(shiny)
library(shinyforms)

ui <- fluidPage(
  h1("shinyforms example"),
  selectInput("dist", "Distribution type:",
              list("Uniform" = "unif","Normal" = "norm","Exponential" = "exp")),
  selectInput("repl", "Replace:",
              list("True" = "truet","False" = "falset")),
  numericInput("n", "Number of samples:", 20, min = 1, max = 100),
  numericInput("minn", "Minimum:", 1, min = 1, max = 100),
  numericInput("maxn", "Maximum:", 100, min = 2, max = 100),
  numericInput("mean", "Mean:", 5, min = 5, max = 100),
  numericInput("sd", "Standard Deviation:", 5, min = 5, max = 100),
  numericInput("ratex", "Rate:", 5, min = 5, max = 100),
  verbatimTextOutput("hover_info"),
  plotOutput("plot", height=300, hover = hoverOpts(id = "plot_hover", delayType = "throttle"))
)

server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    
    repl <- switch(input$repl,
                   truet = TRUE,
                   falset = FALSE,
                   TRUE)
    
    if(input$dist == "unif")
    sample(input$minn:input$maxn, input$n, replace=repl)
    else if(input$dist == "norm")
    rnorm(input$n, mean=input$mean, sd=input$sd)
    else if(input$dist == "exp")
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
  
  output$hover_info <- renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })
}

shinyApp(ui = ui, server = server)