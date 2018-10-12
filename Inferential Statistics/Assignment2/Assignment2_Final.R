library(shiny)
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    .mg-histogram .mg-bar rect {
                    fill: #75AADB;
                    shape-rendering: auto;
                    }
                    .mg-histogram .mg-bar  {
                    fill: #75AADB;
                    shape-rendering: auto;
                    }
                    
                    
                    .mg-histogram .mg-bar rect.active {
                    fill: #ffa500;
                    }"))),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(   selectInput("typee","Select the type of distribution",c("Uniform Distribution"="unii","Normal Distribution"="normm","Exponential Distribution"="expp"),selected = "Unifrom Distribution"),
                    selectInput("sele","Replacement",c("Yes"="yess","No"="noo")),
                    
                    sliderInput("numb",
                                "Number of Samples:",
                                min = 20,
                                max = 100,
                                value = 20),
                    sliderInput("unimin",
                                "Enter the Minimum Value for for Uniform Distibution: ",
                                min = 1,
                                max = 99,
                                value = 1),
                    sliderInput("unimax",
                                "Enter the Maximum Value for Uniform Distibution: ",
                                min = 2,
                                max = 100,
                                value = 100),
                    sliderInput("meann",
                                "Enter the Mean for Normal Distibution: ",
                                min = 0,
                                max = 50,
                                value = 10),
                    sliderInput("stddev",
                                "Enter the Standard Deviation for Normal Distibution: ",
                                min = 0,
                                max = 50,
                                value = 10),
                    
                    sliderInput("exprate",
                                "Enter the Rate for Exponential Distibution: ",
                                min = 1,
                                max = 100,
                                value = 1),
                    tags$div(class="header", checked=NA,
                             tags$p("Check the deployed version here:"),
                             tags$a(href="https://sangamkotalwar.shinyapps.io/Assignmnet_2Final/", "Click Here!")
                    )
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot",brush = brushOpts(id = "plot_brush"),hover = hoverOpts(id = "plot_hover"))
    )
  )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
  d <- reactive({
    
    sele <- switch(input$sele,
                   noo = TRUE,
                   yess = FALSE,
                   TRUE)
    disttt<-switch(input$typee,
                   unii=sample(input$unimin:input$unimax,input$numb,replace = sele),
                   normm=rnorm(input$numb,mean=input$meann,sd=input$stddev),
                   expp=rexp(input$numb,input$exprate),
                   sample(input$unimin:input$unimax,input$numb,replace = sele)
    )

  })
  faa<- reactive({
    disttt<-switch(input$typee,
                   unii="Uniform Distribution of ",
                   normm="Normal Distribution of ",
                   expp="Exponential Distribution of ",
                   "Uniform Distribution of "
    )
  })
  dkk<-reactive({

    if( is.null(input$plot_brush$xmax) && is.null(input$plot_hover$x)) 
      color="blue"
    else if(!is.null(input$plot_hover$x))
    {
      color=dkkb2()
    }
    else if( !is.null(input$plot_brush$xmax) && is.null(input$plot_hover$x)) 
    {
      color=dkkb()
    }
    
    else color=dkkb()
  })
  
  dkkb<-reactive({
    color="blue"
    flag=1
    i=1
    differe =((max(d())-min(d()))/10)
    check=min(d())
    while(i<11)
    {  
      if (check>(input$plot_brush$xmax))
      {
        flag=2
      }
      if(((input$plot_brush$xmin-differe)<check) && (flag==1))
      {
        color[[i]]<-"orange"
      }  
      else{
        color[[i]]<-"blue"
      }
      i=i+1
      check=check+differe
    }
    check=min(d())
    
    
    return(color)
  })
  dkk2<-reactive({

    if( is.null(input$plot_hover$x) ) 
    {
      color="blue"
    }
    else
      color=dkkb2()
    
  })
  dkkb2<-reactive({
    color=c("blue","blue","blue","blue","blue","blue","blue","blue","blue","blue")
    abcc=(as.integer((input$plot_hover$x-  min(d()))*10  / (max(d())-min(d()) ))+1)
    color[[abcc]]="orange"
    return(color)
    #print(color)
  })

  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$numb
    minv=min(d())
    maxv=max(d())
    hist(d(),breaks=seq(minv,maxv,l=11),main = paste(faa(),n, " Random Variables", sep = ""),col = dkk(), border = "white")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


#The app can be run on cloud at https://sangamkotalwar.shinyapps.io/Assignmnet_2Final/