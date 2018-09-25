library(shiny)
library(ggplot2)
source("func.r")

ui <- fluidPage(
  
  titlePanel("Лингвистическая переменная \"Расход топлива\""),
  
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      sliderInput("very_low", "Очень низкий:",
                  min = 1, max = 15,
                  value = c(2,3)),
      
      sliderInput("low", "Низкий:",
                  min = 1, max = 15,
                  value = c(1,5)),
      
      sliderInput("medium", "Средний:",
                  min = 1, max = 15,
                  value = c(5,8)),
      
      sliderInput("upper_med", "Выше среднего:",
                  min = 1, max = 15,
                  value = c(8,9)),
      
      sliderInput("high", "Высокий:",
                  min = 1, max = 15,
                  value = c(7,14)),
      
      sliderInput("very_high", "Очень высокий:",
                  min = 1, max = 15,
                  value = c(13,14))
    ),
    
    mainPanel(
      plotOutput(outputId = "resPlot"),
      numericInput("val","Введите расход топлива:",
                   value = 5,min=1,max=15,step = 0.1),
      h3(textOutput("term_name"))
    )
  )
)

server <- function(input, output) {
  
  output$resPlot <- renderPlot({
    # x <- c(1:15)
    x <- seq(1,15, by = 0.1)
    y_vl <- sapply(x,func.class_lz,a = input$very_low[1],c = input$very_low[2])
    y_l <- sapply(x,func.class_pi,a = input$low[1],c = input$low[2])
    y_m <- sapply(x,func.class_pi,a = input$medium[1],c = input$medium[2])
    y_um <- sapply(x,func.class_tr,a = input$upper_med[1],c = input$upper_med[2])
    y_h <- sapply(x,func.class_s,a = input$high[1],c = input$high[2])
    y_vh <- sapply(x,func.class_ls,a = input$very_high[1],c = input$very_high[2])
    ggplot()+
      labs(x = "Расход топлива",
           y = "Функция принадлежности")+
      geom_area(aes(x,y_vl),col = "black",size = 0.5,fill = "#009933",alpha = 0.5)+
      geom_area(aes(x,y_l),col = "black",size = 0.5,fill = "#33cc33",alpha = 0.5)+
      geom_area(aes(x,y_m),col = "black",size = 0.5,fill = "#e6e600",alpha = 0.5)+
      geom_area(aes(x,y_um),col = "black",size = 0.5,fill = "#FFBB00",alpha = 0.5)+
      geom_area(aes(x,y_h),col = "black",size = 0.5,fill = "#ff661a",alpha = 0.5)+
      geom_area(aes(x,y_vh),col = "black",size = 0.5,fill = "#b30000",alpha = 0.5)
    
    
  })
  
  output$term_name <- renderText({
    term <- ""
    if(is.na(input$val)==FALSE){
    if(input$val >= input$very_low[1] && input$val <= input$very_low[2])
      term <- "\"Очень низкий\""
    if(input$val >= input$low[1] && input$val <= input$low[2])
      term <- paste(term,"\"Низкий\"")
    if(input$val >= input$medium[1] && input$val <= input$medium[2])
      term <- paste(term,"\"Средний\"")
    if(input$val >= input$upper_med[1] && input$val <= input$upper_med[2])
      term <- paste(term,"\"Выше среднего\"")
    if(input$val >= input$high[1] && input$val <= input$high[2])
      term <- paste(term,"\"Высокий\"")
    if(input$val >= input$very_high[1] && input$val <= input$very_high[2])
      term <- paste(term,"\"Очень высокий\"")
    if(term == "") term <- "N/A"
   paste("Введенное значение соответсвует термам:",term)}
    else
      "Расход топлива не введен"
  })
}

# Create Shiny app ----
shinyApp(ui, server)

