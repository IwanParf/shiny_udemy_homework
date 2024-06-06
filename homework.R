library(shiny)
library(shinythemes)
library(DT)
library(ggrepel)
library(tidyverse)





ui <- navbarPage(
  theme = shinytheme("spacelab"),
  titlePanel("The Mining Stock Scale"),
  tabPanel(
    "ADJUST YOUR MINING STOCKS",
    wellPanel(
      sliderInput(
        "weight1",
        "Weight on Grade 1",
        min = 0,
        max = 20,
        value = 10,
        step = 1
      ),
      sliderInput(
        "weight2",
        "Weight on Grade 2",
        min = 0,
        max = 20,
        value = 5,
        step = 1
      ),
      sliderInput(
        "weight3",
        "Weight on Grade 3",
        min = 0,
        max = 6,
        value = 1,
        step = 0.2
      )
    ),
    mainPanel(plotOutput("plot", brush = "brushy", width = "150%"),
      dataTableOutput("brushed_table", width = "150%")
      )
  ), 
  tabPanel("DOCUMENTATION",
           h4("check out this video for getting help on R"),
           tags$iframe(src = "https://www.youtube.com/embed/vySGuusQI3Y?si=Oh3G5deJpPJTewFA",
      title = "YouTube video player",
      width = "900",
      height = "600",
      controls = TRUE
    )
  ),
  tabPanel(
    "DATA TABLE WITH THE UNDERLYING DATA",
    downloadButton(outputId = "download", label = "Download Table"),
    dataTableOutput("table_input")
  )
)

server <- function(input, output, session) {
  # prep

  df <- read_csv2(str_c("course_proj_data.csv"))
  

  
  # calculation of weighted score
  df_formatted <- reactive({
    mutate(df, points = G1 * input$weight1 + G2 * input$weight2 + G3 * input$weight3)
  })
  
  # plot of weighted scores vs evaluation
  output$plot <- renderPlot(
    ggplot(df_formatted(), aes(points, `MarketCap in M`)) +
      geom_point() +
      geom_smooth(method = "lm") +
      geom_text_repel(data = df_subset(), aes(label = Symbol),
                      color = "red", fontface = "bold") +
      labs(x = "Your calculated score",
           y = "Market capitalization in million USD")
  )
  
  # brush functionality
  brush_saved <- reactiveValues(brushed = NULL)
  observeEvent(eventExpr = input$brushy,
               handlerExpr = {brush_saved$brushed <- input$brushy})
  df_subset <- reactive({
    sel <- brushedPoints(df_formatted(), brush_saved$brushed)
    return(sel)
  })
  
  output$brushed_table <- renderDataTable({datatable(df_subset(),
                                                    extensions = "Buttons",
                                                    filter = "top",
                                                    options = 
                                                      list(buttons = 
                                                             c("copy", "csv",
                                                               "excel", "pdf"),
                                                           dom = "Bt"))})
  
  # input table for third tabPanel
  output$table_input <- datatable(df) %>% 
    formatCurrency("MarketCap in M", "$") %>% 
    formatStyle("Symbol", color = "grey") %>% 
    formatStyle(c("G1", "G2", "G3"), backgroundColor = "lightblue") %>% 
      renderDataTable()
  output$download <- downloadHandler(
    filename = "underlying_data.csv",
    content = function(file) {
      write_csv(df, file)
    }
  )
  
}

shinyApp(ui, server)