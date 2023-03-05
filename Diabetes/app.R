library(shiny)
library(tidyverse)
library(reactable)

diabetesRaw <-
  read_delim("diabetes_012_health_indicators_BRFSS2015.csv")

diabetesCount <- diabetesRaw %>%
  filter(Diabetes_012 != 0) %>%
  select(Diabetes_012,
         HighBP,
         HighChol,
         Stroke,
         HeartDiseaseorAttack) %>%
  group_by(Diabetes_012) %>%
  summarize(
    High_BP = sum(HighBP),
    High_Chol = sum(HighChol),
    Stroke = sum(Stroke),
    Heart_Disease_or_Attack = sum(HeartDiseaseorAttack)
  )

ui <- fluidPage(tabsetPanel(
  # About page
  tabPanel("About",
           
           fluidRow(
             column(
               8,
               wellPanel(
                 style = "background-color: #C1C1C1;
                          border-color: #060000;
                          height: 100vh;
                          font-size: 20px",
                 
                 h2("About Type II Diabetes"),
                 
                 p(
                   "Type II diabetes, generally called diabetes, is
                  one of the most widespread chronic diseases in
                  the United States. It impacts millions of
                  Americans every year."
                 ),
                 
                 p(
                   "Diabetes is a chronic disease in which an
                  individual is unable to effectively regulate
                  the levels of glucose in their blood. Diabetes
                  is characterized by the body's inability to
                  create enough insulin or its inability to
                  effectively use the insuline to regulate blood
                  sugar."
                 ),
                 p(
                   "In this project, we aim to analyze the frequency
                  of diabetes in patients with other underlying
                  health conditions like",
                   em(" High Blood Pressure"),
                   ", ",
                   em("High Cholesterol"),
                   ", ",
                   em("previous incidents of Stroke"),
                   ", and ",
                   em("Heart Diseases or Attacks"),
                   "."
                 )
               )
             ),
             
             column(
               4,
               wellPanel(
                 style = "background-color: #54C777;
                      border-color: #060000;
                      height: 100vh;
                      font-size: 20px",
                 
                 h2("The Data"),
                 
                 p(
                   "The data used for this project has been taken
                    from the ",
                   a("Kaggle Diabetes Health Indicator Dataset",
                     href =
                       "https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset"),
                   ". The data includes the responses of 253,680 individuals
                    who took the",
                   strong("Behavioral Risk Factor Surveillance
                    System (BRFSS) survey"),
                   "in 2015."
                 )
               )
               
             )
           )),
  
  # Plot page
  tabPanel("Plot",
           sidebarLayout(
             sidebarPanel(
               style = "background-color: #54C777;
                      border-color: #060000;
                      height: 100vh;
                      font-size: 15px",
               
               p(
                 strong(
                   "To change the background color of
                         the plot, choose a color from the list:"
                 )
               ),
               
               radioButtons(
                 "color",
                 "",
                 choices = c(
                   orange = "darkorange",
                   green = "lightgreen",
                   pink = "lightpink",
                   gray = "lightgray",
                   khaki = "darkkhaki"
                 )
               ),
               
               
               p(
                 strong("Select the underlying conditions you wish to
                         include on the plot:")
               ),
               
               uiOutput("conditions")
             ),
             
             mainPanel(
               style = "background-color: #C1C1C1;
                      border-color: #060000;
                      height: 100vh;
                      font-size: 15px",
               
               plotOutput("barPlot"),
               
               textOutput("message")
             )
           )),
  
  # Table page
  tabPanel("Tables",
           sidebarLayout(
             sidebarPanel(
               style = "background-color: #54C777;
                      border-color: #060000;
                      height: 100vh;
                      font-size: 15px",
               
               p(
                 strong(
                   "To change the background color of
                         the tables, choose a color from the list:"
                 )
               ),
               
               radioButtons(
                 "colorTable",
                 "",
                 choices = c(
                   orange = "darkorange",
                   green = "lightgreen",
                   pink = "lightpink",
                   gray = "lightgray",
                   khaki = "darkkhaki"
                 )
               ),
               
               p(strong(
                 "Select the stage of diabetes you wish
                   to make a table for: "
               )),
               
               radioButtons("type",
                            "",
                            choices = c(
                              Prediabetes = "1",
                              Diabetes = "2"
                            ))
             ),
             mainPanel(
               style = "background-color: #C1C1C1;
                      border-color: #060000;
                      height: 100vh;
                      font-size: 15px",
               
               p(
                 strong(
                   "The table below shows the number of patients with
                          each underlying condition based on the stage of
                          diabetes selected by you"
                 )
               ),
               
               reactableOutput("table1"),
               
               p(
                 strong(
                   "The table below shows the percentage of patients with
                          each underlying condition based on the stage of
                          diabetes selected by you"
                 )
               ),
               
               reactableOutput("table2"),
               
               textOutput("totalPatients")
               
             )
           ))
))

server <- function(input, output) {
  output$conditions <- renderUI({
    checkboxGroupInput("chooseConditions",
                       "",
                       choices = names(subset(
                         diabetesCount, select = -c(Diabetes_012)
                       )))
  })
  
  sample <- reactive({
    if (is.null(input$chooseConditions)) {
      s1 <- data.frame(matrix(ncol = 1, nrow = 0))
    } else {
      s1 <- diabetesCount %>%
        gather(.,
               key = "Conditions",
               value = "Count",
               all_of(input$chooseConditions))
    }
  })
  
  tableDf <- reactive({
    diabetesRaw %>%
      filter(Diabetes_012 == strtoi(input$type)) %>%
      select(Diabetes_012,
             HighBP,
             HighChol,
             Stroke,
             HeartDiseaseorAttack)
  })
  
  
  output$barPlot <- renderPlot({
    if (nrow(sample()) == 0) {
      p <- sample() %>%
        ggplot(aes()) +
        geom_blank() +
        theme(
          plot.background = element_rect(fill = input$color),
          text = element_text(size = 15, color = "black"),
          axis.text = element_text(size = 13, color = "black")
        ) +
        ggtitle("Please select at least one condition")
      
      p
    }
    else {
      p <- sample() %>%
        ggplot(aes(Conditions, Count)) +
        geom_bar(stat = "identity",
                 aes(fill = factor(Diabetes_012)),
                 position = "dodge") +
        coord_flip() +
        theme(
          plot.background = element_rect(fill = input$color),
          text = element_text(size = 15, color = "black"),
          axis.text = element_text(size = 13, color = "black")
        ) +
        ggtitle("Number of prediabetic(1) and diabetic(2) patients for each condition")
      
      p
    }
  })
  
  output$message <- renderText({
    paste("Number of conditions considered: ",
          length(input$chooseConditions))
  })
  
  output$table1 <- renderReactable({
    options(
      reactable.theme = reactableTheme(
        backgroundColor = input$colorTable,
        borderColor = "black",
        borderWidth = "2px"
      )
    )
    
    reactable(
      tableDf() %>%
        summarize(
          High_BP = sum(HighBP),
          High_Chol = sum(HighChol),
          Stroke = sum(Stroke),
          Heart_Disease_or_Attack = sum(HeartDiseaseorAttack)
        )
    )
  })
  
  output$table2 <- renderReactable({
    options(
      reactable.theme = reactableTheme(
        backgroundColor = input$colorTable,
        borderColor = "black",
        borderWidth = "2px"
      )
    )
    
    reactable(
      tableDf() %>%
        summarize(
          High_BP = round((sum(HighBP) / length(Diabetes_012)) * 100, 2),
          High_Chol = round((sum(HighChol) / length(Diabetes_012)) *
                              100, 2),
          Stroke = round((sum(Stroke) / length(Diabetes_012)) *
                           100, 2),
          Heart_Disease_or_Attack = round((
            sum(HeartDiseaseorAttack) / length(Diabetes_012)
          ) * 100, 2)
        )
    )
  })
  
  output$totalPatients <- renderText({
    displayPatients <- tableDf() %>%
      summarize(Total_Patients = length(Diabetes_012))
    
    paste("Total number of patients: ",
          displayPatients$Total_Patients)
  })
}

shinyApp(ui = ui, server = server)
