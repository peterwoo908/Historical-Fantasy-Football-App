#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load necessary packages
library(shiny)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(kableExtra)
library(stringr)
library(ggplot2)
library(shinythemes)

# Read data from RData file
load("dataframe.RData")

# Define user interface
ui <- fluidPage(
  theme = shinytheme("lumen"),
  div(style = "text-align: center",
      titlePanel("NFL All Time Fantasy")),
  # Team A User Input Panel
  sidebarPanel(
    width = 3,
    p(strong("Team A")),
    selectInput(inputId = "year_qb_team1",
                label = "QB Year",
                choices = unique(data$Year)),
    selectInput(inputId = "qb_team1",
                label = "QB",
                choices = ""),
    selectInput(inputId = "year_rb_team1",
                label = "RB Year",
                choices = unique(data$Year)),
    selectInput(inputId = "rb_team1",
                label = "RB",
                choices = ""),
    selectInput(inputId = "year_te_team1",
                label = "TE Year",
                choices = unique(data$Year)),
    selectInput(inputId = "te_team1",
                label = "TE",
                choices = ""),
    selectInput(inputId = "year_wr_team1",
                label = "WR Year",
                choices = unique(data$Year)),
    selectInput(inputId = "wr_team1",
                label = "WR",
                choices = ""),
  ),
  mainPanel(
    width = 6,
    # Main Panel is subset into 2 different tabs (Table and Plot)
    tabsetPanel(
      type = "tabs",
      tabPanel("Table", id = "panel1",
               fluidRow(
                 column(6, tableOutput("my_table1"), progressBar(id = "progress1", value = 0, striped = TRUE),
                        div(textOutput("progress1_text"), style = "text-align: center; font-weight: bold")),
                 column(6, tableOutput("my_table2"), progressBar(id = "progress2", value = 0, striped = TRUE),
                        div(textOutput("progress2_text"), style = "text-align: center; font-weight: bold")),
               )
      ),
      tabPanel("Plot", id = "panel2",
               fluidRow(
                 column(12, br()),
                 column(12, plotOutput("position_chart"))
               )
      )
    )  
  ),
  # Team B User Input Panel
  sidebarPanel(
    width = 3,
    p(strong("Team B")),
    selectInput(inputId = "year_qb_team2",
                label = "QB Year",
                choices = unique(data$Year),
                selected = "2023"),
    selectInput(inputId = "qb_team2",
                label = "QB",
                choices = ""),
    selectInput(inputId = "year_rb_team2",
                label = "RB Year",
                choices = unique(data$Year),
                selected = "2023"),
    selectInput(inputId = "rb_team2",
                label = "RB",
                choices = ""),
    selectInput(inputId = "year_te_team2",
                label = "TE Year",
                choices = unique(data$Year),
                selected = "2023"),
    selectInput(inputId = "te_team2",
                label = "TE",
                choices = ""),
    selectInput(inputId = "year_wr_team2",
                label = "WR Year",
                choices = unique(data$Year),
                selected = "2023"),
    selectInput(inputId = "wr_team2",
                label = "WR",
                choices = "")
  )
)

# Define server logic
server <- function(input, output, session) {
  
  progress_values <- reactiveValues(progress_value1 = 0, progress_value2 = 0)
  
  update_select_input <- function(input_id, position, year, data = NULL) {
    year <- as.numeric(year)  # Convert year to numeric
    updateSelectInput(session, input_id, position, 
                      choices = data %>% filter(Year == year, Position == position) %>% 
                        select(Player) %>% 
                        arrange(str_extract(Player, "\\w+$")))
  }
  
  # Update inputs for Team A
  observe({
    update_select_input("qb_team1", "QB", input$year_qb_team1, data)
  })
  
  observe({
    update_select_input("rb_team1", "RB", input$year_rb_team1, data)
  })
  
  observe({
    update_select_input("wr_team1", "WR", input$year_wr_team1, data)
  })
  
  observe({
    update_select_input("te_team1", "TE", input$year_te_team1, data)
  })
  
  # Update inputs for Team B
  observe({
    update_select_input("qb_team2", "QB", input$year_qb_team2, data)
  })
  
  observe({
    update_select_input("rb_team2", "RB", input$year_rb_team2, data)
  })
  
  observe({
    update_select_input("wr_team2", "WR", input$year_wr_team2, data)
  })
  
  observe({
    update_select_input("te_team2", "TE", input$year_te_team2, data)
  })
  
  # Calculate and update progress bars
  observe({
    max_points <- data %>% 
      group_by(Position) %>%
      filter(PPR == max(PPR)) %>%
      ungroup() %>%
      summarise(total_ppr = sum(PPR)) %>%
      pull(total_ppr)
    
    team_points1 <- data %>% 
      filter(
        (Year == input$year_qb_team1 & Player == input$qb_team1) |
          (Year == input$year_rb_team1 & Player == input$rb_team1) |
          (Year == input$year_wr_team1 & Player == input$wr_team1) |
          (Year == input$year_te_team1 & Player == input$te_team1)
      ) %>% 
      summarise(total_ppr = sum(PPR)) %>%
      pull(total_ppr)
    
    progress_value1 <- (team_points1 / max_points) * 100
    
    updateProgressBar(session, "progress1", value = progress_value1)
    
    team_points2 <- data %>% 
      filter(
        (Year == input$year_qb_team2 & Player == input$qb_team2) |
          (Year == input$year_rb_team2 & Player == input$rb_team2) |
          (Year == input$year_wr_team2 & Player == input$wr_team2) |
          (Year == input$year_te_team2 & Player == input$te_team2)
      ) %>% 
      summarise(total_ppr = sum(PPR)) %>%
      pull(total_ppr)
    
    progress_value2 <- (team_points2 / max_points) * 100
    
    updateProgressBar(session, "progress2", value = progress_value2)
    
    progress_values$progress_value1 <- progress_value1
    progress_values$progress_value2 <- progress_value2
    
  })
  
  # Render table for Team A
  output$my_table1 <- renderUI({
    HTML(
      data %>%
        filter(
          (Year == input$year_qb_team1 & Player == input$qb_team1) |
            (Year == input$year_rb_team1 & Player == input$rb_team1) |
            (Year == input$year_wr_team1 & Player == input$wr_team1) |
            (Year == input$year_te_team1 & Player == input$te_team1)
        ) %>%
        select(Position, Player, PPR) %>%
        arrange(Position) %>% 
        rbind(c("Total", "", sum(.$PPR))) %>% 
        kbl() %>% 
        kable_styling() %>% 
        as.character()
    )
  })
  
  # Render table for Team B
  output$my_table2 <- renderUI({
    HTML(
      data %>%
        filter(
          (Year == input$year_qb_team2 & Player == input$qb_team2) |
            (Year == input$year_rb_team2 & Player == input$rb_team2) |
            (Year == input$year_wr_team2 & Player == input$wr_team2) |
            (Year == input$year_te_team2 & Player == input$te_team2)
        ) %>%
        select(PPR, Player, Position) %>%
        arrange(Position) %>% 
        rbind(c(sum(.$PPR), "", "Total")) %>% 
        kbl() %>% 
        kable_styling() %>% 
        as.character()
    )
  })
  
  # Render progress text for Team A
  output$progress1_text <- renderText({
    paste(round(progress_values$progress_value1, 1), "%", "to the best team possible.")
  })
  
  # Render progress text for Team B
  output$progress2_text <- renderText({
    paste(round(progress_values$progress_value2, 1), "%", "to the best team possible.")  
  })
  
  # Render position plot
  output$position_chart <- renderPlot({
    # Filter data for Team A
    selected_team1_data <- data %>%
      filter(
        (Year == input$year_qb_team1 & Player == input$qb_team1) |
          (Year == input$year_rb_team1 & Player == input$rb_team1) |
          (Year == input$year_wr_team1 & Player == input$wr_team1) |
          (Year == input$year_te_team1 & Player == input$te_team1)
      ) %>%
      select(Position, PPR) %>%
      mutate(Team = "A")
    
    # Filter data for Team B
    selected_team2_data <- data %>%
      filter(
        (Year == input$year_qb_team2 & Player == input$qb_team2) |
          (Year == input$year_rb_team2 & Player == input$rb_team2) |
          (Year == input$year_wr_team2 & Player == input$wr_team2) |
          (Year == input$year_te_team2 & Player == input$te_team2)
      ) %>%
      select(Position, PPR) %>%
      mutate(Team = "B")
    
    # Combine data for both teams
    combined_data <- rbind(selected_team1_data, selected_team2_data)
    
    # Create the plot
    ggplot(combined_data, aes(x = Position, y = PPR, fill = Team)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      labs(title = "Fantasy Points by Position and Team",
           x = "Position", y = "Fantasy Points", fill = "Team") +
      scale_fill_manual(values = c("A" = "darkgoldenrod", "B" = "steelblue")) +
      theme_minimal() +
      theme(legend.position = "bottom",
            title = element_text(size = 16),
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.title.x = element_text(size = 16, vjust = -2),
            axis.title.y = element_text(size = 14, vjust = 3),
            legend.margin = margin(t = 30),
            legend.text = element_text(size = 16))  # Position legend at the bottom
  })
}

# Run the application
shinyApp(ui, server)

