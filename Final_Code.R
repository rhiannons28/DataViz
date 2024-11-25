library(tidyverse)
library(patchwork)
library(DBI)
library(RMariaDB)
library(fmsb)
library(corrplot)
library(reshape2)
library(shiny)
library(gganimate)
library(transformr)
library(shiny)
pacman::p_load(plotly, gapminder)
sleep<-data.frame(read.csv("sleep_data_request copy.csv", header=TRUE))
Rsleep <-data.frame(read.csv("R_SleepData.csv", header=TRUE))

#Box Plots (Mine)
r_sq <- plot_ly(Rsleep, y= ~Sleep_Quality, type="box", color = "red",
                hoverinfo = "y", legendgroup = NULL)%>%
  layout(title = list(text="My Sleep Quality Distribution", y = 0.975),
         xaxis = list(title = "My Dataset"),
         yaxis = list(title = "Sleep Quality", range = c(4,10)),
         hovermode = "closest",
         hoverlabel = list(bgcolor = "white"))
r_sq

#MIT Box Plot
s_sq <- plot_ly(sleep, y = ~mean_sleep_quality, type = "box",
                line = list(color = "blue"),
                hoverinfo = "y", showlegend = FALSE) %>%
  layout(title = list(text = "MIT Sleep Quality Distribution", y = 0.975),
         xaxis = list(title = "Dataset"),
         yaxis = list(title = "Sleep Quality", range = c(6,10)),
         hovermode = "closest",
         hoverlabel = list(bgcolor = "white"))
s_sq

#Violin Plot Comparing Sleep Quality by Gender (MIT Dataset)
violin2<-plot_ly(sleep, x = ~gender, y = ~mean_sleep_quality, type = "violin", color = ~gender, colors = c("red", "blue"),
                 hoverinfo = "y+x+name") %>%
  layout(title = list(text="MIT Comparison of Sleep Quality by Gender", y = 0.975), 
         xaxis = list(title = "Gender"), 
         yaxis = list(title = "Sleep Quality"),
         hovermode = "closest")
violin2

#Line Plot
Rsleep$Date <- as.Date(Rsleep$Date, format="%m/%d/%y")
long_data <- Rsleep %>%
  pivot_longer(cols = c(Sleep_Quality, Sleep_Duration),
               names_to = "variable",
               values_to = "value")
line_plot <- ggplot(long_data, aes(x = Date, y = value, color = variable)) +
  geom_line() +
  geom_point(size = 2) +
  labs(title = "My Sleep Quality and Sleep Duration Over Time",
       x = "Date",
       y = "Value") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1)
line_plot

#Radar Chart
Rsleep3 <- Rsleep %>% select("Sleep_Quality", "Heart.Rate.Variability..HRV.", "Overall_Score", "Sleep_Duration") %>%
  rename("Sleep Quality" = "Sleep_Quality", "HRV" = "Heart.Rate.Variability..HRV.", "Overall Score" = "Overall_Score",
         "Sleep Duration" = "Sleep_Duration")
#Scaling Data to be between 0 and 1
sleep_scaled <- Rsleep3
sleep_scaled[, c("HRV", "Sleep Quality", "Overall Score", "Sleep Duration")] <- scale(Rsleep3[, c("HRV", "Sleep Quality", "Overall Score", "Sleep Duration")])

labels <- c("HRV", "Sleep Quality", "Overall Score", "Sleep Duration")
values <- as.data.frame(sleep_scaled[, c("HRV", "Sleep Quality", "Overall Score", "Sleep Duration")])

radar_chart <- radarchart(values, maxmin = FALSE, pcol = c("#FF6347"), plwd = 1, plty = 2, 
                          cglwd = 1, axislabcol = "black", title = "My Relationship between Overall Score and Other Variables", seg = 4)
axis(1, cex.axis = 0.8, las = 2)
radar_chart

#Scatterplot of Sleep vs. Exercise Minutes with Overall Sleep Quality
Rsleep_date<- Rsleep
Rsleep$Day <- format(Rsleep$Date, "%d")
Rsleep_date$Date <- format(Rsleep$Date, "%m/%d")

scatter1 <- plot_ly(Rsleep_date, x=~Exercise_Minutes, y=~Sleep_Quality, size=~ Sleep_Duration, 
                    type = "scatter", mode = "markers", marker=list(sizemode="diameter", color = "red"), frame =~Date)


interactive_chart <- scatter1 %>% layout(
  title = list(text="My Exercise Minutes vs. Sleep Quality Over Time", y = 0.975), 
  xaxis = list(title = "Exercise Minutes"), 
  yaxis = list(title = "Sleep Quality"),
  updatemenus = list(
    list(
      type = "buttons",
      showactive = FALSE,
      x = 0,  
      y = -0.3,  
      buttons = list(
        
        list(
          label = "Pause",
          method = "animate",
          args = list(NULL, list(frame = list(duration = 0, 
                                              redraw = TRUE), 
                                 mode = "immediate"))
        )
      )
    )
  )
)

interactive_chart

#Correlation Heat Maps
#Correlation Heat Map (My Dataset)
no_gen <- select(Rsleep, -c(Date, Gender, Bed_Time, Wakeup_Time)) %>%
  rename("Sleep Duration" = "Sleep_Duration", "Sleep Quality" = "Sleep_Quality", 
         "Average HR" = "Avg_Heart_Rate", "HRV" = "Heart.Rate.Variability..HRV.", "Exercise Min" = "Exercise_Minutes",
         "Steps" = "Steps", "Overall Score" = "Overall_Score")
Rsleep2 <- no_gen %>%
  mutate_all(as.numeric)

corr_mat <- round(cor(Rsleep2),2)
melted_corr_mat <- melt(corr_mat)

corr_heat<-ggplot(melted_corr_mat, aes(x=Var1, y=Var2, 
                                       fill=value)) + geom_tile() +
  geom_text(aes(Var2, Var1, label = value), 
            color = "black", size = 4) + labs(title = "Personal Correlation Heatmap - Sleep Metrics",
                                              x = "Variable 1", y = "Variable 2") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_gradient(low = "#FFCCCB",
                      high = "#FF0000",
                      guide = "colorbar")
corr_heat

#Comparison w/ MIT Data
sleep_corr <- select(sleep,-c(subject_id, gender, sleep_quality_std)) %>%
  rename("Sleep Duration" = "sleep_duration", "Mean Sleep Quality" = "mean_sleep_quality", "Sleep STD Min" = "sleep_std_minutes",
         "Overall Score" = "overall_score")
corr_mat2 <- round(cor(sleep_corr),2)
melted_corr_mat2 <- melt(corr_mat2)

corr_heat2<-ggplot(melted_corr_mat2, aes(x=Var1, y=Var2, 
                                         fill=value)) + geom_tile() +
  geom_text(aes(Var2, Var1, label = value), 
            color = "black", size = 4) + labs(title = "MIT Correlation Heatmap - Sleep Metrics",
                                              x = "Variable 1", y = "Variable 2") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_gradient(low = "lightblue",
                      high = "blue",
                      guide = "colorbar")
corr_heat2

#Shiny App Code
install.packages("fmsb")
ui <- fluidPage(
  titlePanel("Sleep and Gender"),
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Chart Title", placeholder = "Enter a title"),
      selectInput("gender", "Select Gender", choices = c("Both", "Male", "Female")),
      numericInput("sleep_duration", "Sleep Duration >=(302-528)", min=0, value=0),
      
      actionButton("resetFilters", "Reset Filters"),
    ),
    mainPanel(
      plotOutput("sleepPlot"),
      dataTableOutput("filteredData")
    )
  )
)

#Server
server <- function(input, output, session) {
  filteredData <- reactive({
    data <- sleep
    
    data <-switch(input$gender, "Both" = data, "Male" = data[data$gender == "Male", ],
                  "Female" = data[data$gender == "Female", ])
    data<- data[data$sleep_duration >= input$sleep_duration, ]
    data
  })
  
  output$filteredData <- renderDataTable({
    filteredData()
  })
  
  output$sleepPlot <- renderPlot({
    ggplot(filteredData(), aes(x = overall_score, y = sleep_duration, color = gender)) +
      geom_point(size = 4, alpha = 0.6) +
      labs(title = input$title, x = "Overall Score",
           y = "Sleep Duration (minutes)",
           color = "Gender") + 
      scale_color_manual(values = c("Male" = "Blue", "Female" = "Red"))
  })
  
  observeEvent(input$resetFilters, {
    updateSelectInput(session, "gender", selected = "Both")
    updateTextInput(session, "title", value = "Enter a title")
    updateNumericInput(session, "sleep_duration", value = 0)
  })
}

shinyApp(ui = ui, server = server)

