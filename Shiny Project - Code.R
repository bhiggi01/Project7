# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# DATA8010 - Assignment 2
# Brian Higgins
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

rm(list=ls())


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# EDA/ Data Manipulation
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Libraries
# -----------------------------------------------------------------------------

#install.packages("dplyr")
library(dplyr)

# used to combine levels
#install.packages("forecats")
library(forcats)

# Shiny library
library(shiny)

# ggplot for plots
library(ggplot2)

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------

data <- read.csv("STAT8010_2022_assignment2_2022.csv")

# drop Make as it has 2053 factors and is slow loading.
data = subset(data, select = -c(Model) )

# -----------------------------------------------------------------------------
# Explore Data
# -----------------------------------------------------------------------------
# Look at the data
str(data)
summary(data)

# -----------------------------------------------------------------------------
# Change All character types into as.factor
# -----------------------------------------------------------------------------
# Lload data
data <- data %>%
  mutate_if(sapply(data, is.character), as.factor)


# -----------------------------------------------------------------------------
# Data Manipulation
# -----------------------------------------------------------------------------
# 1. Collapse Class of vehicle into fewer categories
# 2. Collapse Transmission into fewer categories
# 3. Make Fuel types a better descripter

# 1. Collapse Class of vehicle into fewer categories

data$Vehicle.Class
levels(data$Vehicle.Class) # look at levels of cars

# collapse Compact
data$Vehicle.Class <- fct_collapse(data$Vehicle.Class, 
                                   "Compact" = c("MINICOMPACT","SUBCOMPACT",
                                                 "COMPACT"))

# collapse Tourism
data$Vehicle.Class <- fct_collapse(data$Vehicle.Class, 
                                   "Toursim" = c("MID-SIZE", "STATION WAGON - MID-SIZE",
                                                 "STATION WAGON - SMALL", "FULL-SIZE"))

# collapse Van
data$Vehicle.Class <- fct_collapse(data$Vehicle.Class, 
                                   "Van" = c("MINIVAN","VAN - CARGO",
                                             "VAN - CARGO", "VAN - PASSENGER"))

# collapse pickup
data$Vehicle.Class <- fct_collapse(data$Vehicle.Class, 
                                   "Pickup" = c("PICKUP TRUCK - SMALL",
                                                "PICKUP TRUCK - STANDARD"))

# collapse special
data$Vehicle.Class <- fct_collapse(data$Vehicle.Class, 
                                   "Special" = c("SPECIAL PURPOSE VEHICLE"))

# collapse suv
data$Vehicle.Class <- fct_collapse(data$Vehicle.Class, 
                                   "SUV" = c("SUV - STANDARD","SUV - SMALL"))

# collapse Sport
data$Vehicle.Class <- fct_collapse(data$Vehicle.Class, 
                                   "Sport" = c(""))



# 2. Collapse Transmission into fewer categories
levels(data$Transmission) # look at levels of cars

# collapse Manual
data$Transmission <- fct_collapse(data$Transmission, 
                                  "Manual" = c("M5", "M6", "M7" ))

# collapse Automatic
data$Transmission <- fct_collapse(data$Transmission, 
                                  "Automatic" = c("A10", "A4", "A5", "A6", "A7", 
                                                  "A8", "A9", "AM5", "AM6", "AM7", 
                                                  "AM8", "AM9", "AS10", "AS4" , "AS5",
                                                  "AS6", "AS7", "AS8","AS9","AV", 
                                                  "AV10", "AV6", "AV7", "AV8")) 


# 3. make Fuel type levles with a better descriptor
levels(data$Fuel.Type) <- c("Diesel","Ethanol","Natural Gas", 
                               "Regular Gasoline", "Premium Gasoline")


#  Change all int columns to as.numeric column types
#  Did this as a test to see if it speed up loading plots
#data <- data %>%
#  mutate_if(sapply(data, is.integer), as.)

# --------------
# --------------
# Variable List
# --------------
# varaible lists, removed Model as id has 2053 factors and its slowing up my computer.
cat_name = list("Make", "Vehicle.Class", "Transmission", "Fuel.Type")
num_name = list("Engine.Size.L.", "Cylinders", "Fuel.Consumption.City..L.100.km.",
             "Fuel.Consumption.Hwy..L.100.km.", "Fuel.Consumption.Comb..L.100.km.",
             "Fuel.Consumption.Comb..mpg.", "CO2.Emissions.g.km.")

str(data)

all_the_colours = c("red","blue","green")
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Shiny UI Code
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


# Define UI ----
ui1 <- fluidPage(
  
  # App title ----
  titlePanel("Project 2: Shiny Dashboard"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      # Input: Dropdown list for the variables to plot ----
      selectInput('x', 'X - Axis', names(data), names(data)[[2]]),
      selectInput('y', 'Y - Axis', names(data), names(data)[[1]]),
      
      # Space between Dropdown and buttons
      br(),
      
      # Input: Select the check box options ----
      checkboxInput("lm_line", "Show Regression Line"),
      checkboxInput("factor_colours", "Show Factors by Colours", FALSE),
        conditionalPanel(condition="input.factor_colours==true",   
        selectInput("typeInput", "Show Factors by Colour",
                    choices = cat_name)),
      
    ),

    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset with plots and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot of Predicators",
                           h3("Plot Generation"),
                           plotOutput("plotxy"),
                           textOutput("text")),
                  tabPanel("Histogram",
                           h3("Numeric Histograms"),
                           # SLider bar
                           sliderInput( "bins",
                                        "Number of bins:",
                                        min = 1,
                                        max = 30,
                                        value = 15),
                           
                           # Histogram of x axis
                           plotOutput("plotx"),
                           # Histogram of x axis
                           plotOutput("ploty"),
                           
                           
                           
                           ),
                  tabPanel("Summary", 
                           h3("Summary Infomation"),
                           tableOutput("table"))
      )
    )
  )
)

# Define server logic for app ----
server1 <- function(input, output) {
  
     # Tab 1: Plots for columns
     output$plotxy <- renderPlot({

    #-----------------------------
    # Numeric Columns: Scatter Plot
    if(((input$x %in% num_name)  & (input$y %in% num_name)) | 
       ((input$y %in% num_name)  & (input$x %in% num_name))){
     
      p <- ggplot(data, aes(data[input$x][[1]],y=data[input$y][[1]])) +
              geom_point() +
              ggtitle(paste("Scatter Plot of", input$y, "vs", input$x)) +
              xlab(input$x)+
              ylab(input$y)+
              theme_bw()

    # Let factors change colours.
    if(input$factor_colours) {
        p <-p + geom_point(aes_string(color=input$typeInput))
      }  
      
    # Add regression line, and text to plot. 
    if(input$lm_line) {
      lin_reg <- lm(get(input$y) ~ get(input$x), data)
      lin_reg_co <-lin_reg$coefficients[1]
      lin_reg_slope <-lin_reg$coefficients[2]
      
      custom <- (paste("The coeffient is", round(lin_reg_co,3),"\n",
                       " and the slope is ",round(lin_reg_slope,3)))
      
      p <- p + geom_smooth(method = "lm") +
        annotate(geom="text", x=6, y=0, label=(paste(custom)),
                 color="red")
    
    }
      print(p)
    }
    
    #----------------------------
    # Categorical Columns: Count Barplot
       # Categorical Columns: Count Barplot
        if(((input$x %in% cat_name)  & (input$y %in% cat_name)) | 
          ((input$y %in% cat_name)  & (input$x %in% cat_name))){
          
         counts <- table(data[input$y][[1]],data[input$x][[1]])
          p <- barplot(counts, 
                      col=c("lightblue","lightpink1", "gold1","palegreen","salmon2","yellow2","lavender"),
                      legend = rownames(counts), beside=TRUE,
                       xlab = input$x, ylab=input$y,
                      main = paste("Barplot of", input$y, "vs", input$x))
          
          if(input$lm_line) {
            showNotification("No Regression Line for this Plot.", type="message")
          }
          
          if(input$factor_colours) {
            showNotification("No Factor Colour changes for this Plot.", type="message")
          } 
         
         
      }
       
    #-----------------------------
    # One Categorical and one Numeric: Boxplot
   if(((input$x %in% cat_name)  & (input$y %in% num_name)) | 
       ((input$x %in% num_name)  & (input$y %in% cat_name))){
     
     p <- boxplot(as.formula(paste(data[input$y]," ~ ",data[input$x])),
            col=c("lightblue","lightpink1", "gold1","palegreen","salmon2","yellow2","lavender"),
             xlab = input$x, ylab=input$y,
             main = paste("Boxplot of", input$y, "vs", input$x))
    
      if(input$lm_line) {
        showNotification("No Regression Line for this Plot.", type="message")
       }
    
      if(input$factor_colours) {
        showNotification("No Factor Colour changes for this Plot.", type="message")
    }
    
    }
      } )
  
    #-----------------------------
    # Tab 2: histogram of x variable
    output$plotx <- renderPlot({
      
      # Plot the histogram if numeric column is choose.
      if(input$x %in% num_name){
      hist(data[input$x][[1]], 
           xlab = input$x, 
           main = paste("Histogram of", input$x),
           breaks=input$bins,
           col=c("lightblue","lightpink1", "gold1","palegreen","salmon2","yellow2","lavender"))}
      # Looked at different ways to write an error message if categorically was choose
      # Could leave it blank. Will decide later.
      else{
        plot(1,1,col="white")
        text(1,1," Plesase choose a numeric column")
      }
      
      if(input$lm_line) {
        showNotification("No Regression Line for this Plot.", type="message")
      }
      
      if(input$factor_colours) {
        showNotification("No Factor Colour changes for this Plot.", type="message")
      }
      
  })
    
    #-----------------------------
    # Tab 2 Histogram of x variable
    output$ploty <- renderPlot({
      
      if(input$y %in% num_name){
      hist(data[input$y][[1]], 
           xlab = input$y, 
           main = paste("Histogram of", input$y),
           breaks=input$bins,
           col=c("lightblue","lightpink1", "gold1","palegreen","salmon2","yellow2","lavender"))}
      else{
        plot(1,1,col="white")
        text(1,1," Plesase choose a numeric column")
      }
    })
  
    #-----------------------------
    #Tab 3: Summary Tab
    output$table <- renderTable({
      
    matrix(c(data[input$x][[1]],data[input$y][[1]]), ncol = 2,
           dimnames = list(rownames(data),c(input$x,input$y)))
  
  })
   
}

#generate app
shinyApp(ui1, server1)