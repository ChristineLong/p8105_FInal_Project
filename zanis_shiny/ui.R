#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)



year <- merged_data %>% distinct(year) %>% pull()
type_variable <- merged_data %>% distinct(type_variable) %>% pull()

# Define UI for application that draws a histogram
shinyUI(
  
  # Application title
  # titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  navbarPage("shiny_states",
  					 tabPanel("US MAP",
  					 				 fluidRow(
  					 				   sidebarPanel(
  					 				 	   selectInput("year",
  					 				 		  					label = h3("Choose year"),
  					 				 			  				choices = as.list(year),
  					 				 				  			selected = 2015)),
  					 				   mainPanel(
  					 				    	plotOutput("usmap")
  					 				 ))),
  					 tabPanel("Regression",
  					 				 radioButtons("variable_type",
  					 				 						 label = h3("Choose variable type"),
  					 				 						 choices = type_variable,
  					 				 						 selected = "law_strength")
  					 				 ),
  					 mainPanel(
  					 	h3(textOutput("test"))
  					 )
  )
)

