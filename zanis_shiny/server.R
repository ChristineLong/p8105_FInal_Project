#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)
library(flexdashboard)

merged_firearm_mortality <-
	read_csv("./shiny_dataset/merged_firearm_mortality.csv") %>%
	select(state_abb, year, crude_rate, deaths, population, law_strength) %>%
	rename(state = state_abb) %>% 
	unite(col = state_year, state, year, sep = "_")

gun_violence <- read_csv("./shiny_dataset/gun_violence_tidy.csv") %>% 
	sample_n(size = 25000)

gun_violence_merged <- read_csv("./shiny_dataset/gun_violence_tidy.csv") %>%
	group_by(state, year) %>%
	summarize(total_killed = n(), total_injured = n()) %>% 
	unite(col = state_year, state, year, sep = "_")

unempl <- read_csv("./shiny_dataset/unempl.csv") %>% 
	unite(col = state_year, state, year, sep = "_")

merged_data <- full_join(x = gun_violence_merged, y = unempl, by = "state_year")
merged_data <- full_join(x = merged_data, y = merged_firearm_mortality, by = "state_year")
merged_data_wide <- merged_data %>% 
	separate(col = state_year, into = c("state", "year"), sep = "_")

merged_data <- merged_data_wide %>% 
	gather(key = type_variable, value = value, total_killed:law_strength) %>% 
	mutate(year = as.numeric(year)) %>% 
	arrange(year)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$usmap <- renderPlotly({
  	
  	tibble(
  		x = rnorm(n = sample(x = 1:100, replace = TRUE, size = 100)),
  		y = rnorm(n = 100)
  	) %>% 
  	ggplot(aes(x = x, y = y)) +
  		geom_point()
  	
  	# geo1 <- list(
  	# 	scope = "usa",
  	# 	projection = list(type = "state"),
  	# 	showlakes = TRUE,
  	# 	lakecolor = toRGB("white")
  	# )
  	# 
  	# merged_data %>% 
  	# 	filter(type_variable == input$variable_type, year == input$year) %>% 
  	# 	plot_geo(locationmode = "USA-states") %>% 
  	# 	add_trace(
  	# 		z = ~value,
  	# 		locations = ~state,
  	# 		color = ~value,
  	# 		colors = "Reds"
  	# 	) %>%
  	# 	# add_markers(
  	# 	# 	data = gun_violence %>% filter(year == input$year),
  	# 	# 	y = ~latitude,
  	# 	# 	x = ~longitude,
  	# 	# 	alpha = 0.3,
  	# 	# 	showlegend = FALSE) %>%
  	# 	layout(
  	# 		geo = geo1
  	# 	)
  })
  
})
