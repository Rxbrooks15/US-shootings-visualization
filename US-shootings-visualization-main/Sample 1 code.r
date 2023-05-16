#Sample 1 code intended for DALI application 
#Name: Raine Brookshire 
#Date: 5/15/23
#Data visualization: 2 Animated plots highlighting the rise in the lethality and prevelence of school shootings 

#Libraries 
library(readxl)
library(tidyverse)
library(gganimate)
library(readr)
library(readxl)
library(gridExtra)
# install.packages("rvest")
library(rvest)
library(sf)
library(sp)
library(magick)
library(jpeg)
library(lubridate)
library(urbnmapr)
library(cr)
install.packages(cr)
library(gifski)
library(ggtext)
library(ggplot2)

shootings#shootings V5
shootings %>% 
  View
glimpse(shootings)
str(shootings)
summary(shootings)

class(shootings$`Total victims`)

US_sf <- st_read(file.choose())#states shapefile 

#Refined code 
shootings <- read_csv("C:\\Users\\raine\\OneDrive\\Documents\\Adv_data_viz\\Mass_shootings\\Mass Shootings Dataset Ver 5.csv")
#shootings 5
US_sf <- st_read("C:\\Users\\raine\\OneDrive\\Documents\\Winter_Practice\\STATES\\States_shapefile.shp")


shootings <- shootings %>%
  mutate(year = substr(Date, start = nchar(Date) - 3,
                       stop = nchar(Date))) %>% 
  # filter(year >= 2009) %>% 
  #useful for timeline of dots over the year 
  mutate(ordered_date = mdy(Date)) %>% 
  mutate(race_group = case_when(
    #maps Scattered race groups into one clear identifier 
    Race %in% c("white", "White American or European American") ~ "White",
    Race == "White American or European American/Some other Race" ~ "White",
    Race %in% c("Asian American", "Asian American/Some other race") ~ "Asian",
    Race %in% c("black","Black American or African American", "Black", "Black American or African American/Unknown") ~ "Black",
    Race %in% c("Some other race", "Two or more races", "Unknown", NA) ~ "Other",
    Race == "Native American or Alaska Native" ~ "Native",
    TRUE ~ Race
  )) 

# Remove rows with missing latitude or longitude values
shootings <- shootings[complete.cases(shootings[c("Latitude", "Longitude")]),]

# Convert the shootings data frame to an sf object
shootings_sf <- st_as_sf(shootings, coords = c("Longitude", "Latitude"),crs = 4326)

#Count the number of shootings per year to use in animated line graph 
count_per_year <- shootings %>%
  filter(year<= 2016) %>% 
  count(year) %>%
  rename(Dots = n) %>%
  mutate(year = as.integer(year))

shooting_number <- count_per_year %>% 
  ggplot()+
  geom_point(aes(x = year, y = Dots))+
  geom_line(aes(x = year, y = Dots))+
  labs(title = "Number of shootings per year",
       x = "Year",
       y = "Number of shootigs")+
  theme_minimal()+
  #transition over the years
  transition_reveal(year)

shooting_number

b_gif <- animate(shooting_number,
        fps = 5,
        duration = 5,
        width = 900,
        renderer = gifski_renderer("~/Desktop/animation2.gif"))#save to Desktop foler 

rain_color_palette <- c("#0072B2", "#009E73", "lightgrey", "#D55E00", "#CC79A7", "#E69F00")#Personal palette for visualization

#Actual working code 
animated_shooting <- shootings %>% 
  ggplot() +
  geom_sf(data = US_sf, fill = "#012564", color = "white")+
  # geom_sf(data = shootings_sf,color = "red", aes(size =`Total victims`)) +
  geom_point(data = shootings, aes(x = Longitude, y = Latitude, group = year, size = as.numeric(`Total victims`),
                                   color = race_group, stroke = 1))+
  #theme earlier is better because having it later messes with title 
  theme_void()+

  coord_sf() +

  labs(title = "Racial influence and impact of all mass shootings from <span style='color:#FF0000'>1966 - 2017",
       x = NULL,
       y = NULL,
       subtitle = '\nDot size indicates the relative number of people shot in each mass shooting. \n \n Year: {closest_state}',
       color = "Race") +
  theme(plot.title = element_markdown(lineheight = 1.1, 
                                      size = 20,
                                      color  = "#012564",
                                      hjust = 0.5,
                                      face = "bold",
                                      family = "Times New Roman"))+
  # Dots per year: {count_per_year %>% filter(year == as.integer(frame_time)) %>% pull(dots)}
  # , paste("Year:", count_per_year$year, ", Number of dots:", count_per_year$n)'
  guides(size = FALSE,legend = FALSE)+
  
  # theme(plot.title = element_markdown(hjust = 0.5, color = "#012564"))+
  theme(plot.subtitle = element_text(hjust = 0.5, color  = "#012564" ))+
  theme(plot.caption = element_text(hjust = 0.5, color  = "#012564" ))+

  scale_color_manual(values = rain_color_palette)+
  #Increases scale of points on map to show how size of dot is related to number of victims 
  scale_size_continuous(range = c(1, 15))+
  theme(legend.position = "bottom",  # Move the legend to the bottom
        legend.box = "horizontal")+  # Display the legend items in a horizontal layout
  


  transition_states(ordered_date)+
  shadow_mark()

a_gif <- animate(animated_shooting,
                 fps = 5,
                 duration = 5,
                 width = 900,
                 renderer = gifski_renderer("~/Desktop/animation.gif"))


a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)


new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = TRUE)
for(i in 2:25){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = TRUE)
  new_gif <- c(new_gif, combined)
}
new_gif


save_path <- "~/Desktop/new_animation.gif"

# Save the new_gif animation
image_write(new_gif, path = save_path)
