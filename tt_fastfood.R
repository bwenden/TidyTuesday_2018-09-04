setwd("~/TidyTuesday/2018-09-04")
library(tidyverse)
library(ggiraphExtra)


###Fastfood data
data <- read.csv2("fastfood_calories.csv", sep=",", dec=".") %>%
  select(-X,-salad) %>%
  

###plot average calories per entree item for each restaurant
data %>% transform(restaurant=fct_reorder(restaurant, calories, .fun=mean)) %>%
  ggplot( aes(x=restaurant, y=calories)) +
  theme_bw() + 
  theme(legend.position="none",
        axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        strip.text = element_text(size=14),
        plot.title = element_text(size = 18))+
  labs(x="", y="Average calories per item")+
    stat_summary(fun.y="mean", geom="bar", aes(fill=restaurant))

####Average per restaurant
data_rest <- data %>%
  select(restaurant, calories, total_fat, total_carb, sugar,
         protein, fiber, calcium, vit_a, vit_c) %>%
  group_by(restaurant) %>%
  summarise_all(funs(mean(.,na.rm=TRUE))) %>%
  rename(carb=total_carb,fat=total_fat) %>%
  transform(restaurant=fct_reorder(restaurant, calories, .fun=mean)) 
  
#Spider plot
ggRadar(data=data_rest, aes(color=restaurant))+
  theme_bw() + 
  theme(legend.position="none",
          axis.text = element_text(size=14),
          strip.text = element_text(size=14),
          plot.title = element_text(size = 18)) +
  facet_wrap(~restaurant, nrow=2) 

#Animated spider plot
library(gganimate)
ggRadar(data=data_rest, aes(color=restaurant))+
  ggtitle('{closest_state}')+
  theme_bw() + 
  theme(legend.position="none",
        axis.text = element_text(size=14),
        strip.text = element_text(size=14),
        plot.title = element_text(size = 18)) +
  transition_states(restaurant, transition_length=1, state_length=1)+
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')
