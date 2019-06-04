#fire up the machine

library(tidyverse)

#get data

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")


by_style <- ramen_ratings %>% 
  group_by(style) %>% 
  summarise(style_rating = mean(stars, na.rm = T), count = n()) %>% 
  arrange(desc(count))

#we find 9 different styles. most poular styles are "Pack", "Bowl" and 



##look at countries
by_country <- ramen_ratings %>% 
  group_by(country) %>% 
  arrange(country)


#install.packages("stringr") to rename misnamed or doubled entries
library(stringr)

by_country <- ramen_ratings %>% 
  mutate(country = stringr::str_replace(country, "United States", "USA"), 
         country = stringr::str_replace(country, "Holland", "Netherlands"), 
         country = stringr::str_replace(country, "Phillipines", "Philippines")) %>% 
  group_by(country) %>% 
  summarise(country_rating = mean(stars, na.rm = T), count = n()) %>% 
  filter(count > 20) %>% 
  arrange(desc(country_rating))


#18 countries have more than 20 reviews


##filter raw data for countries with > 20 ramen reviews
ramen_ratings_top_countries <- ramen_ratings %>% 
  mutate(country = stringr::str_replace(country, "United States", "USA"), 
         country = stringr::str_replace(country, "Holland", "Netherlands"), 
         country = stringr::str_replace(country, "Phillipines", "Philippines")) %>% 
  group_by(country) %>% 
  filter(n() > 20, style != "NA", style != "Bar"| style != "Can")



library(ggdark) #for dark_mode-plots. I like them



ggplot(data = ramen_ratings_top_countries, mapping = aes(x = fct_reorder(country, stars, fun = mean, na.rm = T), y = stars)) + 
  coord_flip() + geom_boxplot() + geom_jitter( aes(color = style), size = 1, height = 0.1, alpha  = 0.3) +
  labs(title = "Top ramen-producing countries (n> 20)", x = "Country", y = "Rating(stars)", caption = "M.v.R., 2019") + dark_theme_bw()
