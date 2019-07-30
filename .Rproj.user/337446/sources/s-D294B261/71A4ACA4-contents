.libPaths("H:/myCitrixFiles/Documents/R/win-library/3.2")

library(tidyverse)
library(lubridate)

video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")


ggplot(video_games, aes(median_playtime, metascore)) +
  geom_point() +
  scale_x_log10()

sum(video_games$median_playtime == 0, na.rm=T)

top_dev <- video_games %>%
  group_by(developer) %>%
  summarise(playtime = sum(median_playtime),
            games = n()) %>%
  ungroup() %>%
  arrange(desc(playtime)) %>%
  slice(1:10) 

ggplot(video_games %>% semi_join(top_dev, by=c('developer'='developer')), aes(developer, price)) +
  geom_boxplot(aes(group=developer)) +
  theme(axis.text.x = element_text(angle=90))

years <- video_games %>%
  mutate(Year = year(strptime(release_date, '%b %d, %Y'))) %>%
  separate(owners, into=c('lower','upper'), sep='\\.\\.', remove=F) %>%
  mutate(lower = gsub('[, ]', '', lower) %>% str_trim() %>% as.numeric(),
         owners = factor(owners) %>% fct_reorder(lower))
  

ggplot(years, aes(as.factor(Year), fill=owners)) +
  geom_bar(position='fill') +
  theme(axis.text.x = element_text(angle=90))

ggplot(years, aes(Year, owners)) +
  geom_bin2d()
