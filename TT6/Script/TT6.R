
#Loading Libraries#

library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# Oragnizing Data #

tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)
read_TTdata(week = week_num)

season_ns <-
  tuesdata %>%
  friends_emotions %>%
  group_by(season, episode) %>%
  summarise(season_n = n())

# On to Plotting #

plot <-
  friends_emotions %>%
  group_by(season, episode, emotion) %>%
  left_join( tuesdata %>%
               friends_emotions %>%
               group_by(season, episode) %>%
               summarise(season_n = n())
             , by = c('season', 'episode')) %>%
  mutate( season = paste0("Season ", season)) %>%
  ggplot() +
  geom_bar(aes(factor(episode), perc, fill = emotion),
           stat = 'identity',
           col = 'grey80') +
  facet_wrap(. ~ season, nrow = 1) +
  scale_fill_brewer('Utterance Emotion',
                    palette = 'Set1',
                    direction = -1) +
  scale_y_continuous(expand = c(0.2, 0), labels = scales::percent) +
  coord_polar() +
  labs(title = "F.R.I.E.N.D.S") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(family = 'Calibri'),
    axis.text.y = element_text(color = 'red', face = 'bold'),
    legend.title = element_text(face = 'bold'),
    axis.title = element_text(size = 13, family = 'Arial'),
    legend.position = 'top',
    legend.key.width = unit(1, 'cm'),
    legend.box = 'horizontal',
    strip.text = element_text(
      size = 14,
      family = 'Arial',
      face = 'bold'),
    plot.title = element_text(
      size = 70,
      family = 'Chiller',
      face = 'bold.italic',
      hjust = 0.5))

ggsave(here("Tidy Tuesday 6", "Output", "Friendz.png"))
