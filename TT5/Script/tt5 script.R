
#Tidy Tuesday 5- Washington Trails#

library(dplyr)
library(ggplot2)
install.packages("ggwordcloud")
library(ggwordcloud)
library(stringr)
library(tidytext)
install.packages("forcats")
library(forcats)

# load data#
tuesdata <- tidytuesdayR::tt_load('2020-11-24')
df <- tuesdata$hike_data

# Isolating Data#
df =  df %>% 
  mutate(region = ifelse( str_detect(location, '--'), 
                          str_extract(location, '.+(?=--)'), location))

test = df %>% group_by(region) %>% summarise(n = n()) %>% arrange(-n)


# Data Wrangling #
df2 = 
  df %>% 
  group_by(region) %>% 
  filter(n() > 100) %>% 
  unnest_tokens(word, description) %>% 
  group_by(region, word) %>% 
  summarise(n = n())

data("stop_words")

df3 = df2 %>%
  anti_join(stop_words)

df4 =  df3 %>% 
  group_by(region) %>% 
  mutate(n_prop = (n/sum(n)) * 100 )


# Plotting the Data #
df4 %>% 
  group_by(region) %>% 
  top_n(n = 12, wt = n_prop) %>% # top 10 words per region
  ggplot(aes(label = word, size = n_prop)) +
  geom_text_wordcloud(eccentricity = 1) +
  # geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) +
  facet_wrap(~region, nrow = 3) +
  # theme_minimal() +
  labs()
df5 %>%
  ungroup() %>%
  group_by(region) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = region)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~region, ncol = 3, scales = "free") +
  labs(x = "tf-idf", y = NULL)
NULL

ggsave(filename = 'Hikez.png', dpi = 300, plot = last_plot())
