library(tidyverse)
pru <- read_csv("data/emoji_urls.csv")
map2(.x = pru$url,
     .y=pru$filename,
     function(.x,.y) safely(download.file(.x, .y)))

# I don't know why the flags errored so I did this
#pru %>% 
#  left_join(select(jis, name, group)) %>% 
#  filter(str_detect(group, "Flags")) ->tt 
#map2(.x = tt$url,
#     .y=tt$filename,
#     function(.x,.y) safely(download.file(.x, .y)))