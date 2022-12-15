library(tidyverse)
library(googlesheets4)


cb = read_sheet("1pzvGUvjK6qV8BVYSfoc2cid88dx3v7ZOD7TnYFgyWbc", sheet = "cb") %>% mutate_all(as.character)
wle = read_sheet("1pzvGUvjK6qV8BVYSfoc2cid88dx3v7ZOD7TnYFgyWbc", sheet = "wle") %>% mutate_all(as.character)

wle2 = 
  wle %>% 
  fill(tray, analysis) %>% 
  pivot_longer(-c(tray, analysis, letter))

wle2 %>% write.csv("ferrozine-wle.csv", na = "")


