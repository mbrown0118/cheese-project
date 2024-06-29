library(tidyverse)
library(summarytools)
library(janitor)

## Import Data set
cheeses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-04/cheeses.csv')

## Examine Data set
summarytools::dfSummary(cheeses)

## Clean up fat_content variable

### isolate fat_content variable
fat_content <- cheeses %>% 
  select(cheese, fat_content) %>% 
  filter(!is.na(fat_content))

### create variable identifying what format the fat content is in for each cheese
fat_content_tidy <- fat_content %>% 
  mutate(fat_content_format = case_when(
    str_detect(fat_content, "g") ~ "grams",
    str_detect(fat_content, "-") ~ "percent range",
    str_detect(fat_content, "%") ~ "percent",
    TRUE ~ NA
  ))

### create a new fat content variable with all cheeses in the same numeric format
fat_content_tidy <- fat_content_tidy %>% 
  mutate(fat_content_new = case_when(
    fat_content_format == "grams" ~ as.numeric(str_remove(fat_content, " g/100g"))/100,
    fat_content_format == "percent" ~ as.numeric(str_remove(fat_content, "%"))/100,
    fat_content_format == "percent range" ~ as.numeric(str_remove(fat_content, "-\\d{2}%"))/100,
    TRUE ~ NA
  )) %>% 
  select(cheese, fat_content_new)

### join the new fat content variable back with the main cheese dataframe
cheeses_new <- cheeses %>% 
  left_join(., fat_content_tidy, by = join_by(cheese)) %>% 
  relocate(fat_content_new, .after = fat_content)





