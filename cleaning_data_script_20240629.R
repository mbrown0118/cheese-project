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

## Create texture and fat content dataframe
texture_pivot <- cheeses_new %>% 
  select(cheese, texture, fat_content_new) %>% 
  filter(!is.na(fat_content_new)) %>% 
  filter(!is.na(texture)) %>% 
  separate_longer_delim(texture, ", ") %>% 
  filter(texture %in% c("smooth", "supple", "firm", "crumbly", "creamy", "soft"))


ggplot(texture_pivot, aes(x = texture, y = fat_content_new)) +
  geom_dotplot(binaxis = "y", stackdir = "centerwhole", dotsize = .8)



## Create type and fat content dataframe
type_pivot <- cheeses_new %>% 
  select(cheese, type, fat_content_new) %>% 
  filter(!is.na(fat_content_new)) %>% 
  filter(!is.na(type)) %>% 
  separate_longer_delim(type, ", ") %>% 
  filter(type %in% c("hard", "semi-hard", "semi-soft", "soft"))


ggplot(type_pivot, aes(x = type, y = fat_content_new)) +
  geom_dotplot(binaxis = "y", stackdir = "centerwhole", dotsize = .8)


## Create cheese family and fat content dataframe
family_pivot <- cheeses_new %>% 
  select(cheese, family, fat_content_new) %>% 
  filter(!is.na(fat_content_new)) %>% 
  filter(!is.na(family)) %>% 
  separate_longer_delim(family, ", ") %>% 
  filter(family %in% c("Blue", "Brie", "Cheddar", "Gouda"))


ggplot(family_pivot, aes(x = family, y = fat_content_new)) +
  geom_dotplot(binaxis = "y", stackdir = "centerwhole", dotsize = .8)


## Create milk and fat content dataframe
milk_pivot <- cheeses_new %>% 
  select(cheese, milk, fat_content_new) %>% 
  filter(!is.na(fat_content_new)) %>% 
  filter(!is.na(milk)) %>% 
  separate_longer_delim(milk, ", ") %>% 
  filter(milk %in% c("cow", "goat", "sheep"))


ggplot(milk_pivot, aes(x = milk, y = fat_content_new)) +
  geom_dotplot(binaxis = "y", stackdir = "centerwhole", dotsize = .8)
