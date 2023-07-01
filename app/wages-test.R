
library(RCurl)
library(readxl)
library(stringr)
library(tidyverse)

#input 1: wages sheet embed link (right click on wages scheet in google drive and put in the link it generates when you click "embed")

# function that takes the embed link to the wages onedrive sheet, downloads it, and puts it into a table
get_wages_table <- function(embed_link){
  extract <- str_extract(embed_link, "(?<=<iframe src=\").*(?=\" width)")
  link <- gsub("embed", "download", extract)
  
  temp.file <- paste(tempfile(),".xlsx",sep = "")
  download.file(link, temp.file, mode = "wb")
  
  wages <- read_excel(temp.file, sheet="Wages")
  wages
}

# function that cleans the table to something usable
clean_wages_table <- function(wages){
  wages_clean <- wages %>%
    select(1:6) %>%
    tail(-2) 
  colnames(wages_clean) <- wages[2,]
  wages_clean <- wages_clean %>%
    drop_na(Name)
  wages_clean
}



# Grab the Wages Sheet
fall23_embed_link <- '<iframe src="https://onedrive.live.com/embed?cid=4F786D9BAAACD460&resid=4F786D9BAAACD460%2116285&authkey=APUfF8vex2OTSaU&em=2" width="402" height="346" frameborder="0" scrolling="no"></iframe>'
wages <- get_wages_table(fall23_embed_link)
wages_clean <- clean_wages_table(wages)





# Grab Google Forms
library(googlesheets4)
raw_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/18G9ArS_7QZwekcBk5qancZwKV2wSEE-uzv8wITX16QU/edit?resourcekey#gid=246854673")

raw_sheet_important <- raw_sheet %>%
  select('Name', 'What week are you reporting?', 
         'Middle School Practice Hours', 'Middle School Match Hours', 'Middle School MAKEUP/SUBBING Section',
         'Elementary School Practice', 'Elementary School Match', 'Elementary School MAKEUP/SUBBING SECTION', 
         #'High School Practice', 'High School Match', 'High School MAKEUP/SUBBING SECTION',
         'High School Practice Hours', 'High School Match Hours', 'High School MAKEUP/SUBBING SECTION',
         'Red Ball Lessons', 'Adult Lessons') %>%
  rename('Week' = 2, 'Middle School-Practice' = 3, 'Middle School-Match' = 4, 'Middle School-Sub' = 5,
         'Elementary School-Practice' = 6, 'Elementary School-Match' = 7, 'Elementary School-Sub' = 8,
         'High School-Practice' = 9, 'High School-Match' = 10, 'High School-Sub' = 11,
         'Red Ball' = 12, 'Adult Lessons' = 13)

raw_sheet_long <- raw_sheet_important %>%
  pivot_longer(!c(Name,Week), names_to = 'Position', values_to='Hours') %>%
  mutate(Practice = stringr::word(Position,2,sep='-')) %>%
  mutate(Position = stringr::word(Position,1,sep='-'))


sheet_sum <- raw_sheet_long %>%
  group_by(Name, Week, Position) %>%
  summarize(Hours = sum(Hours)) %>%
  mutate(Hours = ifelse(is.na(Hours),0,Hours))




pay_rate <- sheet_sum %>%
  left_join(wages_clean, by = c("Name",'Position')) %>%
  filter(Hours > 0) %>%
  mutate(PayRate = as.numeric(PayRate)) %>%
  mutate(Pay = Hours * PayRate)

double_checkers <- pay_rate %>%
  filter(is.na(Pay)) %>%
  select(Name,Position,Hours)






























# old
#spring23_link <- "https://onedrive.live.com/download?cid=4F786D9BAAACD460&resid=4F786D9BAAACD460%2116178&authkey=AHSOroECuYE6pd0&em=2"
