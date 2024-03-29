
library(RCurl)
library(readxl)
library(stringr)
library(tidyverse)
library(googlesheets4)

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
    select(1:7,27) %>%
    tail(-2) 
  colnames(wages_clean) <- wages[2,]
  wages_clean <- wages_clean %>%
    rename('BONUS' = 'Hours') %>%
    drop_na(Name)
  wages_clean
}

# Grab the Wages Sheet
#fall23_embed_link <- '<iframe src="https://onedrive.live.com/embed?cid=4F786D9BAAACD460&resid=4F786D9BAAACD460%2116285&authkey=APUfF8vex2OTSaU&em=2" width="402" height="346" frameborder="0" scrolling="no"></iframe>'
read_wages_sheet <- function(fall23_embed_link){
  
  wages <- get_wages_table(fall23_embed_link)
  wages_clean <- clean_wages_table(wages)
  
}





# Grab Google Forms and the questions we care about
#raw_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/18G9ArS_7QZwekcBk5qancZwKV2wSEE-uzv8wITX16QU/edit?resourcekey#gid=246854673")
read_form_responses <- function(answer_sheet_link){
  raw_sheet_important <- raw_sheet %>%
    select('Name', 'What week are you reporting?', 
           'Middle School Practice Hours', 'Middle School Match Hours', 'Middle School MAKEUP/SUBBING Section',
           'Elementary School Practice', 'Elementary School Match', 'Elementary School MAKEUP/SUBBING SECTION', 
           'High School Practice Hours', 'High School Match Hours', 'High School MAKEUP/SUBBING SECTION',
           'Red Ball Lessons', 'Adult Lessons') %>%
    rename('Week' = 2, 'Middle School-Practice' = 3, 'Middle School-Match' = 4, 'Middle School-Sub' = 5,
           'Elementary School-Practice' = 6, 'Elementary School-Match' = 7, 'Elementary School-Sub' = 8,
           'High School-Practice' = 9, 'High School-Match' = 10, 'High School-Sub' = 11,
           'Red Ball' = 12, 'Adult Lessons' = 13)
  
  
  # clean up the google form answers
  raw_sheet_long <- raw_sheet_important %>%
    pivot_longer(!c(Name,Week), names_to = 'Position', values_to='Hours') %>%
    mutate(Practice = stringr::word(Position,2,sep='-')) %>%
    mutate(Position = stringr::word(Position,1,sep='-')) %>%
    mutate(Week = str_remove(Week, " \\(.*$"))
  
  sheet_sum <- raw_sheet_long %>%
    group_by(Week, Name, Position) %>%
    summarize(Hours = sum(Hours)) %>%
    mutate(Hours = ifelse(is.na(Hours),0,Hours))
}




# create three final outputs that are ready (except for the filtering of weeks) -- [Wages, AdvancedWages, Gigwage]
weeks2thru7 <- c("Week 2", "Week 3", "Week 4", "Week 5", "Week 6", "Week 7")

advanced_wages_report <-  sheet_sum %>%
  left_join(wages_clean, by = c("Name",'Position')) %>%
  select(-BONUS) %>%
  filter(Hours > 0) %>%
  mutate(PayRate = as.numeric(PayRate),
         DRIVE = as.numeric(DRIVE),
         Training = as.numeric(Training)) %>%
  mutate(Training = ifelse(is.na(Training), 0, Training),
         DRIVE = ifelse(is.na(DRIVE), 0, DRIVE)) %>%
  mutate(PayFinal = case_when(
    Week == 'Week 1' ~ ((Hours + Training) * PayRate),
    Week %in% weeks2thru7 ~ (Hours * PayRate) + (DRIVE / 6),
    TRUE ~ (Hours * PayRate)
  )) %>%
  select(Week, Name, Position, Hours, PayRate, Training, DRIVE, PayFinal)

gigwage_report <- advanced_wages_report %>%
  ungroup() %>%
  group_by(Name) %>%
  summarize(Hours = sum(Hours), 
            Training = sum(Training), 
            DRIVE = sum(DRIVE),
            PayFinal = sum(PayFinal)) %>%
  mutate(Reason = paste0(Hours, " coaching hours plus money from training, drive stipend, or bonus if applicable."),
         "Mark as reimbursement" = NA,
         "External ID" = NA) %>%
  mutate("First Name" = sub("^(\\S+).*", "\\1", Name),
         "Last Name" = sub("^\\S+\\s(.*)", "\\1", Name)) %>%
  rename("Amount" = "PayFinal") %>%
  select('Name','First Name', 'Last Name', 'Amount', 'Reason', 'Mark as reimbursement', 'External ID')
  

wages_cleaner <- wages_clean %>%
  select(Position, Name, Email, Team, PayRate, BONUS)
wages_email <- wages_cleaner %>%
  select(Name, Email, BONUS) %>% unique()



# test user selections
weekSelect = 'Week 1'
sheetSelect = 'Gigwage'

#get_final_table <- function(weekSelect, sheetSelect){

filt_advanced_wages_report <- advanced_wages_report %>%
  ungroup() %>%
  filter(Week == weekSelect) %>%
  select(-Week)

final_advanced_wages_report <- wages_cleaner %>%
  mutate(PayRate = as.numeric(PayRate)) %>%
  left_join(filt_advanced_wages_report, by = c('Name', 'Position', 'PayRate')) %>%
  mutate(BONUS = as.numeric(BONUS)) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(weekSelect2 = weekSelect) %>%
  mutate(PayFinal = ifelse(weekSelect2 == 'Week 11', PayFinal + BONUS, PayFinal)) %>%
  select(Position, Name, Email, Team, PayRate, Hours, Training, DRIVE, BONUS, PayFinal)
  
final_wages_report <- final_advanced_wages_report %>%
  select(Name, Position, PayFinal)

final_gigwage_report <- wages_email %>%
  left_join(gigwage_report, by = c('Name')) %>%
  mutate(BONUS = as.numeric(BONUS)) %>%
  mutate(BONUS = ifelse(is.na(BONUS), 0, BONUS)) %>%
  mutate(weekSelect2 = weekSelect) %>%
  mutate(Amount = ifelse(weekSelect2 == 'Week 11', Amount + BONUS, Amount)) %>%
  select(-BONUS,-weekSelect2)

  
#}










double_checkers <- pay_rate %>%
  filter(is.na(Pay)) %>%
  select(Name,Position,Hours)


























# old
#spring23_link <- "https://onedrive.live.com/download?cid=4F786D9BAAACD460&resid=4F786D9BAAACD460%2116178&authkey=AHSOroECuYE6pd0&em=2"
