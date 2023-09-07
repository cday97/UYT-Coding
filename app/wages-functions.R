
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
  wages_clean %>% 
    select(-BONUS) %>%
    mutate(
      Position = ifelse(is.na(Position), '', as.character(Position)),
      Name = ifelse(is.na(Name), '', as.character(Name)),
      Email = ifelse(is.na(Email), '', as.character(Email)),
      Team = ifelse(is.na(Team), '', as.character(Team)),
      PayRate = ifelse(is.na(PayRate), 0, as.numeric(PayRate)),
      Training = ifelse(is.na(Training), 0, as.numeric(Training)),
      DRIVE = ifelse(is.na(DRIVE), 0, as.numeric(DRIVE))
    )
}

# Grab the Wages Sheet
#fall23_embed_link <- '<iframe src="https://onedrive.live.com/embed?cid=4F786D9BAAACD460&resid=4F786D9BAAACD460%2116285&authkey=APUfF8vex2OTSaU&em=2" width="402" height="346" frameborder="0" scrolling="no"></iframe>'
read_wages_sheet <- function(fall23_embed_link){
  
  wages <- get_wages_table(fall23_embed_link)
  wages_clean <- clean_wages_table(wages)
  
}





# Grab Google Forms and the questions we care about
#"https://docs.google.com/spreadsheets/d/18G9ArS_7QZwekcBk5qancZwKV2wSEE-uzv8wITX16QU/edit?resourcekey#gid=246854673"
read_form_responses <- function(answer_sheet_link){
  
  #gs4_auth(path = 'C:/Users/chris/red-dominion-389619-72b641583730.json')
  #drive_auth(email="christophersday@gmail.com")
  #gs4_auth(cache = "C:/Users/chris/.secrets") #for the first time running the app in R to get the OAuth token
  gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)
  raw_sheet <- read_sheet(answer_sheet_link)
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
    mutate(Week = str_remove(Week, " \\(.*$")) %>%
    mutate(Hours = ifelse(is.na(Hours),0,as.numeric(Hours)))
  
  sheet_sum <- raw_sheet_long %>%
    group_by(Week, Name, Position) %>%
    summarize(Hours = sum(Hours)) %>%
    mutate(
      Hours = ifelse(is.na(Hours),0,as.numeric(Hours)),
      Week = ifelse(is.na(Week), '', as.character(Week)),
      Name = ifelse(is.na(Name), '', as.character(Name)),
      Position = ifelse(is.na(Position), '', as.character(Position))
    )
  
  return(sheet_sum)
  
  ## write error script that won't proceed if there are multiple "Position / Name" Combos (Like Middle School and Saige right now)
  
}

check_for_duplicates <- function(sheet_sum) {
  sheet_sum2 <- sheet_sum %>%
    mutate(dup = paste0(Position,"/",Name)) %>% 
    select(dup) %>% 
    duplicated()
  
  if (any(sheet_sum2)) {
    duplicated_rows <- sheet_sum[sheet_sum2, ]
    duplicated_combos <- with(duplicated_rows, paste(Position, Name, sep = "/"))
    error_message <- paste("The Wages Sheet includes at least one duplicated value of Position/Name combo. Please ensure there is only one Name per Position in the Wages Sheet.\n\n Duplicated values are: \n", paste("-->  ", duplicated_combos, collapse = "\n"))
    stop(error_message)
  }
}


create_advanced_wages_report <- function(sheet_sum, wages_clean, weekSelect){
  weeks2thru7 <- c("Week 2", "Week 3", "Week 4", "Week 5", "Week 6", "Week 7")
  
  wages_cleaner <- wages_clean %>%
    mutate(selectedWeek = weekSelect) %>%
    mutate(Training = ifelse(selectedWeek == 'Week 1', Training, 0),
           Drive16 = ifelse(selectedWeek %in% weeks2thru7, DRIVE / 6, 0)) %>%
    select(Position, Name, Email, Team, PayRate, Training, Drive16,DRIVE, selectedWeek)
  
  mid_advanced_wages_report <- wages_cleaner %>% 
    left_join(sheet_sum, by = c('Name','Position')) %>%
    filter(Week == weekSelect) %>%
    select(Position, Name,  Hours, Week)
    
  
  completed <- mid_advanced_wages_report %>% select(Name) %>% unique()
  completed_list <- as.list(completed$Name)
  
  final_advanced_wages_report <- wages_cleaner %>%
    left_join(mid_advanced_wages_report, by = c('Name','Position')) %>%
    mutate(Training = ifelse(is.na(Training), 0, Training),
           Hours = ifelse(is.na(Hours), 0, Hours)) %>%
    mutate(PayFinal = (Hours + Training) * PayRate + Drive16) %>%
    mutate(CompletedReport = ifelse(!(Name %in% completed_list), 'No', 'Yes')) %>%
    select(Position,Name,Email,Team,PayRate,Hours,Training,Drive16,PayFinal,CompletedReport)
  
  return(final_advanced_wages_report)
  
}


create_wages_report <- function(final_advanced_wages_report){
  final_wages_report <- final_advanced_wages_report %>%
    select(Name, Position, Hours, PayFinal)
}
create_gigwage_report <- function(final_advanced_wages_report,wages_clean, weekSelect){
  weeks2thru7 <- c("Week 2", "Week 3", "Week 4", "Week 5", "Week 6", "Week 7")
  wages_email <- wages_clean %>%
    select(Name, Email) %>% unique()
  
  gigwage_report <- final_advanced_wages_report %>%
    group_by(Name) %>%
    summarize(Hours = sum(Hours), 
              Training = sum(Training), 
              Drive16 = sum(Drive16),
              PayFinal = sum(PayFinal)) %>%
    mutate(Week = weekSelect,
           ReasonWeek1 = paste0(Hours, " coaching hours + ", Training, " training hours."),
           ReasonWeek27 = paste0(Hours, " coaching hours + $", Drive16,  " from 1/6th of drive stipend."),
           Reason7P = paste0(Hours, " coaching hours."),
           "Mark as reimbursement?" = NA,
           "Job ID" = NA) %>%
    rename("Amount" = "PayFinal") %>%
    mutate(Reason = case_when(
            Week == 'Week 1' ~ ReasonWeek1,
            Week %in% weeks2thru7 ~ ReasonWeek27,
            TRUE ~ Reason7P)) %>%
    select('Name','Amount', 'Reason', 'Mark as reimbursement?', 'Job ID')
  
  final_gigwage_report <- wages_email %>%
    left_join(gigwage_report, by = c('Name')) %>%
    mutate("First name" = sub("^(\\S+).*", "\\1", Name),
           "Last Name" = sub("^\\S+\\s(.*)", "\\1", Name)) %>%
    mutate_all(~ifelse(is.na(.), "", .)) %>%
    select('First name', 'Last Name','Email', 'Amount', 'Reason', 'Mark as reimbursement?', 'Job ID') %>%
    mutate(Amount = ifelse(Amount == '', 0, Amount)) %>%
    mutate(Amount = as.numeric(Amount))
  
  return(final_gigwage_report)
  
  
}

create_gigwage_biweek_report <- function(sheet_sum, wages_clean, weekSelect){
  weeks2thru7 <- c("Week 2", "Week 3", "Week 4", "Week 5", "Week 6", "Week 7")
  
  wages_cleaner <- wages_clean %>%
    mutate(selectedWeek = weekSelect) %>%
    mutate(Training = ifelse(selectedWeek == 'Week 1 & 2', Training, 0),
           Drive16 = case_when(
             selectedWeek == 'Week 1 & 2' ~ DRIVE / 6,
             selectedWeek == 'Week 3 & 4' ~ DRIVE / 3,
             selectedWeek == 'Week 5 & 6' ~ DRIVE / 3,
             selectedWeek == 'Week 7 & 8' ~ DRIVE / 6,
             TRUE ~ 0)) %>%
    select(Position, Name, Email, Team, PayRate, Training, Drive16,DRIVE, selectedWeek)

  wages_email <- wages_cleaner %>%
    select(Name, Email) %>% unique()
  
  sheet_sum_bw <- sheet_sum %>%
    mutate(BiWeek = case_when(
      Week %in% c('Week 1', 'Week 2') ~ 'Week 1 & 2',
      Week %in% c('Week 3', 'Week 4') ~ 'Week 3 & 4',
      Week %in% c('Week 5', 'Week 6') ~ 'Week 5 & 6',
      Week %in% c('Week 7', 'Week 8') ~ 'Week 7 & 8',
      Week %in% c('Week 9', 'Week 10') ~ 'Week 9 & 10',
      Week %in% c('Week 11', 'Week 12') ~ 'Week 11 & 12',
      TRUE ~ 'Week > 12'
    ))
  
  
  mid <- wages_cleaner %>% 
    left_join(sheet_sum_bw, by = c('Name','Position')) %>%
    filter(BiWeek == weekSelect) %>%
    select(Position, Name,  Hours, BiWeek)
  
  completed <- mid %>% select(Name) %>% unique()
  completed_list <- as.list(completed$Name)
  
  final_advanced_wages_report <- wages_cleaner %>%
    left_join(mid, by = c('Name','Position')) %>%
    mutate(Training = ifelse(is.na(Training), 0, Training),
           Hours = ifelse(is.na(Hours), 0, Hours)) %>%
    mutate(Note = ifelse(!(Name %in% completed_list), 'Did not complete report', ''))
  
  gigwage_bw_report <- final_advanced_wages_report %>%
    group_by(Name, Position) %>%
    summarize(Hours = sum(Hours),       #HACKY FIX
              Training = max(Training), #training is double counted when left joining in 'final_advanced_wages_report' object
              Drive16 = max(Drive16), # drive16 is double counted when left joining in 'final_advanced_wages_report' object
              PayRate = max(PayRate)) %>% #since its double counted, we need to single count it in a new grouping and then recalculate the PayFinal
    ungroup() %>%
    mutate(PayFinal = (Hours + Training) * PayRate + Drive16) %>% #recalculate PayFinal with newly calculated single Training and Drive16
    group_by(Name) %>%
    summarize(Hours = sum(Hours), # now complete the final summarizing of the values
              Training = sum(Training),
              Drive16 = sum(Drive16),
              PayFinal = sum(PayFinal)) %>%
    mutate(Week = weekSelect,
           ReasonWeek12 = paste0(Hours, " coaching hours + ", Training, " training hours + $", Drive16, " from drive stipend"),
           ReasonWeek26 = paste0(Hours, " coaching hours + $", Drive16,  " from drive stipend."),
           ReasonWeek78 = paste0(Hours, " coaching hours + $", Drive16,  " from drive stipend."),
           ReasonWeek9P = paste0(Hours, " coaching hours."),
           "Mark as reimbursement?" = NA,
           "Job ID" = NA) %>%
    rename("Amount" = "PayFinal") %>%
    mutate(Reason = case_when(
      Week == 'Week 1 & 2' ~ ReasonWeek12,
      Week %in% c('Week 3 & 4', 'Week 5 & 6') ~ ReasonWeek26,
      Week == 'Week 7 & 8' ~ ReasonWeek78,
      TRUE ~ ReasonWeek9P)) %>%
    select('Name','Amount', 'Reason', 'Mark as reimbursement?', 'Job ID')
  
  final_gigwage_bw_report <- wages_email %>%
    left_join(gigwage_bw_report, by = c('Name')) %>%
    mutate("First name" = sub("^(\\S+).*", "\\1", Name),
           "Last Name" = sub("^\\S+\\s(.*)", "\\1", Name)) %>%
    mutate_all(~ifelse(is.na(.), "", .)) %>%
    select('First name', 'Last Name', 'Email', 'Amount', 'Reason', 'Mark as reimbursement?', 'Job ID') %>%
    mutate(Amount = ifelse(Amount == '', 0, Amount)) %>%
    mutate(Amount = as.numeric(Amount))
  
  return(final_gigwage_bw_report)
}


  
#EXAMPLES
#sheet1 <- read_wages_sheet('<iframe src="https://onedrive.live.com/embed?cid=4F786D9BAAACD460&resid=4F786D9BAAACD460%2116285&authkey=APUfF8vex2OTSaU&em=2" width="402" height="346" frameborder="0" scrolling="no"></iframe>')
#form_responses <- read_form_responses("https://docs.google.com/spreadsheets/d/18G9ArS_7QZwekcBk5qancZwKV2wSEE-uzv8wITX16QU/edit?resourcekey#gid=246854673")
#final_adv_report <- create_advanced_wages_report(form_responses,sheet1,"Week 1")
#final_report <- create_wages_report(final_adv_report)
#final_gw_report <- create_gigwage_report(form_responses, sheet1, "Week 1")

# THIS WORKED
#https://gargle.r-lib.org/articles/non-interactive-auth.html#project-level-oauth-cache
#https://github.com/tidyverse/googlesheets4/issues/184

#drive_auth(cache = "C:/Users/chris/.secrets")
#gs4_auth(cache = ".secrets") #for the first time running the app in R to get the OAuth token
#gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)


#'THINGS TO FIX:
#'  - Make it work for salt lake as well:
#'     - Input box for onedrive link and google form sheet and other inputs?
#'     - Does the wages sheet have to be the second sheet? Or does that not matter?
#'  - Make sure outputs are correct
#'  - Bi-weekly summary option for gigwage report
#'  .
  


#test <- read_form_responses('https://docs.google.com/spreadsheets/d/1a5Uv9-JNJxwXujMxU-PPyH_lVTjwYSdR2aujcbXiQuY/edit?resourcekey#gid=451295547')

#raw_sheet_important <- read_csv('testanswers-as.csv') %>%
#  select('Name', 'What week are you reporting?', 
#         'Middle School Practice Hours', 'Middle School Match Hours', 'Middle School MAKEUP/SUBBING Section',
#         'Elementary School Practice', 'Elementary School Match', 'Elementary School MAKEUP/SUBBING SECTION', 
#         'High School Practice Hours', 'High School Match Hours', 'High School MAKEUP/SUBBING SECTION',
#         'Red Ball Lessons', 'Adult Lessons') %>%
#  rename('Week' = 2, 'Middle School-Practice' = 3, 'Middle School-Match' = 4, 'Middle School-Sub' = 5,
#         'Elementary School-Practice' = 6, 'Elementary School-Match' = 7, 'Elementary School-Sub' = 8,
#         'High School-Practice' = 9, 'High School-Match' = 10, 'High School-Sub' = 11,
#         'Red Ball' = 12, 'Adult Lessons' = 13)
#
#raw_sheet_long <- raw_sheet_important %>%
#  pivot_longer(!c(Name,Week), names_to = 'Position', values_to='Hours') %>%
#  mutate(Practice = stringr::word(Position,2,sep='-')) %>%
#  mutate(Position = stringr::word(Position,1,sep='-')) %>%
#  mutate(Week = str_remove(Week, " \\(.*$")) %>%
#  mutate(Hours = ifelse(is.na(Hours),0,as.numeric(Hours)))
#
#sheet_sum <- raw_sheet_long %>%
#  group_by(Week, Name, Position) %>%
#  summarize(Hours = sum(Hours)) %>%
#  mutate(
#    Hours = ifelse(is.na(Hours),0,as.numeric(Hours)),
#    Week = ifelse(is.na(Week), '', as.character(Week)),
#    Name = ifelse(is.na(Name), '', as.character(Name)),
#    Position = ifelse(is.na(Position), '', as.character(Position))
#  )
# old
#spring23_link <- "https://onedrive.live.com/download?cid=4F786D9BAAACD460&resid=4F786D9BAAACD460%2116178&authkey=AHSOroECuYE6pd0&em=2"
