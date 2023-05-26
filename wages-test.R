
library(RCurl)
library(readxl)



# Grab the Wages Sheet

spring23_link <- "https://onedrive.live.com/download?cid=4F786D9BAAACD460&resid=4F786D9BAAACD460%2116178&authkey=AHSOroECuYE6pd0&em=2"

temp.file <- paste(tempfile(),".xlsx",sep = "")
download.file(spring23_link, temp.file, mode = "wb")

wages <- read_excel(temp.file, sheet="Wages")



# Grab Google Forms

