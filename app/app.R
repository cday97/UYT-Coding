library(DT)
library(shiny)
library(shinyjs)
library(RCurl)
library(readxl)
library(stringr)
library(tidyverse)
library(googlesheets4)
source('wages-functions.R')

ui <- fluidPage(
  shinyjs::useShinyjs(),  # Enable shinyjs
  titlePanel("UYT Wages Summary"),
  sidebarLayout(
    sidebarPanel(
      textInput("google_link", "Link to Google Form Responses:", 
                value = "https://docs.google.com/spreadsheets/d/18G9ArS_7QZwekcBk5qancZwKV2wSEE-uzv8wITX16QU/edit?resourcekey#gid=246854673"),
      textInput("outlook_embed_link", "Embed Link to Onedrive Wages Sheet:",
                value = "<iframe src=\"https://onedrive.live.com/embed?cid=4F786D9BAAACD460&resid=4F786D9BAAACD460%2116285&authkey=APUfF8vex2OTSaU&em=2\" width=\"402\" height=\"346\" frameborder=\"0\" scrolling=\"no\"></iframe>"),
      selectInput("week", "Select Week:", 
                  choices = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5", 
                              "Week 6", "Week 7", "Week 8", "Week 9", "Week 10", "Week 11", "Week 12"), #Other
                  selected = "Week 1"),
      selectInput("type", "Select Report:",
                  choices = c("Wages", "Advanced Wages", "Gigwage Weekly", "Gigwage BiWeekly"),
                  selected = "Wages"),
      downloadButton("downloadButton", "Download Table")  # Add a download button
    ),
    mainPanel(
      DTOutput("tableOutput")
    )
  )
)
server <- function(input, output, session) {
  # Create a reactiveValues to store the data
  data <- reactiveValues()
  
  # Function to read the wages sheet when input$outlook_embed_link changes
  observeEvent(input$outlook_embed_link, {
    data$sheet1 <- read_wages_sheet(input$outlook_embed_link)
  })
  
  # Function to read form responses when input$google_link changes
  observeEvent(input$google_link, {
    data$form_responses <- read_form_responses(input$google_link)
  })
  
  observe({
    # Update the choices of the "Select Week" input based on the selected report
    if (input$type != "Gigwage BiWeekly") {
      updateSelectInput(session, "week", 
                        label = "Select Week:",
                        choices = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5", 
                                    "Week 6", "Week 7", "Week 8", "Week 9", "Week 10", "Week 11", "Week 12"),
                        selected = input$week)
    } else if (input$type == "Gigwage BiWeekly") {
      # Determine the corresponding week group based on the selected week
      week_group <- switch(input$week,
                           "Week 1" = "Week 1 & 2",
                           "Week 2" = "Week 1 & 2",
                           "Week 3" = "Week 3 & 4",
                           "Week 4" = "Week 3 & 4",
                           "Week 5" = "Week 5 & 6",
                           "Week 6" = "Week 5 & 6",
                           "Week 7" = "Week 7 & 8",
                           "Week 8" = "Week 7 & 8",
                           "Week 9" = "Week 9 & 10",
                           "Week 10" = "Week 9 & 10",
                           "Week 11" = "Week 11 & 12",
                           "Week 12" = "Week 11 & 12",
                           "Week 1 & 2" = "Week 1 & 2",
                           "Week 3 & 4" = "Week 3 & 4",
                           "Week 5 & 6" = "Week 5 & 6",
                           "Week 7 & 8" = "Week 7 & 8",
                           "Week 9 & 10" = "Week 9 & 10",
                           "Week 11 & 12" = "Week 11 & 12")
      
      updateSelectInput(session, "week", 
                        label = "Select Week:",
                        choices = c("Week 1 & 2", "Week 3 & 4", "Week 5 & 6", "Week 7 & 8",
                                    "Week 9 & 10", "Week 11 & 12"), 
                        selected = week_group)
    }
  })
  
  
  tableData <- reactive({
    # Check if data is available
    if (is.null(data$sheet1) || is.null(data$form_responses)) {
      return(NULL)
    }
    
    final_adv_report <- create_advanced_wages_report(data$form_responses, data$sheet1, input$week)
    final_report <- create_wages_report(final_adv_report)
    final_gw_report <- create_gigwage_report(data$form_responses, data$sheet1, input$week)
    final_gw_bw_report <- create_gigwage_biweek_report(data$form_responses, data$sheet1, input$week)
    
    filtered_df <- final_adv_report
    if (input$type == "Wages") {
      filtered_df <- final_report
    } else if (input$type == "Advanced Wages") {
      filtered_df <- final_adv_report
    } else if (input$type == 'Gigwage Weekly') {
      filtered_df <- final_gw_report
    } else if (input$type == 'Gigwage BiWeekly')
      filtered_df <- final_gw_bw_report
    filtered_df
  })
  
  output$tableOutput <- renderDT({
    tableData()
  })
  
  output$downloadButton <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "-", tolower(input$type)), "-", gsub(" ", "-", tolower(input$week)), ".csv")
    },
    content = function(file) {
      write.csv(tableData(), file, row.names = FALSE)
    },
    contentType = "text/csv")
}

shinyApp(ui = ui, server = server)












# Copy to Clipboard button (user had to click it twice!)

#tags$head(
#  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.4/clipboard.min.js")
#),

#actionButton("copyButton", "Copy to Clipboard"),


#observeEvent(input$copyButton, {
#  js <- "
#    var table = $('#tableOutput table').get(0);
#    var clipboard = new ClipboardJS('#copyButton', {
#      text: function() {
#        var range = document.createRange();
#        range.selectNodeContents(table);
#        var selection = window.getSelection();
#        selection.removeAllRanges();
#        selection.addRange(range);
#        var text = selection.toString();
#        selection.removeAllRanges();
#        return text;
#      }
#    });
#    clipboard.on('success', function(e) {
#      e.clearSelection();
#      Shiny.setInputValue('copiedToClipboard', true, {priority: 'event'});
#    });
#  "
#  shinyjs::runjs(js)
#})

#observeEvent(input$copiedToClipboard, {
#  if (input$copiedToClipboard) {
#    shinyalert::shinyalert(
#      title = "Success!",
#      text = "Table copied to clipboard.",
#      type = "success"
#    )
#  }
#})
