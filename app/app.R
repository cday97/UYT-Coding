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
      selectInput("week", "Select Week:", 
                  choices = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5", 
                              "Week 6", "Week 7", "Week 8", "Week 9", "Week 10", "Week 11"), #Other
                  selected = "Week 1"),
      selectInput("type", "Select Report:",
                  choices = c("Wages", "Advanced Wages", "Gigwage"),
                  selected = "Wages"),
      downloadButton("downloadButton", "Download Table")  # Add a download button
    ),
    mainPanel(
      DTOutput("tableOutput")
    )
  )
)

server <- function(input, output) {
  tableData <- reactive({
    
    # Perform filtering and create a sample table
    sheet1 <- read_wages_sheet('<iframe src="https://onedrive.live.com/embed?cid=4F786D9BAAACD460&resid=4F786D9BAAACD460%2116285&authkey=APUfF8vex2OTSaU&em=2" width="402" height="346" frameborder="0" scrolling="no"></iframe>')
    form_responses <- read_form_responses("https://docs.google.com/spreadsheets/d/18G9ArS_7QZwekcBk5qancZwKV2wSEE-uzv8wITX16QU/edit?resourcekey#gid=246854673")
    final_adv_report <- create_advanced_wages_report(form_responses,sheet1,input$week)
    final_report <- create_wages_report(final_adv_report)
    final_gw_report <- create_gigwage_report(form_responses, sheet1, input$week)
    
    filtered_df <- final_adv_report
    if (input$type == "Wages"){filtered_df <- final_report}
    if (input$type == "Advanced Wages"){filtered_df <- final_adv_report}
    if (input$type == 'Gigwage'){filtered_df <- final_gw_report}
    
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
