library(DT)
library(shiny)
library(shinyjs)

ui <- fluidPage(
  shinyjs::useShinyjs(),  # Enable shinyjs
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.4/clipboard.min.js")
  ),
  titlePanel("UYT Wages Summary"),
  sidebarLayout(
    sidebarPanel(
      selectInput("week", "Select Week:", 
                  choices = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5", 
                              "Week 6", "Week 7", "Week 8", "Week 9", "Week 10", "Other"),
                  selected = "Week 1"),
      selectInput("type", "Select Report:",
                  choices = c("Wages", "Advanced Wages", "Gigwage"),
                  selected = "Wages"),
      actionButton("copyButton", "Copy to Clipboard")
    ),
    mainPanel(
      DTOutput("tableOutput")
    )
  )
)

server <- function(input, output) {
  output$tableOutput <- renderDT({
    # Perform filtering and create a sample table
    df <- expand.grid(
      Name = c("John", "Alice", "Bob"),
      Week = c("Week 1", "Week 2", "Week 3"),
      Type = c("Wages", "Advanced Wages", "Gigwage"),
      stringsAsFactors = FALSE
    )
    df$Value <- sample(100:200, nrow(df), replace = TRUE)
    
    filtered_df <- df[df$Week == input$week & df$Type == input$type, ]
    filtered_df
  })
  
  observeEvent(input$copyButton, {
    js <- "
      var table = $('#tableOutput table').get(0);
      var clipboard = new ClipboardJS('#copyButton', {
        text: function() {
          var range = document.createRange();
          range.selectNodeContents(table);
          var selection = window.getSelection();
          selection.removeAllRanges();
          selection.addRange(range);
          var text = selection.toString();
          selection.removeAllRanges();
          return text;
        }
      });
      clipboard.on('success', function(e) {
        e.clearSelection();
        Shiny.setInputValue('copiedToClipboard', true, {priority: 'event'});
      });
    "
    shinyjs::runjs(js)
  })
  
  observeEvent(input$copiedToClipboard, {
    if (input$copiedToClipboard) {
      shinyalert::shinyalert(
        title = "Success!",
        text = "Table copied to clipboard.",
        type = "success"
      )
    }
  })
}

shinyApp(ui = ui, server = server)
