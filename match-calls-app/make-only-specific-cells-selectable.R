library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  verbatimTextOutput("debugRows"),
  DTOutput("mytable")
)


# https://datatables.net/reference/event/user-select
# Select extension in : https://rstudio.github.io/DT/extensions.html

# Define server logic
server <- function(input, output) {
  df <- iris

  # Customizing the first column to be non-selectable
  df$Sepal.Length <- as.character(df$Sepal.Length)
  df$Sepal.Length <- paste0('<span class="non-selectable">', df$Sepal.Length, '</span>')


  output$debugRows <- renderText({
    rows <- input$mytable_rows_selected
    paste("Rows selected:", if (is.null(rows)) "NULL" else rows)
  })
  output$mytable <- renderDT({
    datatable(
      df,
      escape = FALSE,  # Allow HTML in the table
      extensions = c('Select', 'Buttons'),
      options = list(
        paging = FALSE,
        scrollY = 400,
        select = list(style = 'single', items = 'row')
      ),
      selection = "none",
      callback = JS(
        "table.on('user-select', function(e, dt, type, cell, originalEvent) {",
        "console.log('triggered')",
        "if (cell.index().column === 1) {",
        "    e.preventDefault();",
        "  }",
        "});"
      )
    ) %>% formatStyle('Sepal.Length',  backgroundColor = 'cyan', fontWeight = 'bold')
  }, server = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)
