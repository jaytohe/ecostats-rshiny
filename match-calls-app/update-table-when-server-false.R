library(shiny)
library(DT)
data(iris)

iris$new_col <- ''

server <- function(input, output, session) {

  DF = reactiveValues(iris = iris)

  output$table <- DT::renderDataTable(expr = {
    if (is.null(isolate(input$table_state))) {
      DT::datatable(
        DF$iris,
        selection = 'single',
        callback = JS("$.fn.dataTable.ext.errMode = 'none';"),
        options = list(stateSave = TRUE)
      )
    } else {
      # print(isolate(input$table_state$order))
      DT::datatable(
        DF$iris,
        selection = 'single',
        callback = JS("$.fn.dataTable.ext.errMode = 'none';"),
        options = list(
          stateSave = TRUE,
          order = isolate(input$table_state$order),
          paging = TRUE,
          pageLength = isolate(input$table_state$length)
        )
      )
    }
  }, server = FALSE)

  proxy <- dataTableProxy('table')

  observeEvent(input$button, {
    DF$iris[input$table_rows_selected, c('new_col')] <- 'changed!'
  })

  observeEvent(DF$iris, {
    updateSearch(proxy, keywords = list(global = input$table_state$search$search, columns = NULL)) # see input$table_state$columns if needed
    selectPage(proxy, page = input$table_state$start/input$table_state$length+1)
  }, ignoreInit = TRUE, priority = -1)
}

ui <- fluidPage(
  actionButton('button', 'Press Me'),
  DT::DTOutput('table')
)

shinyApp(ui, server)
