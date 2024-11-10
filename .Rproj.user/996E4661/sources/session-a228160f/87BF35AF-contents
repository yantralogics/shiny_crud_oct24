# Read Module UI
readModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(ns("table"))
  )
}

# Read Module Server
readModuleServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderDT({
      datatable(data(), selection = 'single')
    })
    
    # Return selected row index
    reactive({
      input$table_rows_selected
    })
  })
}