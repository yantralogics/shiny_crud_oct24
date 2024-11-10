# Delete Module UI
deleteModuleUI <- function(id) {
  ns <- NS(id)
  actionButton(ns("delete"), "Delete Selected Row")  # Button for deletion
}

# Delete Module Server
deleteModuleServer <- function(id, data, selected_row) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$delete, {
      if (length(selected_row()) == 0) {
        showModal(modalDialog(
          title = "No row selected",
          "Please select a row to delete.",
          easyClose = TRUE
        ))
      } else {
        # Remove the selected row from the data
        new_data <- data()
        new_data <- new_data[-selected_row(), ]
        data(new_data)  # Update the data reactive value
      }
    })
  })
}
