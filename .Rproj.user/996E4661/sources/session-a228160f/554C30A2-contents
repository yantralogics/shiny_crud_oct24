# Update Module UI
updateModuleUI <- function(id) {
  ns <- NS(id)
  actionButton(ns("edit"), "Edit Selected Row")  # Button to trigger modal for editing
}

# Update Module Server
updateModuleServer <- function(id, data, selected_row) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger modal dialog for editing selected row
    observeEvent(input$edit, {
      if (length(selected_row()) == 0) {
        showModal(modalDialog(
          title = "No row selected",
          "Please select a row to edit.",
          easyClose = TRUE
        ))
      } else {
        selected_data <- data()[selected_row(), ]
        
        showModal(modalDialog(
          title = "Edit Record",
          textInput(ns("modal_id"), "ID", value = selected_data$ID),
          textInput(ns("modal_name"), "Name", value = selected_data$Name),
          numericInput(ns("modal_age"), "Age", value = selected_data$Age),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save"), "Save Changes")
          )
        ))
      }
    })
    
    # Save changes from modal
    observeEvent(input$save, {
      new_data <- data()
      new_data[selected_row(), "ID"] <- input$modal_id
      new_data[selected_row(), "Name"] <- input$modal_name
      new_data[selected_row(), "Age"] <- input$modal_age
      
      data(new_data)  # Update the data reactive value
      
      removeModal()  # Close modal after saving changes
    })
  })
}
