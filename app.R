library(shiny)
library(DBI)
library(duckdb)
library(pool)
library(dplyr)
library(dbplyr)
library(DT)

# Source the modules
source("modules/readTable.R")
source("modules/updateEntry.R")
source("modules/deleteEntry.R")

ui <- fluidPage(
  titlePanel("Modularized CRUD App"),
  
  # Input form for creating a new entry
  sidebarLayout(
    sidebarPanel(
      textInput("id", "ID"),
      textInput("name", "Name"),
      numericInput("age", "Age", value = 30),
      actionButton("create", "Create"),
      actionButton("read", "Read"),
      updateModuleUI("update_module"),  # Call Update Module UI
      deleteModuleUI("delete_module")   # Call Delete Module UI
    ),
    
    # Main panel for displaying the table using Read Module UI
    mainPanel(
      readModuleUI("read_module")  # Call Read Module UI
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data frame to store the records
  data <- reactiveVal(data.frame(ID = character(), Name = character(), Age = numeric(), stringsAsFactors = FALSE))
  
  # Create record
  observeEvent(input$create, {
    new_entry <- data.frame(ID = input$id, Name = input$name, Age = input$age, stringsAsFactors = FALSE)
    data(rbind(data(), new_entry))
  })
  
  # Read Module
  selected_row <- readModuleServer("read_module", data)
  
  # Update Module
  updateModuleServer("update_module", data, selected_row)
  
  # Delete Module
  deleteModuleServer("delete_module", data, selected_row)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
