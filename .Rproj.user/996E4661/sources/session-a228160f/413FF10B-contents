library(shiny)
library(bslib)
library(dplyr)
library(here)
library(data.table)

pit_cap_data <- fread(here('pit_capacity.csv'))

not_all_na <- function(x) any(!is.na(x))
###. remove columns if there is all na 
pit_cap_data <- pit_cap_data %>% select_if(not_all_na) %>% mutate(Plant_Name = as.character(Plant_Name))

ui <- page_fluid(
  layout_columns(
    actionButton('add_row','Add Record'),
    actionButton('edit_row','Edit Record'),
    actionButton('delete_row','Delete Record')
  ),
  DT::dataTableOutput('pit_data')
)

server<- function(input,output,session){
  ## 
  data_in <- reactiveVal(pit_cap_data)
  #rv$
  rv <- reactiveValues()
  output$pit_data <- DT::renderDataTable({
    data_in() %>% DT::datatable(selection='single')
  })
 ## Now lets put stuff down as each CRUD action happnes
  
  ## what happens when add row happens 
  observeEvent(input$add_row,{
    ## when this happens, a modal should be pulled up that allows for adding stuff 
    showModal(modalDialog(
      ## this has to have everything 
      layout_column_wrap(
        textInput('pit_name','Pit Name'),
        selectInput('plant_name','Plant Name',choices = unique(pit_cap_data$Plant_Name)),
        textInput('daily_cap','Daily Pit Capacity LBS'),
        textInput('min_len','Minimum Ingot Length in'),
        textInput('max_len','Minimum Ingot Length in'),
      ),
      footer =tagList(
        modalButton('Cancel'),
        actionButton('add_data',"Add")
      ),
      easyClose = T,
      fade = T,
      size = 'l'
    ))
  })
  
  ## This is what happens when a piece of data gets added 
  observeEvent(input$add_data,{
    showNotification('Someone pressed Add Data button bro!')
    ## Put in logic for creating the entry in new record 
    # browser()
    new_data = data_in() %>% slice(1)
    new_data$Pit_Id = max(pit_cap_data$Pit_Id)+1
    new_data$Pit_Name = input[['pit_name']]
    new_data$Plant_Name = input[['plant_name']]
    new_data$Daily_Capacity_Lbs = input[['daily_cap']] %>% as.integer()
    new_data$Min_Length = input[['min_len']] %>% as.integer()
    new_data$Max_Length = input[['max_len']] %>% as.integer()
    # browser()
    #new_data <- new_data %>% mutate(Plant_Name = factor(Plant_Name, labels=unique(pit_cap_data$Plant_Name)))
    
    data_in(data_in() %>% bind_rows(new_data))
    removeModal()
  })
  
  ## This is what happens when a piece of data gets edited 
  observeEvent(input$edit_row,{
    ## collect the row selected 
    selected_ind = input[['pit_data_rows_selected']]
    select_df <- data_in() %>% slice(selected_ind)
    # browser()
    rv$selected_ind <- selected_ind
    rv$selected_df <- select_df
    
    
    ## Now use the similar modal 
    showModal(modalDialog(
      ## this has to have everything 
      layout_column_wrap(
        textInput('pit_name_edit','Pit Name',value = select_df$Pit_Name),
        selectInput('plant_name_edit','Plant Name',choices = unique(pit_cap_data$Plant_Name),selected = select_df$Plant_Name),
        textInput('daily_cap_edit','Daily Pit Capacity LBS',value = select_df$Daily_Capacity_Lbs),
        textInput('min_len_edit','Minimum Ingot Length in',value = select_df$Min_Length),
        textInput('max_len_edit','Minimum Ingot Length in',value = select_df$Max_Length),
      ),
      footer =tagList(
        modalButton('Cancel'),
        actionButton('edit_data',"Edit")
      ),
      easyClose = T,
      fade = T,
      size = 'l'
    ))
  })
  
  ## Now the logic for what happens when edit_data is pressed
  observeEvent(input$edit_data,{
    ## Lets collect everything from the edit values 
    new_data = data_in() %>% slice(1)
    new_data$Pit_Id = rv$selected_df$Pit_Id
    new_data$Pit_Name = input[['pit_name_edit']]
    new_data$Plant_Name = input[['plant_name_edit']]
    new_data$Daily_Capacity_Lbs = input[['daily_cap_edit']] %>% as.integer()
    new_data$Min_Length = input[['min_len_edit']] %>% as.integer()
    new_data$Max_Length = input[['max_len_edit']] %>% as.integer()
    
    ## Now we need to replace the selected row with this one 
    # browser()
    temp_data = data_in()[-rv$selected_ind, ]
    ## add new_data to temp_data
    data_in(temp_data %>% bind_rows(new_data) %>% arrange(Pit_Id))
    showNotification('Someone changed the data bro!')
    removeModal()
  })
  
  ### Sme the logic for deleting the data 
  observeEvent(input$delete_row,{
    ## want to show the modal asking if they want to proceed with deletion
    selected_ind = input[['pit_data_rows_selected']]
    select_df <- data_in() %>% slice(selected_ind)
    # browser()
    rv$selected_ind_del <- selected_ind
    rv$selected_df_del <- select_df
    showModal(modalDialog(
      ## this has to have everything 
      tagList(
          h3('Are you sure you want to delete this record?'),
          DT::dataTableOutput('data_for_deletion')
         
      ),
      footer =tagList(
        modalButton('Cancel'),
        actionButton('delete_data',"Delete")
      ),
      easyClose = T,
      fade = T,
      size = 'l'
    ))
  })
  
  output$data_for_deletion <- DT::renderDataTable({
    rv$selected_df_del %>% DT::datatable(options = list(dom='t'))
  })
  
  ## THis is what happens when someone presses the delete button
  observeEvent(input$delete_data,{
    # browser()
    ## Get the data for deletion
    temp_data = data_in()[-rv$selected_ind_del, ]
    ## add new_data to temp_data
    data_in(temp_data %>% arrange(Pit_Id))
    ## Persist operation with a SQL query 
    showNotification('Someone deleted a record bro!')
    removeModal()
  })
  
  ## WE need a way to persist data now 
  ## On exit, we will persist all operations ?
  
  
}

shinyApp(ui,server)