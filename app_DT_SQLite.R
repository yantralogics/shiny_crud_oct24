library(shiny)
library(bslib)
library(dplyr)
library(here)
library(data.table)
## database stuff 
library(DBI)
library(RSQLite)


## DB Path
sqlitePath <- here::here('dbsqlite_test')


saveData <- function(data,tableName) {
  # Connect to the database - this is append operation
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    tableName, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  DBI::dbSendQuery(db, query)
  dbDisconnect(db)
}

loadData <- function(tableName) {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", tableName)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}



deleteData <- function(data, tableName){
  # Connect to the database - this is append operation
  db <- dbConnect(SQLite(), sqlitePath)

  query_DF <- data %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column() 
  names(query_DF) <- c('rowname','V1')
  query_DF <- query_DF %>%   
  mutate(querystring = paste0(rowname,' = ',"'",V1,"'"))
  
  primary_key_statement = query_DF %>% filter(rowname == 'Pit_Id') %>% pull(querystring)
   query <- sprintf(
    "DELETE  FROM %s where %s",
    tableName, 
    
    primary_key_statement
  )
  # Submit the update query and disconnect
  dbExecute(db, query)
  dbDisconnect(db)
}

## we need something for edit data queries and delete data queries 
## SAMPLE
# UPDATE Customers
# SET ContactName = 'Alfred Schmidt', City= 'Frankfurt'
# WHERE CustomerID = 1;
editData <- function(data,tableName) {
  # Connect to the database - this is append operation
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  str1=''
  for(i in 1:ncol(data)){
    str1 = paste0(str1,names(data)[i],'=',"'",data[,i],"',")
  }
  
  query_DF <- data %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>% 
    mutate(querystring = paste0(rowname,' = ',"'",V1,"'"))
  
  primary_key_statement = query_DF %>% filter(rowname == 'Pit_Id') %>% pull(querystring)
  update_statement = query_DF %>% filter(rowname != 'Pit_Id') %>% pull(querystring) %>% paste(collapse = ',')
  query <- sprintf(
    "UPDATE  %s SET %s where %s",
    tableName, 
    update_statement,
   primary_key_statement
  )
  # Submit the update query and disconnect
  dbExecute(db, query)
  dbDisconnect(db)
}
## Upload file 
## for this we need to identify which primary keys are present
## delete from database first and then insert into followed by sort 
uploadData <- function(uploadData,tableName){
  ## Need to check what data has been changed 
  ## each table has a combination key that's not primary key 
  ## identify that per table for example - pitname+plantname
  
  ##verify if the file uploaded has the same fields as tableNames
  
  
  
  
}




ui <- page_fluid(
  layout_columns(
    actionButton('add_row','Add Record'),
    actionButton('edit_row','Edit Record'),
    actionButton('delete_row','Delete Record'),
    downloadButton('download_data'),
    fileInput('upload_data','Bulk edited data')
  ),
  DT::dataTableOutput('pit_data')
)

server<- function(input,output,session){
  ## 
  data_in <- reactiveVal(loadData('pit_capacity_data'))
  #rv$
  rv <- reactiveValues()
  output$pit_data <- DT::renderDataTable({
    data_in() %>% DT::datatable(selection='single',options = list(pageLength=20))
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
    new_data$Pit_Id = max(data_in()$Pit_Id)+1
    new_data$Pit_Name = input[['pit_name']]
    new_data$Plant_Name = input[['plant_name']]
    new_data$Daily_Capacity_Lbs = input[['daily_cap']] %>% as.integer()
    new_data$Min_Length = input[['min_len']] %>% as.integer()
    new_data$Max_Length = input[['max_len']] %>% as.integer()
    # browser()
    #new_data <- new_data %>% mutate(Plant_Name = factor(Plant_Name, labels=unique(pit_cap_data$Plant_Name)))
    ## Save the new data into the DB table 
    all_data <- data_in() %>% bind_rows(new_data)
    saveData(data = new_data,'pit_capacity_data')
    #data_in(data_in() %>% bind_rows(new_data))
    data_in(loadData('pit_capacity_data'))
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
    
    delete_df = data_in()[rv$selected_ind_del,]
    deleteData(delete_df,'pit_capacity_data')
    
    data_in(loadData('pit_capacity_data'))
    
    ## Persist operation with a SQL query 
    showNotification('Someone deleted a record bro!')
    removeModal()
  })
  
  
  output$download_data <- downloadHandler(
    filename <- function(){
      paste0('current_data.csv')
    },
    content = function(file){
      write.csv(data_in(),file)
    }
  )
  
  
  ## What happens when we upload?
  observeEvent(input$upload_data,{
    # browser()
    rv$uploadData <- (data.table::fread(input$upload_data$datapath))
    
    ## Show Modal of the data uploaded 
    showModal(modalDialog(
      ## this has to have everything 
      tagList(
        h3('Are you sure you want to add  this record?'),
        DT::dataTableOutput('upload_data_add')
        
      ),
      footer =tagList(
        modalButton('Cancel'),
        actionButton('append_data','Append Data'),
        actionButton('overwrite_data',"Overwrite")
      ),
      easyClose = T,
      fade = T,
      size = 'l'
    ))
    ## Modal should have the option to Overwrite/ Append  or discard data 
    
  })
  
  output$upload_data_add <- DT::renderDataTable({
    rv$uploadData %>% DT::datatable()
  })
  
  ## what happens when we append the data
  
  ## what happens when we overwrite the data 
  
  
}

shinyApp(ui,server)