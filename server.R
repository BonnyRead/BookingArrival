
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
library(plyr)
library(dplyr)
library(googlesheets)

shinyServer(function(input, output) {
  
  # Modify the file size limitation to 30 MB
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)
  
  # Set the Encoding option to hide by default
  hide("Encoding")
  
  # Hiding the button after clicking it
  observeEvent(input$NextStep, {
    hide("NextStep")
  })
  
  # Checking the file name in GoogleDrive does not duplicate
  observeEvent(input$Check, {
    output$CheckList <- renderDataTable({
      data.table(ID = gs_ls()$sheet_title %>% seq_along,
                 Name = gs_ls()$sheet_title) %>% return
    })
    hide("Check")
  })
  
  # Set the button to be toggle encoding option
  observeEvent(input$EncodingOptions, {
    toggle("Encoding")
  })
  
  # insert an action button with website link which is linked the record file
  # in GoogleDrive
  observeEvent(input$Go, {
    output$Browse <- renderUI({
      if (is.null(input$GoogleTitle)) {
        return (NULL)
      } else {
        googleurl <- gs_title(input$GoogleTitle)$browser_url
        googleurl <- paste0("window.open('", googleurl, "', '_blank')")
        actionButton("BrowseURL", "確認資料", width = "100%" ,
                     onclick = googleurl)
      }
    })
  })
  
  # Cleaning mainpanel while users click execute button
  observeEvent(input$Go, {
    hide("CheckList")
  })
  
  # Importing snapshot file into this system
  snapshot.file <- eventReactive(input$Go, {
    inFile1 <- input$mainfile
    if (is.null (inFile1)) {
      return (NULL)
    } else {
      lapply(inFile1$datapath, function(k) {
        read.csv(k, fileEncoding = input$Encoding, stringsAsFactors = FALSE) }) %>% 
        rbind.fill %>% return
    }
  })
  
  # Importing record file into this system
  record.file <- eventReactive(input$Go, {
    exist.confirm <- gs_ls()
    exist.confirm <- input$GoogleTitle %in% exist.confirm$sheet_title %>% any
    if (exist.confirm) {
      gs_title(input$GoogleTitle) %>% gs_read %>% return
    } else {
      gs_new(input$GoogleTitle) %>% gs_read %>% return 
    }
  })
  
  # Activiating the procedure to make comparison within those snapshots
  processed.file <- reactive({
    if (record.file() %>% nrow == 0) {
      pro.snap <- snapshot.file() %>% setDT
      pro.snap <- pro.snap[送貨狀態 == "已到達", .SD[1], 訂單號碼]
      pro.snap <- pro.snap[, c("訂單號碼", "訂單日期", "顧客", "電郵",
                               "電話號碼", "付款狀態", "合計"), with = FALSE]
      pro.snap <- pro.snap[, 到達天數 := 1]
      pro.snap <- pro.snap %>% setcolorder(c("訂單號碼", "到達天數", "訂單日期",
                                             "顧客","電郵", "電話號碼",
                                             "付款狀態", "合計"))
      pro.snap[, 電話號碼 := gsub("+886-", 0, 電話號碼, fixed = TRUE)]
      pro.snap[substr(電話號碼, 1, 1) != 0, 電話號碼 := paste0(0, 電話號碼)]
      gs_title(input$GoogleTitle) %>% gs_edit_cells(input = pro.snap)
      return (pro.snap)
    } else {
      
      # Set in data as data.table format
      pro.snap <- snapshot.file() %>% setDT %>% 
        .[送貨狀態 == "已到達", .SD[1], 訂單號碼]
      pro.record <- record.file() %>% setDT %>%
        .[, 電話號碼 := as.character(電話號碼)] %>%
        .[substr(電話號碼, 1, 1) != 0, 電話號碼 := paste0("0", 電話號碼)]
      
      # Removing the booking ID which has been took away by the customer or 
      # been returned by the convenience store shipment system
      pro.record <- pro.record[! 訂單號碼 %in% 
                    setdiff(pro.record$訂單號碼,pro.snap$訂單號碼)]
      
      # Adding one more value to those booking order which are still stay in
      # arrived status in their delivery status
      
      stayed.ID <- intersect(pro.record$訂單號碼, pro.snap$訂單號碼)
      pro.record[訂單號碼 %in% stayed.ID, 到達天數 := 到達天數 + 1]
      
      # Adding the new value which just arrives the convenience store
      if (setdiff(pro.snap$訂單號碼, pro.record$訂單號碼) %>% length > 0) {
        merge.candidate <- pro.snap[訂單號碼 %in%
                           setdiff(pro.snap$訂單號碼, pro.record$訂單號碼),
                           c("訂單號碼", "訂單日期","顧客","電郵","電話號碼",
                             "付款狀態", "合計"), with = FALSE]
        merge.candidate[, 訂單日期 := as.Date(訂單日期)]
        merge.candidate[, 到達天數 := 1]
        merge.candidate[, 電話號碼 := gsub("+886-", 0, 電話號碼, fixed = TRUE)]
        merge.candidate[substr(電話號碼, 1, 1) != 0, 
                        電話號碼 := paste0(0, 電話號碼)]
        pro.record <- rbind.fill(pro.record, merge.candidate) 
        gs_title(input$GoogleTitle) %>% gs_delete
        gs_new(title = input$GoogleTitle) %>% gs_edit_cells(input = pro.record)
        return(pro.record)
      } else {
        gs_title(input$GoogleTitle) %>% gs_delete
        gs_new(title = input$GoogleTitle) %>% gs_edit_cells(input = pro.record)
        return(pro.record)
      }
    }
  })
  
  # Displaying the result to users
  output$final.result <- renderDataTable({
    if (is.null (processed.file())) {
      return (NULL)
    } else {
      processed.file()
    }
  })
})
