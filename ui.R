
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
library(googlesheets)

shinyUI(fluidPage(

  # Application title
  titlePanel("評估到貨時間"),

  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      tags$style(type="text/css", "
             #loadmessage {
                 position: fixed;
                 top: 0px;
                 left: 0px;
                 width: 100%;
                 padding: 5px 0px 5px 0px;
                 text-align: center;
                 font-weight: bold;
                 font-size: 100%;
                 color: #FFFFFF;
                 background-color: #289dd7;
                 z-index: 105;
                 }
                 "),
      conditionalPanel(condition = "input.NextStep == 0",
                       helpText("此系統為利用每日資料快照作為到貨天數的依據,
                                 若沒有每日更新報表則會出現結果有誤差,此系統僅
                                 能用於參考輔助使用,結果可能會有些許誤差,報表最
                                 好於每日中午過後下載")),
      actionButton("NextStep", "Next", width = "100%",
                   icon = icon("mouse-pointer")),
      conditionalPanel(condition = "input.NextStep != 0",
                       fileInput("mainfile", "銷售資料", multiple = TRUE),
                       textInput("GoogleTitle", "Google試算表名稱", 
                                 width = "100%"),
                       actionButton("Go", "執行", icon = icon("play"),
                                    width = "100%"),
                       helpText("注意!!該功能會使用妳googledrive內部的資料名稱,
                                請在使用前確認填入名稱並無重複,以免造成資料被
                                覆蓋的情形"),
                       actionButton("Check", "檢查GoogleDrive", width = "100%")),
      uiOutput("Browse"),
      conditionalPanel(condition = "input.NextStep != 0",
                       hr(),
                       actionButton("EncodingOptions", "Additional Options",
                                    width = "100%")),
      radioButtons("Encoding","編碼", c("big5", "UTF-8")),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("Processing...", id="loadmessage"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("final.result"),
      dataTableOutput("CheckList")
    )
  )
))
