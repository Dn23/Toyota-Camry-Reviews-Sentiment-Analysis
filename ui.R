library(shiny)
library(shinythemes)
library(dplyr)
library(data.table)
library(rvest)
library(stringi)
library(purrr)
shinyUI(
  
  
  
  fluidPage(theme = shinytheme('sandstone'),
            
            # Application title
            titlePanel("Dwarkesh Natarajan - Sentiment Analysis"),
            
            # Sidebar 
            sidebarLayout(
              sidebarPanel(
                textInput("main_url","Enter URL", value = "https://www.cars.com/research/toyota-camry/")
              ),
              mainPanel(tabsetPanel(tabPanel("First Page",uiOutput("First_Page")),
                                    tabPanel("Data Snippet of Train",dataTableOutput("table")),
                                    tabPanel("Data Snippet of Test",dataTableOutput("table1")),
                                    tabPanel("Normalized and Tagged",dataTableOutput("table2")),
                                    tabPanel("Sentiment Score",fluidRow(column(4,tableOutput("average2")),
                                                                       column(4,tableOutput("average3")),
                                                                       column(12,dataTableOutput("table3")))),
                                    tabPanel("Predicted Table",fluidRow(column(12,tableOutput("Accuracy")),
                                                                        column(12,dataTableOutput("table4")))),
                                    tabPanel("TF_IDF",dataTableOutput("table5")),
                                    tabPanel("Visualization",fluidRow(column(5,plotOutput("plot1")),
                                                                      column(5,plotOutput("plot2")),
                                                                      column(5,plotOutput("plot3")),
                                                                      column(5,plotOutput("plot4"))))
                                    
              )
              
              )
            )
  ))
