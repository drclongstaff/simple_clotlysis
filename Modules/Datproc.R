#Initial data processing

#Some functions for data processing


#This function subtracts a background from all data defined by row in all data
BaselineNP<-function(n, NP){
  n <- n-n[NP]
}

#Subtract the min abs of each column and include an offset
BaselineOff <- function(n, OF) {
  n <- n-min(n)-OF
}

library(shiny)
# plot module ----
DatProc_ui <- function(id) {
  
      ns <- NS(id)
  
      tagList(
    
        card(
              height = 248,
              #full_screen = TRUE,
            card_header(h6("Baseline options"), class = "bg-success"),
            card_body(
              style = "background-color: #F6FCF4;",
            fluidRow(
               radioButtons(ns("abini"), label = NULL , inline = TRUE,
                                     choices = c("global zero", "nth absorbance", 
                                                 "min + offset")),
                ),
        
        fluidRow(
            column(2, numericInput(ns("back"),
                                   label = h6("global zero"), step = 0.005, value = 0.042)),
      
            
            column(2, numericInput(ns("arow"),
                                   label = h6("nth point"), value = 54)),
            
            column(3, numericInput(ns("off"), label = h6("offset"), value = 0.005, min = 0, step = 0.005) )
        
                )
              )
          ),
    
    tableOutput(ns("procdat")) 
    
  )
}
#Here the data are processed according to baseline selection
#map_df from the Purrr package is used to generate the baseline-corrected data
DatProc_server <- function(id, df) {
  
    moduleServer(id, function(input, output, session) {
    
          #data is the raw data
          data <- reactive({ as_tibble(df())
            #as_tibble(cbind("Time"=df()[[1]], df()[,-1]-input$back))
                          })
          #data2 is for subtracting a fixed specified background
          data2 <- reactive({
            as_tibble(cbind("Time"=df()[[1]], df()[,-1]-input$back))
                          })
          
          #data3 is data with absorbance subtracted from a point number (row)
          data3 <- reactive({
            data3 <- map_df(df()[,-1], ~BaselineNP(.x, input$arow)) %>% #BaselineNP is a function defined above
              add_column("Time"=df()[[1]], .before = TRUE)# 
                          })
          
          #data4 is data with min value and optimum offset subtracted
          data4 <- reactive({
            map_df(df()[,-1], ~BaselineOff(.x, input$off)) %>% 
              add_column("Time"=df()[[1]], .before = TRUE)# 
                          })
  #procdat is the processed data after baseline selection
  procdat<- reactive({
      
          switch(input$abini,
                 #"global zero"=data2(),
                 "global zero"=data(),
                 "nth absorbance"=data3(),
                 "min + offset"=data4()
                )
            })
      })
}




