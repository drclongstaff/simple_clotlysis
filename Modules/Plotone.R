
#This function plots data from the selected well 'WELLNUM'
#The table of results, TabRes, is used to draw the lines and pick out curve features
one_plotFun <- function(PLATE, WELLNUM, TABRES) {
  #define some parameters for the plot      
        TabRes <- TABRES
        k<-WELLNUM #the selected well
        RowNum <- 1 #here there is only one plot, so only one row is needed
        Time<-PLATE[[1]] #time in the first column of the plate data
        plateData<-PLATE[,-1] #The platedata without the time column
        absWells <- 1 #This is for single plot
        mint<-min(Time, na.rm = TRUE)
        maxt<-max(Time, na.rm = TRUE)
        maxy<-max(plateData, na.rm = TRUE) #These min and max values are for scaling the plots
        samples <- colnames(plateData) #for writing the well name
        par(mfrow=c(RowNum,(absWells/RowNum)))
        par(mar=c(4,4,1,1)) # dimensions for figure
  
        yi<- plateData[[k]]
        plot<-plot(Time, yi, type = "l", col= "grey40", lwd = 2, xlim= c(0, maxt), 
                   ylim=c(0, maxy*1.2), ylab="Absorbance")
        
        points(Time, yi, pch=21, col = "slategrey")
        #different coloured lines for start to clotting to max to %lysis and to full lysis
        lines(Time[1:TabRes[[k,12]]], yi[1:TabRes[[k,12]]],col="red", lwd=3)
        lines(Time[TabRes[[k,12]]:TabRes[[k,13]]], yi[TabRes[[k,12]]:TabRes[[k,13]]],col="green", lwd=3)
        lines(Time[TabRes[[k,13]]:TabRes[[k,14]]], yi[TabRes[[k,13]]:TabRes[[k,14]]],col="blue", lwd=3)
        lines(Time[TabRes[[k,14]]:TabRes[[k,15]]], yi[TabRes[[k,14]]:TabRes[[k,15]]],col="darkorange2", lwd=3)
        
        #legend including the well number and name
        legend(TabRes[k,3], maxy*1.2, xjust=TabRes[k,3]/maxt, bty="n", paste0(samples[k],"=",k), cex=2)
    
        #dashed lines showing the baseline and times
        abline("v"= TabRes[k,3], lty=2)
        abline("v"= TabRes[k,7], lty=2)
        abline("v"= TabRes[k,8], lty=2)
        abline("h"= TabRes[k,2], lty = 2)
        abline("v"= TabRes[k,11], lty = 2)

    }


library(shiny)
# plot module ----
oneplot_ui <- function(id) {
 
  ns <- NS(id)
  
    card(
      height = 450,
      full_screen = TRUE,
      card_header(numericInput(ns("wellnum"),label = "Plot well number", value = 1, min = 1, step = 1),
                  class = "bg-info"),
      card_body(
        style = "background-color: #FAFBFB ;",
    tagList(
    plotOutput(ns("oneplot"))
          )
        )
      )
  }

  oneplot_server <- function(id, procfile, resfile) {#use df or procfile here or resfile or myRes?
   
      moduleServer(id, function(input, output, session) {
      #thenames <- reactive({colnames(procfile()[,-1])}) #may try to use names of wells and selectize
      #aname <- reactive({thenames()[input$wellnum]})
      plot <- reactive({one_plotFun(procfile(), input$wellnum, resfile())})
      output$oneplot <- renderPlot({plot()})
    
      })
  }

