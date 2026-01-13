# library(shiny)
# plot module ----
TabRes_ui <- function(id) {
  ns <- NS(id)

  tagList(
    card(
      height = 170,
      card_header(h6("Analysis options"), class = "bg-success"),
      card_body(
        style = "background-color: #F6FCF4;",
        fluidRow(
          column(7, numericInput(ns("ini"),
            label = h6("percent change"), step = 5, value = 50
          )),
          column(5, numericInput(ns("thresh"),
            label = h6("threshold"), step = 0.01, value = 0.05
          ))
        ),
      )
    ),
    tableOutput(ns("tabres"))
  )
}

TabRes_server <- function(id, procfile) {
  moduleServer(id, function(input, output, session) {
    # Calculations for the min (baseline) and max
    minandmax<-function(m, aplate, ini, thresh){
      
      Time <- aplate[[1]]
      #minAbs calculation is selected here so these functions aren't separated into exterior files
      minAbs <- switch(input$abini,
                       "global zero"=input$back,
                       "nth absorbance"=m[input$arow],
                       "min+offset"=min(m, na.rm=TRUE)#+input$off
      )
      #This min abs is presented without the offset because it's not used for clot or lys calculations                
      #It is used for plotting                     
      
      firstA=m[1]
      pointmax<-which.max(m)
      
      maxA <- max(m, na.rm = TRUE)
      
      maxT <- Time[which.max(m)]#,
      changeA=maxA-minAbs
      
      minmax<-c(firstA, minAbs, maxA, changeA, maxT, pointmax)
      
    }
    
    #uppity analysis the clotting part
    uppity<-function(u, aplate, ini, thresh){
      
      Time <- aplate[[1]]
      
      minAbs <- switch(input$abini,
                       "global zero"=input$back,
                       "nth absorbance"=u[input$arow],
                       "min+offset"=min(u, na.rm=TRUE)+input$off
                       
      )
      
      maxAbs <- max(u, na.rm = TRUE)
      
      pointmax<-which.max(u)
      upTime<-Time[c(1:pointmax)] #vector of time to max
      upAbs<-u[c(1:pointmax)]  #vector of absorbances to max
      pcChange<-ini*(maxAbs-minAbs)+minAbs#this minAbs is determined in shiny, may be set or calculated
      startPoint<-which(abs(upAbs-pcChange)==min(abs(upAbs-pcChange)))[1]
      
      #StartAbs is fitted if abs > threshold, otherwise is closest point
      #This prevents crashing if there are blank wells
      ifelse((max(u)-min(u)<thresh | min(upAbs)>=pcChange),
             startAbs<-upAbs[startPoint],
             #startAbs <- NA,
             startAbs<-round(approx(upTime, upAbs, xout = pcChange, ties = mean)$x,3)
      )
      
      #StartTime is fitted if abs > threshold, otherwise is closest point
      #This prevents crashing if there are blank wells
      ifelse((max(u)-min(u)<thresh | min(upAbs)>=pcChange),
             startTime<-upTime[startPoint],
             #startTime <- NA,
             startTime<-round(approx(upAbs, upTime, xout = startAbs, ties = mean)$y,3)
      )
      
      ifelse(is.na(startTime), startTime <- 0, startTime <- startTime)
      ifelse(is.na(startTime), startPoint <- 1, startPoint <- startPoint)
      
      upcurve<-c(minAbs, startTime,startAbs, maxAbs, startPoint, pointmax)
      
    } 
    
    #and here we analyse the downside of the curve
    downy <- function(d, aplate, ini, thresh) {
      Time <- aplate[[1]]
      
      minAbs <- switch(input$abini,
                       "global zero"=input$back,
                       "nth absorbance"=d[input$arow],
                       "min+offset"=min(d, na.rm=TRUE)+input$off
                       
      )
      
      #Need to define how to get the max abs
      maxAbs <- max(d, na.rm = TRUE)
      
      pointmax <- which.max(d)
      pcChange <- ini * (maxAbs - minAbs) + minAbs
      downTime <- Time[-c(1:pointmax)]
      downAbs <- d[-c(1:pointmax)]
      #This deals with wiggly late points
      TC <- which.min(downAbs)
      #downTime <- downTime[1:TC]
      #downAbs <- downAbs[1:TC]
      ifelse(TC<=1, downTime <- downTime, downTime <- downTime[1:TC])
      ifelse(TC<=1, downAbs <- downAbs, downAbs <- downAbs[1:TC])
      
      #This allows for late blips followed by flat response
      maxChange <- max(downAbs, na.rm = TRUE)-min(downAbs, na.rm = TRUE)
      
      #For complete lysis
      ifelse(d[length(d)]>=pcChange, endPoint <- length(d),endPoint <- which(downAbs<=minAbs)[1] )
      ifelse(d[length(d)]>=pcChange, endTime <- Time[length(d)], endTime <- downTime[endPoint])
      ifelse(d[length(d)]>=pcChange, lastPoint <- endPoint,lastPoint <- endPoint+pointmax )
      #will crash if lastPoint is NA
      ifelse(is.na(lastPoint), lastPoint <- length(d), lastPoint <- endPoint+pointmax)
      
      #The point where %lysis occurs
      decayPoint<-which(abs(downAbs-pcChange)==min(abs(downAbs-pcChange)))[1] 
      
      AUC<-sum(diff(Time[1:lastPoint])*(head(d[1:lastPoint],-1)+tail(d[1:lastPoint],-1)))/2
      
      #Allows for a flat response or a late flat response
      ifelse((maxAbs<thresh | maxChange<thresh), decayAbs <- downAbs[decayPoint], 
             decayAbs <- round(approx(downTime, downAbs, xout = pcChange, ties = mean)$x, 3)
      )
      #Allows for a flat response or a late flat response
      ifelse((maxAbs<thresh | maxChange<thresh), decayTime <- downTime[decayPoint], 
             decayTime <- round(approx(downAbs, downTime, xout = decayAbs, ties = mean)$y, 3)
      )
      #some safety net for insufficient lysis
      ifelse(downAbs[length(downAbs)] >=pcChange, lysPoint <- lastPoint, lysPoint <- decayPoint+pointmax)
      ifelse(downAbs[length(downAbs)] >=pcChange, decayTime <- NA, decayTime <- decayTime)
      
      downcurve <- c(decayAbs, decayTime, lysPoint, decayPoint, pointmax, lastPoint, endTime, AUC)
      
    }

    # Generate the table of results
    TabRes<- reactive ({
      #for reference
      #minmax<-c(firstA, minAbs, maxA, changeA, maxT, pointmax)
      #upcurve<-c(minAbs, startTime,startAbs, maxAbs, startPoint, pointmax)
      #downcurve <- c(decayAbs, decayTime, lysPoint, decayPoint, pointmax, lastPoint, endTime, AUC)
      
      whichPlate <- procfile()
      args_list <- list(whichPlate, ini = input$ini*.01, thresh = input$thresh)
      #ini <- input$ini*.01
      #thresh <- input$thresh
      
      Time <- whichPlate[[1]]
      #Time <- readData()[[1]]
      TabRes <- whichPlate[ -1] |>
        imap(~ {
          resultsu <- do.call(uppity, c(list(.x), args_list))
          resultsd <- do.call(downy, c(list(.x), args_list))
          resultsm <- do.call(minandmax, c(list(.x), args_list))
          data.frame(
            Wells = .y,
            minAbs         = resultsm[2],
            clotTime       = resultsu[2],
            clotAbs        = resultsu[3],
            maxAbs         = resultsu[4],
            deltaAbs       = resultsm[4],
            maxTime        = resultsm[5],
            lysAbs         = resultsd[1],
            lysTime        = resultsd[2],
            clotTolysTime  = resultsd[2]-resultsu[2],
            endTime        = resultsd[7],
            AUC            = resultsd[8],
            startPoint     = resultsu[5],
            maxPoint       = resultsm[6],
            lysPoint       = resultsd[3],
            lastPoint      = resultsd[6], 
            decayPoint     = resultsd[4]
            
            
          )
        }) |>
        list_rbind() |> 
        mutate(across(where(is.numeric), \(x) round(x, digits=4))) 
      #No clipboard for online app
      #clipr::write_clip(TabRes)
      TabRes
    })
  })
}
