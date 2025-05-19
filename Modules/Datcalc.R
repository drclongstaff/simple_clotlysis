#library(shiny)
# plot module ----
TabRes_ui <- function(id) {
  
    ns <- NS(id)
  
    tagList(
    
      card(height = 170,
          card_header(h6("Analysis options"), class = "bg-success"),
          card_body(
              style = "background-color: #F6FCF4;",
    fluidRow(
      column(7, numericInput(ns("ini"),
                             label = h6("percent change"), step = 5, value = 50)),
      
      column(5, numericInput(ns("thresh"),
                             label = h6("threshold"), step = 0.01, value = 0.05))
            ),
        )
      ),
   
    tableOutput(ns("tabres"))
  
      )
  
  }

TabRes_server <- function(id, procfile) {
  
      moduleServer(id, function(input, output, session) {
    
        #Calculations for the min (baseline) and max
        MaxandMin<-function(m, Time, thresh){
            
          #minA is for setting the baseline
            minA <- switch(input$abini,
                    "global zero"=input$back,
                    #"global zero"=min(m, na.rm=TRUE)-input$back, #some options here for how best to display results
                    #"global zero"=min(m, na.rm=TRUE),
                     "nth absorbance"=m[input$arow],
                    #"nth absorbance"=min(m, na.rm=TRUE),
                     #"min + offset"=min(m, na.rm=TRUE)+input$off
                    "min + offset"=min(m, na.rm=TRUE)
                          )
      
            firstA=m[1]
            pointmax<-which.max(m)
      
            maxA <- max(m, na.rm = TRUE)
      
            maxT <- Time[which.max(m)]#,
            changeA=maxA-minA
      
            #Here list the variables you want to collect
      Aminmax<-c(firstA, minA, maxA, changeA, maxT, pointmax)
      
    }
    
      #A function to isolate and do calculations 
        #on the ascending part of the curve    
      uppity<-function(u, Time, ini, thresh){
     
        minA <- switch(input$abini,
                     "global zero"=input$back,
                     "nth absorbance"=min(u, na.rm=TRUE),
                     "min + offset"=min(u, na.rm=TRUE)+input$off
                     #"min + offset"=min(u, na.rm=TRUE)
                        )
          
        #Need to define how to get the max abs
        maxAbs <- max(u, na.rm = TRUE)
        
        #Calculations to find the clotting time and abs
        pointmax<-which.max(u)
        upTime<-Time[c(1:pointmax)] #vector of time to max
        upAbs<-u[c(1:pointmax)]  #vector of absorbances to max
        pcChange<-ini*(maxAbs-minA)+minA#this minAbs is determined in shiny, may be set or calculated
        startPoint<-which(abs(upAbs-pcChange)==min(abs(upAbs-pcChange)))[1]
        #StartAbs is fitted if abs > threshold, otherwise is closest point
        #This prevents crashing if there are blank wells
        ifelse(max(u)-min(u)<thresh,
               startAbs<-upAbs[startPoint],
               startAbs<-round(approx(upTime, upAbs, xout = pcChange, ties = mean)$x,4)
              )
        
        #StartTime is fitted if abs > threshold, otherwise is closest point
        #This prevents crashing if there are blank wells
        ifelse(max(u)-min(u)<thresh,
               startTime<-upTime[startPoint],
               startTime<-round(approx(upAbs, upTime, xout = startAbs, ties = mean)$y,4)
              )
      
      #Collect the variables
      upcurve<-c(minA, startTime,startAbs, startAbs, startPoint, pointmax)
      
    } 
    
    #Calculations on the descending part of the curve  
    downy<-function(d, Time, ini, thresh ){
      
          minAbs <- switch(input$abini,
                         "global zero"=input$back,
                         "nth absorbance"=min(d, na.rm=TRUE),
                         "min + offset"=min(d, na.rm=TRUE)+input$off
                         #"min + offset"=min(d, na.rm=TRUE)
                          )
        
          #Need to define how to get the max abs
          maxAbs <- max(d, na.rm = TRUE)
          
          #Calculations for the 50% lysis time and abs and 100% lysis time  
          pointmax<-which.max(d)
          pcChange<-ini*(maxAbs-minAbs)+minAbs
          #ifelse statements are used to differentiate between clotting or clotlysis curves
          ifelse(d[length(d)]>=pcChange, downTime <- Time, downTime<-Time[-c(1:pointmax)])
          ifelse(d[length(d)]>=pcChange, downAbs <- d,  downAbs<-d[-c(1:pointmax)] )
          #decaypoint is where set% lysis occurs
          if_else(d[length(d)]>=pcChange, decayPoint <- length(d), decayPoint<-which(abs(downAbs-pcChange)==min(abs(downAbs-pcChange)))[1] )
          #end point is where 100% lysis occurs
          if_else(d[length(d)]>=pcChange, endPoint <- length(d),endPoint <- which(downAbs<=minAbs)[1] )
          
          if_else(d[length(d)]>=pcChange, endTime <- Time[length(d)], endTime <- downTime[endPoint])
          
          if_else(d[length(d)]>=pcChange, lastPoint <- endPoint,lastPoint <- endPoint+pointmax )
          #will crash if lastPoint is NA
          ifelse(is.na(lastPoint), lastPoint <- length(d), lastPoint <- endPoint+pointmax)
          
          #will this avoid crashes due to endtime = NA
          ifelse(is.na(endTime), endTime <- Time[length(d)], endTime <- endTime)
          
          #In the simple clotlysis app don't use AUC it can be crashy
          #AUC<-sum(diff(Time[1:lastPoint])*(head(d[1:lastPoint],-1)+tail(d[1:lastPoint],-1)))/2
          
          if_else(max(d)-min(d)<thresh,
                  decayAbs<-downAbs[decayPoint],
                  decayAbs<-round(approx(downTime, downAbs, xout = pcChange, ties = mean)$x,4)
                  )
      
          #StartTime is fitted if abs > threshold, otherwise is closest point
          #This prevents crashing if there are blank wells
          ifelse(max(d)-min(d)<thresh,
                 decayTime<-downTime[decayPoint],
                 decayTime<-round(approx(downAbs, downTime, xout = decayAbs, ties = mean)$y,4)
                ) 
        #Collect the down curve variables
        downcurve <- c(decayAbs, decayTime, decayPoint+pointmax,lastPoint, endTime)
      
    }
    
    #Generate the table of results
    tabres <- reactive({
      
          thePlate <- procfile() #use processed data
          Time<-thePlate[[1]] #Time is the first column of the datafile
          ini <- input$ini*.01 #ini is for %lysis
          thresh <- input$thresh #thresh sets delta abs for interpolation to avoid crashes in noisy or empty wells
      
          #map-df from the Purrr package is used in conjunction with the functions above
          #mapDatDF is a larger results table that is trimmed to make TabRes below
          mapDatDF<- thePlate[-1] %>%  #take away the time column
            map_df(~ data.frame(firstAbs=MaxandMin(.x, Time, thresh)[1], min.abs=MaxandMin(.x, Time, thresh)[2], 
                                max.abs=MaxandMin(.x, Time, thresh)[3], 
                                delta.abs=MaxandMin(.x, Time, thresh)[4], 
                                max.time=MaxandMin(.x, Time, thresh)[5], pointmax=MaxandMin(.x, Time, thresh)[6], 
                                clot.time=uppity(.x, Time, ini, thresh)[2], clot.abs=uppity(.x, Time, ini, thresh)[4], startPoint=uppity(.x, Time, ini, thresh)[5],
                                lys.abs=downy(.x, Time, ini, thresh)[1],
                                lys.time=downy(.x, Time, ini, thresh)[2],
                                decayPoint=downy(.x, Time, ini, thresh)[3],
                                endPoint=downy(.x, Time, ini, thresh)[4],
                                end.time=downy(.x, Time, ini, thresh)[5])) %>% 
            mutate(clotTolys.time=lys.time-clot.time) %>% 
            add_column(Wells=colnames(thePlate[-1]), .before = TRUE)     
      
            TabRes<-mapDatDF %>% #choose which results you need and in the right order
              #select(Wells, min.abs, clot.time, clot.abs, max.abs, delta.abs, max.time, lys.time, lys.abs, clotTolys.time, startPoint, pointmax, decayPoint, endPoint, end.time) %>%
              select(Wells, min.abs, clot.time, clot.abs, max.abs, delta.abs, max.time, lys.time, lys.abs, clotTolys.time, end.time, startPoint, pointmax, decayPoint, endPoint) %>%
              mutate(across(where(is.numeric), \(x) round(x, digits=4))) #use newer method for across
            #No clipboard for online app
            clipr::write_clip(TabRes)
      
                    })
            })
    }

