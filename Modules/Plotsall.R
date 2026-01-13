multi_plotFun <- function(PLATE, ROWNUM, TABRES) {
  # Set up some plotting parameters
  TabRes <- TABRES
  RowNum <- ROWNUM

  plateData <- PLATE[, -1]
  absWells <- length(plateData[1, ])
  maxy <- max(plateData, na.rm = TRUE)*1.1 # To set the maximum of the whole plate
  par(mfrow = c(RowNum, (absWells / RowNum))) # Organisation of multiple plots
  par(mar = c(0.2, 0.2, 0.2, 0.2)) # Dimensions for figure

  # Generate the plots from plotmake_fun
  lapply(seq_along(plateData), function(k) {
    # for(k in seq_along(plateData)){ #This is an alternative loop

    plotmake_fun(PLATE, TabRes, k, axx="n", axy="n", maxy)
  }) # remove this ')' with alternative loop
}


library(shiny)
# plot module ----

plot_ui <- function(id) {
  ns <- NS(id)
  card(
    height = 450,
    full_screen = TRUE, # is expandable
    card_header(numericInput(ns("numrows"), label = "Plot number of rows", value = 3, min = 1, step = 1),
      class = "bg-info"
    ),
    card_body(
      style = "background-color: #FAFBFB  ;",
      tagList(
        plotOutput(ns("plot"))
      )
    )
  )
}

plot_server <- function(id, procfile, resfile) { # use df or procfile here or resfile or myRes?

  moduleServer(id, function(input, output, session) {
    plot <- reactive({
      multi_plotFun(procfile(), input$numrows, resfile())
    })
    output$plot <- renderPlot({
      plot()
    })
  })
}

