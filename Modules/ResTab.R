myRes <- read.csv("Data/myRes.csv")

filenames_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # dataTableOutput(ns("obs"))
    DT::DTOutput(ns("obs"))
  )
}

filenames_server <- function(id, theobs) {
  moduleServer(id, function(input, output, session) {
    myobs <- reactive({
      AA <- matrix(theobs(), byrow = TRUE, nrow = input$numrows)
      AA <- data.frame(AA)
      setNames(AA, rep(" ", length(AA)))
      # clipr::write_clip(AA)
    })
  })
}
