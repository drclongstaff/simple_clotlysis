# Data loading module UI and Server

# Module UI function
csvFileUI <- function(id, label = "CSV file") {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(7, fileInput(ns("file"), label)),
      column(5, numericInput(ns("sheet"),
        label = h6("Excel sheet"), step = 1, value = 1
      ))
    )
  )
}

csvFileServer <- function(id, stringsAsFactors) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # The selected file, if any
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$file, message = FALSE))
        input$file
      })

      # The user's data, parsed into a data frame
      # This function detects data type from file ending, csv, txt, xlsx
      dataframe <- reactive({
        if (is.null(input$file)) {
          return(read.csv("Data/newCLD.csv"))
        } # load the provided data until user data provided
        else {
          ext <- tools::file_ext(userFile()$datapath)
          switch(ext,
            "xlsx" = readxl::read_excel(userFile()$datapath, sheet = input$sheet, .name_repair = "universal"),
            csv = vroom::vroom(userFile()$datapath, delim = ",", show_col_types = FALSE, .name_repair = "universal"),
            # csv=data.table::fread(userFile()$datapath),#can use vroom but fread may be faster for large files
            tsv = vroom::vroom(userFile()$datapath, delim = "\t", .name_repair = "universal"),
            txt = vroom::vroom(userFile()$datapath, show_col_types = FALSE, .name_repair = "universal"),
            validate("Invalid file. Please upload a .csv or .txt file")
          )
        }
      })

      return(dataframe)
    }
  )
}
