# app.R file
library(shiny)
library(dplyr)
library(tibble)
library(purrr)
library(shinythemes)
library(bslib)

source("./Modules/Datload.R")
source("./Modules/Datproc.R")
source("./Modules/Datcalc.R")
source("./Modules/Plotone.R")
source("./Modules/Plotsall.R")
source("./Modules/ResTab.R")
source("./Modules/Makeplot.R")

ThisVersion <- "version 0.3.4"
ThisApp <- "Simple clotlysis app"
ui <- fluidPage(

  navbarPage(title = ThisApp),
  thematic::thematic_shiny(),
  theme = bs_theme(bootswatch = "lumen"),
  sidebarLayout(
    sidebarPanel(
      tags$style(".well {background-color:#FEFEFE;}"), # colour for sidebar
      csvFileUI("datafile", h6("Load user data")),
      TabRes_ui("x"),
      oneplot_ui("x"),
      card(
        card_header(h6("Citation for publications"), class = "bg-dark"),
        helpText(h6(
          tags$i("Longstaff C, Development of a Shiny app tool to simplify and standardize the analysis
              of hemostasis assay data: communication from the SSC of the ISTH, J Thromb Haemost, 15: 1044-6, 2017"),
          tags$a(href = "https://onlinelibrary.wiley.com/doi/10.1111/jth.13656", "DOI 10.1111/jth.13656")
        )),
        card_header(h6("Contact details"), class = "bg-light"),
        # tags$a(href="mailto: drclongstaff@gmail.com", "drclongstaff@gmail.com"))),

        tags$a(href = "mailto: drclongstaff@gmail.com", "drclongstaff@gmail.com"),
        helpText(h6(ThisApp, ThisVersion, " last accessed", Sys.Date()), ),
        card_header(
          class = "bg-light",
          tags$a(href = "https://drclongstaff.github.io/shiny-clots/", "Links to other apps and help notes"),
          tags$br(),
          tags$a(href = "https://www.youtube.com/@colinlongstaff7270", "Youtube channel of help videos")
        )
      ),
    ),
    mainPanel(
      tabsetPanel(
        type = "tab",
        tabPanel(
          "Plot", # main opening page

          DatProc_ui("x"),
          plot_ui("x"),
          filenames_ui("x"),
          card(
            height = 450,
            full_screen = TRUE, # is expandable
            card_header("Results table",
              class = "bg-warning"
            ),
            card_body(
              style = "background-color: #FFFCF4;",
              layout_sidebar(
                fillable = TRUE,
                sidebar = sidebar(
                  open = TRUE,
                  fluidRow(
                    radioButtons("atab",
                      label = h6("Display results"),
                      choices = c("all results", "selected"), selected = "selected", inline = TRUE
                    )
                  ),
                  fluidRow(
                    selectInput("obs1", label = h6("Selected"), choices = c(colnames(myRes)), selected = "lysTime")
                  )
                ),
                tableOutput("obs"),
                tableOutput("tabres"), # For results file

                tableOutput("whichtab")
              )
            )
          )
        ),
        tabPanel(
          "Data",
          fluidRow(
            column(9, radioButtons("dattab",
              label = "Display data",
              choices = c("raw data", "processed data"), selected = "raw data", inline = TRUE
            )),
            tableOutput("datatabs")
          )
        ),
        tabPanel(
          "Help",
          # shinythemes::themeSelector(), #include this to allow the user to change themes
          tags$blockquote(h6(
            "► The app loads, displays and analyses a set of data with some typical settings. You can adjust the settings to get a feel of how the app works ",
            tags$br(),
            tags$br(),
            " ► You can load your own data as csv, txt or xlsx and the format will be detected ",
            tags$br(),
            tags$br(),
            " ► Your data should be organised as in the example data (see the Data tab) with a column of Time followed by any number  >1 of columns of absorbance readings",
            tags$br(),
            tags$br(),
            "► Try to avoid spaces or empty wells in data files",
            tags$br(),
            tags$br(),
            "► The green boxes are where parameters that affect curve analysis are adjusted: % lysis, threshold for interpolation, and baseline",
            tags$br(),
            tags$br(),
            "► The default setting of 50 'percent change' calculates a time for 50% clotting and lysis but can be changed",
            tags$br(),
            tags$br(),
            "► The code traps errors but if the data is noisy and the program crashes try increasing the 'threshold' absorbance value",
            tags$br(),
            tags$br(),
            "► Increasing the 'threshold' gives more robust analysis but is less accurate because it avoids interpolation and gives results at the nearest time point ",
            tags$br(),
            tags$br(),
            "► 'Baseline options' allow the baseline to be set in 3 ways using the radio buttons and additional input text boxes",
            tags$br(),
            tags$br(),
            "► Set the baseline to find a satisfactory point for 100% lysis ",
            tags$br(),
            tags$br(),
            "► The blue boxes allow adjustable settings that affect the way clotlysis curves are plotted",
            tags$br(),
            tags$br(),
            "► The orange box contains the table of results, either selected parameter or the complete set of results",
            tags$br(),
            tags$br(),
            "► Plots and results can be expanded to full screen for detailed inspection",
            tags$br(),
            tags$br(),
            "►There is a web page with links to my other apps and some detailed help pages",
            tags$a(href = "https://drclongstaff.github.io/shiny-clots/", "Links to other apps")
          )),
          # tags$img(src="Plots1.png", width=500, height=300), #images may be added
          # tags$img(src="Table1.png", width=500, height=200)
        )
      )
    )
  )
)

server <- function(input, output, session) {
  datafile <- csvFileServer("datafile", stringsAsFactors = FALSE)
  procfile <- DatProc_server("x", reactive({
    datafile()
  }))
  plotfile <- oneplot_server("x", reactive({
    procfile()
  }), resfile)
  mplotfile <- plot_server("x", reactive({
    procfile()
  }), resfile)
  resfile <- TabRes_server("x", reactive({
    procfile()
  }))

  theobs <- reactive({
    resfile() |>
      select(input$obs1) |>
      pull()
  })
  myobs <- filenames_server("x", theobs)

  output$whichtab <- renderTable({
    switch(input$atab,
      "selected" = myobs(),
      "all results" = resfile()
    )
  })

  output$datatabs <- renderTable({
    switch(input$dattab,
      "raw data" = datafile(),
      "processed data" = procfile()
    )
  })
}


shinyApp(ui, server)
