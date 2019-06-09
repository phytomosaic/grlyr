# Shiny web application for grlyr: Ground Layer Estimation

require(shiny)
require(grlyr)

`ui` <- fluidPage(
     fluidPage(

          ### title and headers
          titlePanel(
               div(HTML("<em>grlyr</em> : Ground Layer Estimation"))
          ),
          br(),
          h4("Estimates biomass, carbon and nitrogen of moss and
        lichen ground layers as part of the US Forest Service's Forest
        Inventory and Analysis program."),
          br(),

          ### main page has sidebar
          sidebarLayout(

               ### sidebar (left)
               sidebarPanel(
                    h3("Upload"),
                    actionButton("show", "File requirements"),
                    br(),br(),
                    fileInput(
                         'file1', 'Choose CSV file',
                         accept=c(
                              'text/csv',
                              'text/comma-separated-values,text/plain',
                              '.csv')),
                    tags$hr(),
                    h3("Download"),
                    'Summaries are automatic, download them here:',
                    br(),br(),
                    downloadButton('downloadData', 'Download'),
                    width = 4),

               ### main panel (right, upper)
               mainPanel(

                    ### upper
                    tableOutput('plot_summaries_contents')

                    # ### lower
                    # tags$hr(),
                    # tableOutput('fg_summaries_contents ')

               )

          ),

          ### footer
          p('Further information at: ',
            a("https://github.com/phytomosaic/grlyr",
              href = "https://github.com/phytomosaic/grlyr")),
          p('Contact maintainer at: ',
            a("smithr2@oregonstate.edu",
              href = "smithr2@oregonstate.edu")),
          br(),br(),br()
     )
)

`server` <- function(input, output) {

     ### observe button click to show dialog box
     observeEvent(input$show, {
          showModal(modalDialog(
               title = "File requirements",
               p("Your uploaded file must be:"),
               p("a CSV file in \"long\" format (each row is one
            observation of a functional group in one microquad)."),
               p("Column names, and their values, must be:"),
               tags$ol(
                    tags$li("`plot` = anything"),
                    tags$li("`microquad` = integers 1 thru 32"),
                    tags$li("`fg` = CC, CO, LF, LLFOL, LLFRU, LNFOL,
                LNFRU, MF, MN, MS, MT, VF, VS"),
                    tags$li("`cover` = 0, 0.1, 1, 2, 5, 10, 25,
                    50, 75, 95, 99"),
                    tags$li("`depth` = 0, 0.125, 0.25, 0.5, 1, 2,
                            4, 8, 16")
               ),
               p("Other values may \"work\" but give nonsense. Missing
              values not permitted in first 5 columns."),
               easyClose = TRUE
          ))
     })


     ### observe file input, to calculate plot summaries
     get_plot_summaries <- reactive({
          inFile <- input$file1
          if (is.null(input$file1)) return(NULL)
          # read user-selected CSV
          est <- read.csv(inFile$datapath, header = TRUE)
          # calculate biomass using the calc_biomass function
          x <- grlyr::calc_biomass(est)
          # summary by plots
          s <- grlyr::summary_plot(x)
          return(s)
     })
     ### render the plot summaries
     output$plot_summaries_contents <- renderTable(
          get_plot_summaries()
     )


     # ### observe file input, to calculate functional grp summaries
     # get_fg_summaries <- reactive({
     #      inFile <- input$file1
     #      if (is.null(input$file1)) return(NULL)
     #      # read user-selected CSV
     #      est <- read.csv(inFile$datapath, header = TRUE)
     #      # calculate biomass using the calc_biomass function
     #      x <- grlyr::calc_biomass(est)
     #      # summary by plots
     #      s <- grlyr::summary_fg(x)
     #      s
     # })
     # ### render the functional grp  summaries
     # output$fg_summaries_contents <- renderTable(
     #      get_fg_summaries()
     # )

     ### download the plot summaries
     output$downloadData <- downloadHandler(
          filename = function() {
               paste("grlyr_summary_",
                     # Sys.Date(),
                     format(Sys.time(), "%Y-%m-%d_%H-%M"),
                     ".csv", sep='')
          },
          content = function(file) {
               write.csv(get_plot_summaries(), file, row.names = FALSE)
          })

}

# ### run the shiny app
# shinyApp(ui, server)
