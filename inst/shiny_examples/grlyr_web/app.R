####################################################################
###
###   Shiny web application for grlyr: Ground Layer Estimation
###
####################################################################

require(shiny)
require(grlyr)

# dir <- system.file('shiny_examples', 'grlyr_web', package = 'grlyr')
# setwd(dir)
# shiny::shinyAppDir('.')

u1 <- paste0(
        'https://www.fs.fed.us/pnw/rma/fia-topics/',
        'documentation/field-manuals/documents/Annual/',
        '2018_FIA_Interior_Alaska_Supplement.pdf')
u2 <- 'https://github.com/phytomosaic/grlyr'
acc <- c('text/csv', 'text/comma-separated-values,text/plain', '.csv')

`ui` <- fluidPage(fluidPage(

        ### title and headers
        titlePanel(div(HTML('<em>grlyr</em> :
                            Ground Layer Estimation'))),
        br(),
        h4('Estimates biomass, carbon and nitrogen of moss and
        lichen ground layers, as part of the US Forest Service\'s
        Forest Inventory and Analysis program.'),
        br(),

        ### setup sidebar layout
        sidebarLayout(

                ### sidebar (left)
                sidebarPanel(h3('Upload'),
                             actionButton('show','File requirements'),
                             br(),br(),
                             fileInput('file1', NULL, accept=acc,
                                       buttonLabel=list(
                                               icon('upload'),
                                               'Upload CSV')
                             ),
                             tags$hr(),
                             h3('Download'),
                             'Summaries are automatic,
                             download them here:',
                             br(),br(),
                             downloadButton('dwnld','Download'),
                             tags$hr(),
                             h3('Further info'),
                             p('Source code: '),
                             p(a(u2, href = u2)),
                             p('Contact maintainer: '),
                             p(a('smithr2@oregonstate.edu',
                                 href = 'smithr2@oregonstate.edu')),
                             p('Sampling protocol: '),
                             p(a('Download PDF',
                                 href = u1)),
                             br(),
                             width = 4),

                ### main panel (right, upper)
                mainPanel(h3(textOutput('hdr1')),
                          tableOutput('plot_summaries_contents'),
                          tags$hr(),
                          h3(textOutput('hdr2')),
                          tableOutput('fg_summaries_contents')
                )
        )
),
title='grlyr : Ground Layer Estimation'
)

`server` <- function(input, output) {

        ### observe button click to show dialog box
        observeEvent(input$show, {showModal(
                modalDialog(
                        title = 'File requirements',
                        p('Your uploaded file must be a CSV file in
                        \'long\' format (each row is one observation
                        of a functional group in one microquad).'),
                        div(HTML('If you used the <b>FIA forestland
                        protocol</b>, then column names and possible
                        values, must be:')),
                        tags$pre('`plot`\t\t= any values
`microquad`\t= integers\t= 5,10,15,20
`fg`\t\t= categories\t= CC, CO, LF, LLFOL, LLFRU, LNFOL, LNFRU,
\t\t\t\t\t MF, MN, MS, MT, VF, VS
`cover`\t\t= percentages\t= 0, 0.1, 1, 2, 5, 10, 25, 50, 75, 95, 99
`depth`\t\t= inches\t= 0, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16
`transect`\t= integers\t= 90, 270, 360, 180, 135, 315, 45, 225'),
                        div(HTML('If you used the <b>rangeland variant
                        protocol</b>, then column names and possible
                        values, must be:')),
                        tags$pre('`plot`\t\t= any values
`microquad`\t= integers\t= 1 thru 32,
`fg`\t\t= categories\t= CBIND, CCYANO, CN, CO, CROCK, CSOIL,
\t\t\t\t\t LF, LLFOL, LLFRU, LNFOL, LNFRU,
\t\t\t\t\t MF, MN, MS, MT, MTL, VF, VS
`cover`\t\t= percentages\t= 0, 0.1, 1, 2, 5, 10, 25, 50, 75, 95, 99
`depth`\t\t= inches\t= 0, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16'),
                        p('Missing values not permitted in
                          first 5 columns.'),
                        p('Metric values for `depth` are also
                          accepted.'),
                        easyClose = TRUE
                ))
        })

        ### reactive expression for user-selected CSV file input
        CALCS <- reactive({
                inFile <- input$file1
                if (is.null(input$file1)) {
                        return(NULL)
                } else {
                        est <- read.csv(inFile$datapath, header = T)
                        x   <- grlyr::calc_biomass( est )
                        o1  <- grlyr::summary_plot( x )
                        o2  <- grlyr::summary_fg( x )
                        from <- c('plot',
                                  'total_mn', 'total_sd',
                                  'lich_mn', 'lich_sd',
                                  'moss_mn', 'moss_sd',
                                  'c_mn', 'c_sd',
                                  'n_mn', 'n_sd',
                                  'vol_mn', 'vol_sd',
                                  'cover_mn', 'cover_sd',
                                  'depth_mn', 'depth_sd',
                                  'fgr')
                        to <- c('Plot_id',
                                'Mean_biomass', 'SD_biomass',
                                'Mean_biomass_lichen',
                                'SD_biomass_lichen',
                                'Mean_biomass_mosses',
                                'SD_biomass_mosses',
                                'Mean_C', 'SD_C',
                                'Mean_N', 'SD_N',
                                'Mean_volume', 'SD_volume',
                                'Mean_cover', 'SD_cover',
                                'Mean_depth', 'SD_depth',
                                'Functional_group_richness')
                        colnames(o1)[colnames(o1) == from] <- to
                        from <- c('fg',
                                  'mass', 'masssd',
                                  'c', 'csd',
                                  'n', 'nsd',
                                  'vol', 'volsd',
                                  'cover', 'coversd')
                        to <- c('Fxl_grp',
                                'Mean_biomass', 'SD_biomass',
                                'Mean_C', 'SD_C',
                                'Mean_N', 'SD_N',
                                'Mean_volume', 'SD_volume',
                                'Mean_cover', 'SD_cover')
                        colnames(o2)[colnames(o2) == from] <- to
                        list(o1 = o1, o2 = o2)
                }
        })


        ### render PLOT summaries
        output$plot_summaries_contents <- renderTable(
                if (is.null( CALCS() )) {
                        return(NULL)
                } else {
                        xx <- CALCS()$o1
                        xx[,-1] <- round(xx[,-1],1)
                        uu <- c(' ',
                                rep('(kg/ha)', 10),
                                rep('(m3/ha)', 2),
                                rep('(percentage)',2),
                                rep('(cm)', 2), '(count)')
                        dimnames(xx)[[2]] <-
                                paste0(dimnames(xx)[[2]], '\n', uu)
                        head(xx)
                }
        )

        ### render FUNCTIONAL GROUP summaries
        output$fg_summaries_contents <- renderTable(
                if (is.null( CALCS() )){
                        return(NULL)
                } else {
                        xx <- CALCS()$o2
                        uu <- c('  ',
                                rep('(kg/ha)', 6),
                                rep('(m3/ha)', 2),
                                rep('(percentage)',2))
                        dimnames(xx)[[2]] <-
                                paste0(dimnames(xx)[[2]], '\n', uu)
                        xx
                }
        )

        ### dynamic headers appear only after summaries done
        output$hdr1 <- renderText(
                if (is.null( CALCS() ))
                        return(NULL)
                else
                        paste0('Plot summary (preview)')

        )
        output$hdr2 <- renderText(
                if (is.null( CALCS() ))
                        return(NULL)
                else
                        paste0('Functional group summary')

        )

        ### download the plot summaries
        output$dwnld <- downloadHandler(
                filename = function() {
                        paste('grlyr_summary_',
                              format(Sys.time(), '%Y-%m-%d_%H%M'),
                              '.csv', sep='')
                },
                content = function(file) {
                        write.csv(CALCS()$o1, file, row.names = FALSE)
                })
}

### run the shiny app
shinyApp(ui, server)
