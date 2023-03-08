#' Upload Strava dataset Input
#' 
#' @import shiny

uploadStravaInput <- function(id) {
    ns = NS(id)
    tagList(
        fileInput(
            inputId = ns("uploadStravaTrafego"),
            label = "Carregar Arquivos Strava Metro",
            buttonLabel = "Procurar",
            accept = c(".csv"), 
            multiple = T,
            placeholder = "Nenhum arquivo selecionado"
        ),
        actionButton(
            inputId = ns("adicionar"),
            label = "Adicionar"
        )
    )
}

#' Upload Strava dataset Server
#' 
#' @import shiny
#' @import waiter
#' @import data.table
#' @import sf
#' 
uploadStravaServer <- function(id) {
    options(shiny.maxRequestSize = 100 * 1024^2)
    moduleServer(
        id,
        function(input, output, session) {
            
            observeEvent(input$adicionar, {
                
                req(input$uploadStravaTrafego)
                
                # show the waiter
                waiter_show(
                    color = transparent(.5),
                    html = spin_3() # use a spinner
                )

                # get full filename:
                flnms = input$uploadStravaTrafego$datapath
                
                # copying shape files to data directory:
                file.copy(
                    flnms,
                    'data/'
                )
                
                # new names
                flnms_new = str_replace(
                    basename(flnms),
                    tools::file_path_sans_ext(basename(flnms)),
                    "strava"
                )
                
                # renaming:
                file.rename(
                    str_c("data/", basename(flnms)),
                    str_c("data/", flnms_new)
                )
                
                # reading:
                rides = fread("data/strava.csv")
                ruas = st_read("data/strava.shp")
                setDT(ruas)

                # processing:
                rides2 = processing_strava(
                    metadata = rides,
                    shape = ruas, 
                    dateVar = "month"
                )
                
                # deleting files:
                file.remove(str_c("data/", flnms_new))
                
                # saving:
                saveRDS(rides2, "data/rides2.rds")
                
                #  hide the waiter
                waiter_hide() 
                
                shinyWidgets::sendSweetAlert(
                    session = session,
                    title = "Sucesso!!",
                    text = "Arquivo salvo com sucesso",
                    type = "success"
                )
                
            })
        }
    )
}

#' Upload Strava dataset App
#' 
#' @import shiny

uploadStravaApp <- function(){
    ui = fluidPage(
        shinyWidgets::useSweetAlert(),
        waiter::use_waiter(),
        sidebarLayout(
            sidebarPanel(
                uploadStravaInput("id1")
            ),
            mainPanel()
        )
    )
    
    server = function(input, output, session) {
        uploadStravaServer("id1")
    }
    shinyApp(ui, server)
}
