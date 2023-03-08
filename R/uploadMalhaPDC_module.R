#' Upload Malha PDF Input
#' 
#' @import shiny
#' 

uploadMalhaPDCInput <- function(id) {
    ns = NS(id)
    tagList(
        fileInput(
            inputId = ns("uploadMalhaPDC"),
            label = "Carregar Arquivo Malha Plano Diretor Cicloviário (kml)",
            buttonLabel = "Procurar",
            accept = c(".kml"), 
            multiple = F
        ),
        actionButton(
            inputId = ns("adicionar"),
            label = "Adicionar"
        )
    )
}

#' Upload Malha PDC Input
#' 
#' @import shiny

uploadMalhaPDCServer <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            
            observeEvent(input$adicionar, {
                req(input$uploadMalhaPDC)
                
                # show the waiter
                waiter::waiter_show(
                    color = transparent(.5),
                    html = spin_3() # use a spinner
                )
                
                # reading arquivo kml:
                data = sf::st_read(
                    input$uploadMalhaPDC$datapath,
                    layer = "Rede Cicloviária Complementar", 
                    type = 2
                )
                data = sf::st_zm(data, drop = T, what = "ZM")
                data = data[!sf::st_is_empty(data),]
                
                # saving:
                saveRDS(data, "data/malhaPDC.rds")
                
                # Interseção com malha permanente:
                strava = readRDS("data/strava.rds")
                strava[, flag_PDC := NULL]
                
                # merge espacial entre strava-malha PDC:
                strava = spatialMerge_stravaMalhaPDC(
                    strava = strava, 
                    malhaPDC = data
                )
                # saving:
                saveRDS(strava, "data/strava.rds")
                
                #  hide the waiter
                waiter::waiter_hide() 
                
                shinyWidgets::sendSweetAlert(
                    session = session,
                    title = "Sucesso!!",
                    text = "Arquivo salvo",
                    type = "success"
                )
            })
        }
    )
}


#' Upload Malha PDC App
#' 
#' @import shiny

uploadMalhaPDCApp <- function(){
    ui = fluidPage(
        shinyWidgets::useSweetAlert(),
        waiter::use_waiter(),
        sidebarLayout(
            sidebarPanel(
                uploadMalhaPDCInput("id1")
            ),
            mainPanel()
        )
    )
    server = function(input, output, session) {
        uploadMalhaPDCServer("id1")
    }
    shinyApp(ui, server)
}
