#' Upload Malha Permanente Input
#' 
#' @import shiny

uploadMalhaPermanenteInput <- function(id) {
    ns = NS(id)
    tagList(
        fileInput(
            inputId = ns("uploadMalhaPermanente"),
            label = "Carregar Arquivo Malha Permanente (geojson)",
            buttonLabel = "Procurar",
            accept = c(".geojson"),
            placeholder = "Nenhum arquivo selecionado",
            multiple = F
        ),
        actionButton(
            inputId = ns("adicionar"),
            label = "Adicionar"
        )
    )
}

#' Upload Malha Permanente Server
#' 
#' @import shiny
#' 
uploadMalhaPermanenteServer <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {

            observeEvent(input$adicionar, {
                
                req(input$uploadMalhaPermanente)
                
                # show the waiter
                waiter::waiter_show(
                    color = transparent(.5),
                    html = spin_3() # use a spinner
                )
                
                # reading:
                malhaPermanente = geojsonsf::geojson_sf(input$uploadMalhaPermanente$datapath)
                malhaPermanente = malhaPermanente[!sf::st_is_empty(malhaPermanente),]
                # saving:
                saveRDS(malhaPermanente, "data/malhaPermanente.rds")
                
                # 2. merge espacial com strava processado:
                # reading dados de trafego do strava processado:
                rides2 = readRDS("data/rides2.rds")
                # spatial merge 
                rides3 = spatialMerge_stravaMalhaPermanente(
                    strava = rides2,
                    malhaPermanente = malhaPermanente
                )

                # saving:
                saveRDS(rides3, "data/strava.rds")
                
                # hide the waiter
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


#' Upload Malha Permanente App
#' 
#' @import shiny
#' 
uploadMalhaPermanenteApp <- function(){
    ui = fluidPage(
        shinyWidgets::useSweetAlert(),
        waiter::use_waiter(),
        sidebarLayout(
            sidebarPanel(
                uploadMalhaPermanenteInput("id1")
            ),
            mainPanel()
        )
    )
    
    server = function(input, output, session) {
        uploadMalhaPermanenteServer("id1")
    }
    shinyApp(ui, server)
}
