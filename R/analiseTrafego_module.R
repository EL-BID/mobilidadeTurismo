#' Analise Trafego Input
#' 
#' @import shiny
#' @import shinydashboard
#' @import htmltools
#' 

analiseTrafegoInput = function(id) {
    ns = NS(id)
    tagList(
        fluidRow(
            box(width = 4,
                title = tags$strong("% de Ruas com Tráfego Baixo"),
                valueBoxOutput(
                    width = 12,
                    outputId = ns("percTrafegoBaixo")
                ),
                valueBoxOutput(
                     width = 12,
                    outputId = ns("percTrafegoBaixo_CPDC")
                ),
                valueBoxOutput(
                     width = 12,
                    outputId = ns("percTrafegoBaixo_SPDC")
                )
                
            ),
            box(width = 4,
                title = tags$strong("% de Ruas com Tráfego Médio"),
                valueBoxOutput(
                    width=12,
                    outputId = ns("percTrafegoMedio")
                ),
                valueBoxOutput(
                    width=12,
                    outputId = ns("percTrafegoMedio_CPDC")
                ),
                valueBoxOutput(
                    width=12,
                    outputId = ns("percTrafegoMedio_SPDC")
                )
            ),
            box(width = 4, 
                title = tags$strong("% de Ruas com Tráfego Alto"),
                valueBoxOutput(
                    width=12,
                    outputId = ns("percTrafegoAlto")
                ),
                valueBoxOutput(
                    width=12,
                    outputId = ns("percTrafegoAlto_CPDC")
                ),
                valueBoxOutput(
                    width=12,
                    outputId = ns("percTrafegoAlto_SPDC")
                )
            )
        ),
        fluidRow(
            box(width=12,
            tabsetPanel(
                tabPanel(
                    title = "Todas",
                    box(width = 4,
                        #solidHeader = T,
                        #status = "primary",
                        title = tags$strong("Ranking Ruas com Tráfego Baixo"),
                        div(style="font-size:150%",
                            DT::dataTableOutput(
                                outputId = ns("nomeRuas_baixo")
                            )
                        )
                    ),
                    box(width = 4,
                        #solidHeader = T,
                        #status = "primary",
                        title = tags$strong("Ranking Ruas com Tráfego Médio"),
                        div(style="font-size:150%",
                            DT::dataTableOutput(
                                outputId = ns("nomeRuas_medio")
                            )
                        )
                    ),
                    box(width = 4,
                        #solidHeader = T,
                        #status = "primary",
                        title = tags$strong("Ranking Ruas com Tráfego Alto"),
                        div(style="font-size:150%",
                            DT::dataTableOutput(
                                outputId = ns("nomeRuas_alto")
                            )
                        )
                    )
                ),
                tabPanel(
                    title = "Com previsão PDC",
                    box(width = 4,
                        #solidHeader = T,
                        #status = "primary",
                        title = tags$strong("Ranking Ruas com Tráfego Baixo"),
                        div(style="font-size:150%",
                            DT::dataTableOutput(
                                outputId = ns("nomeRuas_baixo_CPDC")
                            )
                        )
                    ),
                    box(width = 4,
                        #solidHeader = T,
                        #status = "primary",
                        title = tags$strong("Ranking Ruas com Tráfego Médio"),
                        div(style="font-size:150%",
                            DT::dataTableOutput(
                                outputId = ns("nomeRuas_medio_CPDC")
                            )
                        )
                    ),
                    box(width = 4,
                        #solidHeader = T,
                        #status = "primary",
                        title = tags$strong("Ranking Ruas com Tráfego Alto"),
                        div(style="font-size:150%",
                            DT::dataTableOutput(
                                outputId = ns("nomeRuas_alto_CPDC")
                            )
                        )
                    )
                ),
                tabPanel(
                    title = "Sem previsão PDC",
                    box(width = 4,
                        #solidHeader = T,
                        #status = "primary",
                        title = tags$strong("Ranking Ruas com Tráfego Baixo"),
                        div(style="font-size:150%",
                            DT::dataTableOutput(
                                outputId = ns("nomeRuas_baixo_SPDC")
                            )
                        )
                    ),
                    box(width = 4,
                        #solidHeader = T,
                        #status = "primary",
                        title = tags$strong("Ranking Ruas com Tráfego Médio"),
                        div(style="font-size:150%",
                            DT::dataTableOutput(
                                outputId = ns("nomeRuas_medio_SPDC")
                            )
                        )
                    ),
                    box(width = 4,
                        #solidHeader = T,
                        #status = "primary",
                        title = tags$strong("Ranking Ruas com Tráfego Alto"),
                        div(style="font-size:150%",
                            DT::dataTableOutput(
                                outputId = ns("nomeRuas_alto_SPDC")
                            )
                        )
                    )
                )
            )
            )
        ),
        fluidRow(
            box(width = 12,
                title = "Malha Cicloviária",
                leafletOutput(
                    outputId = ns("mymap"),
                    height = 900
                )
            )
        )
    )
}


#' Analise Trafego Server
#' 
#' @import shiny
#' @import shinydashboard
#' @import data.table
#' @import leaflet

analiseTrafegoServer <- function(id, nome_variavel) {
    stopifnot(is.reactive(nome_variavel))
    
    moduleServer(
        id,
        function(input, output, session) {
            
            # shapefile da malha cicloviaria permanente:
            malhaPermanente <- reactivePoll(
                intervalMillis = 1000, session = session,
                checkFunc = function() file.mtime("data/malhaPermanente.rds"),
                valueFunc = function() readRDS("data/malhaPermanente.rds")
            )
            
            malhaPDC = reactivePoll(
                intervalMillis = 1000, session = session,
                checkFunc = function() file.mtime("data/malhaPDC.rds"),
                valueFunc = function() readRDS("data/malhaPDC.rds")
            )
            
            # dados de trafego do strava:
            rides = reactivePoll(
                intervalMillis = 1000, session = session,
                checkFunc = function() file.mtime("data/strava.rds"),
                valueFunc = function() readRDS("data/strava.rds")
            )
            
            # filtered data:
            filteredData = reactive({
                dplyr::select(rides(), "name", "flag", "flag_PDC", nome_variavel(), "geometry") %>% 
                    tidyr::drop_na(nome_variavel())
            }) %>% 
                bindCache(nome_variavel())
            
            # 2. compute stats de taxa de Ñ cobertura por nível de tráfego:
            out = reactive({
                computeStatsTrafego(
                    filteredData(), 
                    nome_variavel = nome_variavel()
                )
            }) %>% 
                bindCache(nome_variavel())
            
            ##================================================================##
            
            # % de ruas com trafego baixo sem cobertura 
            output$percTrafegoBaixo = renderValueBox({
                valueBox(
                    color = "red",
                    value = paste0(100*out()$trafegoBaixo$percentual$all, "%"),
                    subtitle = tags$p("% de ruas com tráfego baixo sem cobertura", style = "font-size: 175%;")
                )
            })
            
            # % de ruas com trafego baixo sem cobertura C/PDC
            output$percTrafegoBaixo_CPDC = renderValueBox({
                valueBox(
                    color = "maroon",
                    value = paste0(100*out()$trafegoBaixo$percentual$CPDC, "%"),
                    subtitle = tags$p("% com previsão PDC", style = "font-size: 175%;")
                )
            })
            
            # % de ruas com trafego baixo sem cobertura S/PDC
            output$percTrafegoBaixo_SPDC = renderValueBox({
                valueBox(
                    color = "orange",
                    value = paste0(100*out()$trafegoBaixo$percentual$SPDC, "%"),
                    subtitle = tags$p("% sem previsão PDC", style = "font-size: 175%;")
                )
            })
            
            
            
            # % de ruas com trafego medio sem cobertura 
            output$percTrafegoMedio = renderValueBox({
                valueBox(
                    color = "red",
                    value = paste0(100*out()$trafegoMedio$percentual$all, "%"),
                    subtitle = tags$p("% de ruas com tráfego médio sem cobertura",  style = "font-size: 175%;")
                )
            })
            
            # % de ruas com trafego medio sem cobertura 
            output$percTrafegoMedio_CPDC = renderValueBox({
                valueBox(
                    color = "maroon",
                    value = paste0(100*out()$trafegoMedio$percentual$CPDC, "%"),
                    subtitle = tags$p("% com previsão PDC",  style = "font-size: 175%;")
                )
            })
            
            # % de ruas com trafego medio sem cobertura 
            output$percTrafegoMedio_SPDC = renderValueBox({
                valueBox(
                    color = "orange",
                    value = paste0(100*out()$trafegoMedio$percentual$SPDC, "%"),
                    subtitle = tags$p("% sem previsão PDC",  style = "font-size: 175%;")
                )
            })
            
            
            
            # % de ruas com trafego alto sem cobertura 
            output$percTrafegoAlto = renderValueBox({
                valueBox(
                    color = "red",
                    value = paste0(100*out()$trafegoAlto$percentual$all, "%"),
                    subtitle = tags$p("% de ruas com tráfego alto sem cobertura",  style = "font-size: 175%;")
                )
            })
            
            # % de ruas com trafego alto sem cobertura 
            output$percTrafegoAlto_CPDC = renderValueBox({
                valueBox(
                    color = "maroon",
                    value = paste0(100*out()$trafegoAlto$percentual$CPDC, "%"),
                    subtitle = tags$p("% com previsão PDC",  style = "font-size: 175%;")
                )
            })
            
            # % de ruas com trafego alto sem cobertura 
            output$percTrafegoAlto_SPDC = renderValueBox({
                valueBox(
                    color = "orange",
                    value = paste0(100*out()$trafegoAlto$percentual$SPDC, "%"),
                    subtitle = tags$p("% sem previsão PDC",  style = "font-size: 175%;")
                )
                
            })
            
            # nome das ruas com trafego baixo sem cobertura
            output$nomeRuas_baixo = DT::renderDataTable({
                DT::datatable(
                    data.table(Rua = out()$trafegoBaixo$ruas$all ),
                    rownames = T,
                    colnames = "",
                    options = list(
                        dom = "ftl",
                        ordering = F,
                        lengthMenu = list(c(5, 10, 15, 20, -1), c('5', '10', '15', '20', 'Tudo')),
                        language = list(url = "https://cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json")
                    )
                )
            })
            
            # nome das ruas com trafego medio sem cobertura
            output$nomeRuas_medio = DT::renderDataTable({
                data.table(Rua = out()$trafegoMedio$ruas$all ) |> 
                    DT::datatable(
                        rownames = T,
                        colnames = "",
                        options = list(
                            dom = "ftl",
                            ordering = F,
                            lengthMenu = list(c(5, 10, 15, 20, -1), c('5', '10', '15', '20', 'Tudo')),
                            language = list(url = "https://cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json")
                        )
                    )
            })
            
            # nome das ruas com trafego alto sem cobertura
            output$nomeRuas_alto = DT::renderDataTable({
                data.table(Rua = out()$trafegoAlto$ruas$all ) |> 
                    DT::datatable(
                        rownames = T, 
                        colnames = "",
                        options = list(
                            dom = "ftl",
                            ordering = F,
                            lengthMenu = list(c(5, 10, 15, 20, -1), c('5', '10', '15', '20', 'Tudo')),
                            language = list(url = "https://cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json")
                        )
                    )
            })
            
            # nome das ruas com trafego baixo sem cobertura
            output$nomeRuas_baixo_CPDC = DT::renderDataTable({
                DT::datatable(
                    data.table(Rua = out()$trafegoBaixo$ruas$CPDC ),
                    rownames = T,
                    colnames = "",
                    options = list(
                        dom = "ftl",
                        ordering = F,
                        lengthMenu = list(c(5, 10, 15, 20, -1), c('5', '10', '15', '20', 'Tudo')),
                        language = list(url = "https://cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json")
                    )
                )
            })
            
            # nome das ruas com trafego medio sem cobertura
            output$nomeRuas_medio_CPDC = DT::renderDataTable({
                data.table(Rua = out()$trafegoMedio$ruas$CPDC ) |> 
                    DT::datatable(
                        rownames = T,
                        colnames = "",
                        options = list(
                            dom = "ftl",
                            ordering = F,
                            lengthMenu = list(c(5, 10, 15, 20, -1), c('5', '10', '15', '20', 'Tudo')),
                            language = list(url = "https://cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json")
                        )
                    )
            })
            
            # nome das ruas com trafego alto sem cobertura
            output$nomeRuas_alto_CPDC = DT::renderDataTable({
                data.table(Rua = out()$trafegoAlto$ruas$CPDC ) |> 
                    DT::datatable(
                        rownames = T, 
                        colnames = "",
                        options = list(
                            dom = "ftl",
                            ordering = F,
                            lengthMenu = list(c(5, 10, 15, 20, -1), c('5', '10', '15', '20', 'Tudo')),
                            language = list(url = "https://cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json")
                        )
                    )
            })
            
            # nome das ruas com trafego baixo sem cobertura
            output$nomeRuas_baixo_SPDC = DT::renderDataTable({
                DT::datatable(
                    data.table(Rua = out()$trafegoBaixo$ruas$SPDC ),
                    rownames = T,
                    colnames = "",
                    options = list(
                        dom = "ftl",
                        ordering = F,
                        lengthMenu = list(c(5, 10, 15, 20, -1), c('5', '10', '15', '20', 'Tudo')),
                        language = list(url = "https://cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json")
                    )
                )
            })
            
            # nome das ruas com trafego medio sem cobertura
            output$nomeRuas_medio_SPDC = DT::renderDataTable({
                data.table(Rua = out()$trafegoMedio$ruas$SPDC ) |> 
                    DT::datatable(
                        rownames = T,
                        colnames = "",
                        options = list(
                            dom = "ftl",
                            ordering = F,
                            lengthMenu = list(c(5, 10, 15, 20, -1), c('5', '10', '15', '20', 'Tudo')),
                            language = list(url = "https://cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json")
                        )
                    )
            })
            
            # nome das ruas com trafego alto sem cobertura
            output$nomeRuas_alto_SPDC = DT::renderDataTable({
                data.table(Rua = out()$trafegoAlto$ruas$SPDC ) |> 
                    DT::datatable(
                        rownames = T, 
                        colnames = "",
                        options = list(
                            dom = "ftl",
                            ordering = F,
                            lengthMenu = list(c(5, 10, 15, 20, -1), c('5', '10', '15', '20', 'Tudo')),
                            language = list(url = "https://cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json")
                        )
                    )
            })
            
            # mapa:
            output$mymap <- renderLeaflet({

                # criando paleta de cores:
                pal = colorFactor(
                    palette = c("#ef3b2c", "#cb181d", "#67000d"),
                    levels = c("baixo", "médio", "alto")
                )

                # icon BikePE
                icons = makeIcon(
                    iconUrl = "www/faviconBikePE.ico",
                    iconWidth = 20,
                    iconHeight = 20
                )

                # icon sava Bike
                iconSalvaBike = makeIcon(
                    iconUrl = "www/faviconSalvaBike.ico",
                    iconWidth = 35,
                    iconHeight = 35
                )

                # plot map:
                leaflet(filteredData()) |>
                    setView(lat = -8.0663, lng = -34.9321, zoom = 13) %>%
                    addTiles() |>
                    addMarkers(
                        data = bikePE,
                        lng = ~longitude, lat = ~latitude,
                        label = ~nome2,
                        popup = ~content,
                        icon = icons,
                        group = "Estação Bike PE"
                    ) %>%
                    addMarkers(
                        data = salvaBike,
                        lng = ~longitude, lat = ~latitude,
                        label = ~local,
                        icon = iconSalvaBike,
                        group = "Estação Salva Bike"
                    ) %>%
                    addLayersControl(
                        overlayGroups = c("Estação Bike PE",
                                          "Estação Salva Bike",
                                          "Tráfego Reportado (Strava)",
                                          "Malha Cicloviária Permanente",
                                          "Malha Plano Diretor Cicloviário"
                        ),
                        options = layersControlOptions(collapsed = FALSE)
                    )  %>%
                    addPolylines(
                        data = st_as_sf(filteredData()),
                        color = ~pal(get(nome_variavel())),
                        label = ~name,
                        popup = ~paste(name, "<br>", "Nível tráfego:", get(nome_variavel())),
                        labelOptions = labelOptions(direction = "top"),
                        group = "Tráfego Reportado (Strava)"
                    ) %>%
                    addPolylines(
                        data = st_as_sf(malhaPermanente()),
                        color = "green",
                        fillOpacity = 1,
                        label = htmltools::HTML("<strong>Malha cicloviária</strong>"),
                        labelOptions = labelOptions(direction = "top"),
                        group = "Malha Cicloviária Permanente"
                    ) %>%
                    addPolylines(
                        data = st_as_sf(malhaPDC()),
                        color = "blue",
                        fillOpacity = 1,
                        label = htmltools::HTML("<strong>Malha Plano Diretor Cicloviário</strong>"),
                        labelOptions = labelOptions(direction = "top"),
                        group = "Malha Plano Diretor Cicloviário"
                    ) %>%
                    addLegend(
                        "bottomright",
                        title= "Tráfego",
                        pal = pal,
                        values = ~get(nome_variavel()),
                        opacity = 1,
                        group = "Tráfego Reportado (Strava)"
                    ) %>%
                    hideGroup(
                        group = c("Estação Bike PE",
                                  "Estação Salva Bike")
                    )
            }) %>%
                bindCache(nome_variavel())
        }
    )
}



#' Analise Trafego App
#' 
#' @import shiny
#' @import data.table
#' @import fresh
#' @import leaflet
#' @import waiter
#' 
analiseTrafegoApp <- function() {
    mytheme = create_theme(
        adminlte_color(
            red = "#a50f15",
            orange = "#cb181d",
            maroon = "#ef3b2c",
            
            green = "#3fff2d",
            blue = "#2635ff",
            yellow = "#feff6e",
            fuchsia = "#ff5bf8",
            navy = "#374c92",
            purple = "#615cbf",
            light_blue = "#5691cc"
        )
    )
    ui = shinydashboardPlus::dashboardPage(
        freshTheme = mytheme,
        header = shinydashboardPlus::dashboardHeader(),
        sidebar = shinydashboardPlus::dashboardSidebar(
            sidebarMenu(
                id = "sidebarid",
                menuItem(
                    text = "Análise do Tráfego",
                    tabName = "analise_trafego",
                    icon = icon("biking", lib = "font-awesome", verify_fa=F)
                ),
                conditionalPanel(
                    'input.sidebarid == "analise_trafego"',
                    selectInput(
                        inputId = "variavel", 
                        "Selecionar variável:", 
                        choices = c("Número de viagens" = "trip_count_cat", 
                                    "Número de viagens (trabalho)" = "commute_trip_count_cat",
                                    "Número de viagens (lazer)" = "leisure_trip_count_cat",
                                    "Número de viagens (manhã)" = "morning_trip_count_cat",
                                    "Número de viagens (noite)" = "evening_trip_count_cat",
                                    "Número de pessoas" = "people_count_cat",
                                    "Número de pessoas (homens)" = "male_people_count_cat",
                                    "Número de pessoas (mulher)" = "female_people_count_cat",
                                    "Número de pessoas (idade 13-19)" = "age_13_19_people_count_cat",
                                    "Número de pessoas (idade 20-34)" = "age_20_34_people_count_cat",
                                    "Número de pessoas (idade 35-54)" = "age_35_54_people_count_cat",
                                    "Número de pessoas (idade 55-64)" = "age_55_64_people_count_cat",
                                    "Número de pessoas (idade 65+)" = "age_65_plus_people_count_cat"
                        )
                    )
                )
            )
        ),
        body = dashboardBody(
            use_waiter(),
            autoWaiter(
                color = transparent(.5),
                html = spin_3() # use a spinner
            ),
            tabItems(
                tabItem(
                    tabName = "analise_trafego",
                    analiseTrafegoInput("id1")
                )
            )
        )
    )
    server = function(input, output, session) {
        var = reactive(input$variavel)
        analiseTrafegoServer("id1", nome_variavel = var)
    }
    shinyApp(ui, server)
}
