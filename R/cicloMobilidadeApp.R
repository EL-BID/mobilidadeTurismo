#' Ciclo de Mobilidade UI
#' 
#' @description 
#' User Interface (UI) for the Proof of Concept Ciclo de Mobilidade from 
#' the city of Recife.
#' 
#' @details 
#' This function only provides HTML for the User Interface (frontend)
#' of the application. 
#'
#' @return the HTML for the frontend.
#' 
#' @import shiny
#' @import shinydashboard
#' @import htmltools
#' 
#' @export

app_ui = function() { 
    
    ## header:
    myHeader =  shinydashboardPlus::dashboardHeader(
        title = "Pedala Recife",
        tags$li(a(href = 'https://www2.recife.pe.gov.br/',
                  img(src = 'dual_logo3.png',
                      title = "Pedala Recife", height = "50px"),
                  style = "padding-top:10px; padding-bottom:10px;"),
                class = "dropdown")
    )
    
    ## sidebar:
    mySidebar =  shinydashboardPlus::dashboardSidebar(
        width = 4,
        collapsed = F,
        sidebarMenu(
            id = "sidebarid",
            menuItem(
                "Ciclo de Mobilidade", 
                tabName = "analiseTrafego",
                startExpanded = T,
                icon = icon("traffic-light", lib = "font-awesome", verify_fa=F)
            ),
            conditionalPanel(
                'input.sidebarid == "analiseTrafego"',
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
            ),
            menuItem(
                "Upload",
                tabName = "uploadFiles",
                icon = icon("upload", lib = "font-awesome", verify_fa = F)
            )
        )
    )
    
    ## Body:
    myBody = dashboardBody(
            shinyWidgets::useSweetAlert(),
            waiter::use_waiter(),
            waiter::autoWaiter(
                 color = transparent(.5),
                 html = spin_3() # use a spinner
            ),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tabItems(
                tabItem(
                    tabName = "analiseTrafego",
                    analiseTrafegoInput("id1")
                ),
                tabItem(
                    tabName = "uploadFiles",
                    uploadInput("uploadFiles")
                )
            )
        )
    
    
    # App:
    
    # customized theme:
    mytheme = fresh::create_theme(
            fresh::adminlte_color(
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
    
    # UI:
    ui = shinydashboardPlus::dashboardPage(
        freshTheme = mytheme,
        header = myHeader,
        sidebar = mySidebar,
        body = myBody
    )
    
    ui
}

#' Ciclo de Mobilidade Server
#' 
#' @description 
#' Server (Backend) for the Proof of Concept Ciclo de Mobilidade from 
#' the city of Recife.
#' 
#' #' @details 
#' This function only provides the code for the Server (backend)
#' of the application. 
#' 
#' @return function with the call for the Server.
#' 
#' @import shiny
#' 
#' @export
    
app_server = function()  {
    # Server:
    server = function(input, output, session) {
        analiseTrafegoServer("id1", nome_variavel = reactive(input$variavel))
        uploadServer("uploadFiles")
    }
    
    server
}

#' Ciclo Mobilidade App
#' 
#' @export
cicloMobilidadeApp = function() {
    shiny::shinyApp(ui = app_ui(), server = app_server())
}