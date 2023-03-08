#' Compute stats for ride traffic
#'
#' @description 
#' This function will compute a number of statistics about the ride traffic 
#' for the specified nome_variable
#' 
#' @param rides a data.frame/data.table object with ride information from Strava.
#' @param nome_variavel a character object with the variable name to be analyzed.
#' 
#' @return a list object with statistics for streets categorized as low, medium 
#' and high traffic.
#' 
#' @import data.table
#' @importFrom tidyr drop_na
#' @importFrom dplyr filter pull
#' 
#' @export

computeStatsTrafego = function(rides, nome_variavel) {
    # ARGUMENTOS:
    # rides
    # nome_variavel = "trip_count_cat"
    rides = copy(rides)
    
    # Nome das ruas sem cobertura de ciclofaixa:
    # tráfego alto
    nomeRuas_trafegoAlto = rides[flag == "Não Cobertura" & get(nome_variavel) == "alto", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        pull(name)
    
    # tráfego médio
    nomeRuas_trafegoMedio = rides[flag == "Não Cobertura" & get(nome_variavel) == "médio", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        filter(!name %in% nomeRuas_trafegoAlto) %>% 
        pull(name)
    
    # tráfego baixo
    nomeRuas_trafegoBaixo = rides[flag == "Não Cobertura" & get(nome_variavel) == "baixo", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        filter(!name %in% c(nomeRuas_trafegoAlto, nomeRuas_trafegoMedio)) %>% 
        pull(name)
    
    # Percentural de ruas sem cobertura por nível tráfego:
    # baixo
    percTrafegoBaixo <- rides[get(nome_variavel) == "baixo", 
                              .N, 
                              c(nome_variavel, "flag")][, prop := N/sum(N)][flag == "Não Cobertura", prop]
    percTrafegoBaixo <- round(percTrafegoBaixo, 4)
    
    # médio
    percTrafegoMedio <- rides[get(nome_variavel) == "médio", 
                              .N, 
                              c(nome_variavel, "flag")][, prop := N/sum(N)][flag == "Não Cobertura", prop]
    percTrafegoMedio <- round(percTrafegoMedio, 4)
    
    # alto
    percTrafegoAlto <- rides[get(nome_variavel) == "alto", 
                             .N, 
                             c(nome_variavel, "flag")][, prop := N/sum(N)][flag == "Não Cobertura", prop]
    percTrafegoAlto <- round(percTrafegoAlto, 4)
    
    
    # COM PREVISAO DO PDC:
    # Nome das ruas sem cobertura de ciclofaixa:
    # tráfego alto
    nomeRuas_trafegoAlto_CPDC = rides[flag == "Não Cobertura" & flag_PDC == "Cobertura" & get(nome_variavel) == "alto", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        pull(name)
    
    # tráfego médio
    nomeRuas_trafegoMedio_CPDC = rides[flag == "Não Cobertura" & flag_PDC == "Cobertura" & get(nome_variavel) == "médio", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        filter(!name %in% nomeRuas_trafegoAlto_CPDC) %>% 
        pull(name)
    
    # tráfego baixo
    nomeRuas_trafegoBaixo_CPDC = rides[flag == "Não Cobertura" & flag_PDC == "Cobertura" & get(nome_variavel) == "baixo", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        filter(!name %in% c(nomeRuas_trafegoAlto_CPDC, nomeRuas_trafegoMedio_CPDC)) %>% 
        pull(name)
    
    # Percentural de ruas sem cobertura por nível tráfego:
    # baixo
    percTrafegoBaixo_CPDC <- rides[get(nome_variavel) == "baixo", 
                              .N, 
                              c(nome_variavel, "flag", "flag_PDC")][, prop := N/sum(N)][flag == "Não Cobertura" & flag_PDC == "Cobertura", prop]
    percTrafegoBaixo_CPDC <- round(percTrafegoBaixo_CPDC, 4)
    
    # médio
    percTrafegoMedio_CPDC <- rides[get(nome_variavel) == "médio", 
                              .N, 
                              c(nome_variavel, "flag", "flag_PDC")][, prop := N/sum(N)][flag == "Não Cobertura" & flag_PDC == "Cobertura", prop]
    percTrafegoMedio_CPDC <- round(percTrafegoMedio_CPDC, 4)
    
    # alto
    percTrafegoAlto_CPDC <- rides[get(nome_variavel) == "alto", 
                             .N, 
                             c(nome_variavel, "flag", "flag_PDC")][, prop := N/sum(N)][flag == "Não Cobertura" & flag_PDC == "Cobertura", prop]
    percTrafegoAlto_CPDC <- round(percTrafegoAlto_CPDC, 4)
    
    
    # SEM PREVISAO DO PDC:
    # Nome das ruas sem cobertura de ciclofaixa:
    # tráfego alto
    nomeRuas_trafegoAlto_SPDC = rides[flag == "Não Cobertura" & flag_PDC == "Não Cobertura" & get(nome_variavel) == "alto", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        pull(name)
    
    # tráfego médio
    nomeRuas_trafegoMedio_SPDC = rides[flag == "Não Cobertura" & flag_PDC == "Não Cobertura" & get(nome_variavel) == "médio", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        filter(!name %in% nomeRuas_trafegoAlto_SPDC) %>% 
        pull(name)
    
    # tráfego baixo
    nomeRuas_trafegoBaixo_SPDC = rides[flag == "Não Cobertura" & flag_PDC == "Não Cobertura" & get(nome_variavel) == "baixo", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        filter(!name %in% c(nomeRuas_trafegoAlto_SPDC, nomeRuas_trafegoMedio_SPDC)) %>% 
        pull(name)
    
    # Percentural de ruas sem cobertura por nível tráfego:
    # baixo
    percTrafegoBaixo_SPDC <- rides[get(nome_variavel) == "baixo", 
                              .N, 
                              c(nome_variavel, "flag", "flag_PDC")][, prop := N/sum(N)][flag == "Não Cobertura" & flag_PDC == "Não Cobertura", prop]
    percTrafegoBaixo_SPDC <- round(percTrafegoBaixo_SPDC, 4)
    
    # médio
    percTrafegoMedio_SPDC <- rides[get(nome_variavel) == "médio", 
                              .N, 
                              c(nome_variavel, "flag", "flag_PDC")][, prop := N/sum(N)][flag == "Não Cobertura" & flag_PDC == "Não Cobertura", prop]
    percTrafegoMedio_SPDC <- round(percTrafegoMedio_SPDC, 4)
    
    # alto
    percTrafegoAlto_SPDC <- rides[get(nome_variavel) == "alto", 
                             .N, 
                             c(nome_variavel, "flag", "flag_PDC")][, prop := N/sum(N)][flag == "Não Cobertura" & flag_PDC == "Não Cobertura", prop]
    percTrafegoAlto_SPDC <- round(percTrafegoAlto_SPDC, 4)
    
    
    # checking
    percTrafegoAlto 
    percTrafegoAlto_CPDC + percTrafegoAlto_SPDC
    
    percTrafegoMedio 
    percTrafegoMedio_CPDC + percTrafegoMedio_SPDC
    
    percTrafegoBaixo
    percTrafegoBaixo_CPDC + percTrafegoBaixo_SPDC
    
    setdiff(nomeRuas_trafegoAlto, c(nomeRuas_trafegoAlto_CPDC, nomeRuas_trafegoAlto_SPDC))
    setdiff(nomeRuas_trafegoMedio, c(nomeRuas_trafegoMedio_CPDC, nomeRuas_trafegoMedio_SPDC))
    setdiff(nomeRuas_trafegoBaixo, c(nomeRuas_trafegoBaixo_CPDC, nomeRuas_trafegoBaixo_SPDC))
    
    # output:
    out = list(
        trafegoBaixo = list(
            ruas = list(
                all = nomeRuas_trafegoBaixo,
                CPDC = nomeRuas_trafegoBaixo_CPDC,
                SPDC = nomeRuas_trafegoBaixo_SPDC
            ),
            percentual = list(
                all = percTrafegoBaixo,
                CPDC = percTrafegoBaixo_CPDC,
                SPDC = percTrafegoBaixo_SPDC
            )
        ),
        trafegoMedio = list(
            ruas = list(
                all = nomeRuas_trafegoMedio,
                CPDC = nomeRuas_trafegoMedio_CPDC,
                SPDC = nomeRuas_trafegoMedio_SPDC
            ),
            percentual = list(
                all = percTrafegoMedio,
                CPDC = percTrafegoMedio_CPDC,
                SPDC = percTrafegoMedio_SPDC
            )
        ),
        trafegoAlto = list(
            ruas = list(
                all = nomeRuas_trafegoAlto,
                CPDC = nomeRuas_trafegoAlto_CPDC,
                SPDC = nomeRuas_trafegoAlto_SPDC
            ),
            percentual = list(
                all = percTrafegoAlto,
                CPDC = percTrafegoAlto_CPDC,
                SPDC = percTrafegoAlto_SPDC
            )
        )
    )
    
    return(out)

}