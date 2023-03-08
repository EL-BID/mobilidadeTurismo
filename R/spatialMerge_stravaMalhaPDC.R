#' Spatial merge between Strava and Cycling Network Proposal
#' 
#' @description 
#' This function will perform a spatial merge between dataset from Strava Metro
#' and the proposal cycling network.
#' 
#' @details 
#' The spatial merge is done using a buffering of the cycling network (15 meters),
#' and then joining it with ride information from Strava by using the 'within distance (5 meters)'
#' criteria. This process is able to identify the rides that are covered by the
#' cycling network.
#' 
#' 
#' @param strava data.frame/data.table object with ride information from Strava. 
#' This is the result from the processing_strava() function.
#' @param malhaPermanente data.frame with the geographical features from the 
#' cycling network. Basically, it is the object resulted from reading the .shp file
#' using the sf package.
#'
#' @return a data.table object with ride information merged with 
#' geographical features from the cycling network.
#' 
#' @import data.table
#' @import sf
#' 
#' @export

spatialMerge_stravaMalhaPDC <- function(strava,
                                        malhaPDC) {
    ## Identificar trechos do strava com cobertura de malha cicloviária
    
    ## dois argumentos: 
    ##  1. malhaPermanente
    ##  2. strava formatado
    rides = copy(strava)
    
    # retorno:
    # base do strava com flag de cobertura.
    
    # spatial merge to associate edgeUID and osmId to malha permanente
    malhaBuffered = st_join(
        st_as_sf(rides[, .(osm_reference_id, edge_uid, name, geometry)]),
        st_buffer(st_as_sf(malhaPDC), dist = 15),
        join = st_is_within_distance,
        left= F,
        dist = 5
    )
    setDT(malhaBuffered)
    malhaBuffered
    
    # merge to identify paths covered by cycleway
    foo = merge(
        rides |> dplyr::select(edge_uid, osm_reference_id),
        malhaBuffered |> dplyr::select(-geometry, -name),
        all.x = T,
        by= c("edge_uid", "osm_reference_id")
    )
    
    # flag to identify paths covered by cycleway:
    foo[, flag_PDC := ifelse(is.na(Name), "Não Cobertura", "Cobertura")]
    foo[, c("Name", "Description") := NULL]
    foo = unique(foo)
    foo # flag de cobertura para cada trecho
    
    rides = merge(rides, 
                  foo, 
                  by = c("edge_uid", "osm_reference_id"))
    rides
}

