#' Processing data from Strava dataset
#' 
#' @description 
#' This function will process the monthly dataset files from Strava Metro. Specifically, 
#' it will process the metadata file and merge with the related shapefile.
#' 
#' @details 
#' The information provided by Strava Metro is a set of directory containing data 
#' about all rides in hourly, daily and monthly period. This function is aimed to
#' be used with the monthly data, that is, files contained in the 
#' all_edges_monthly_* directory.
#' 
#' @param metadata data.frame containing metadata about the rides.
#' @param shape data.frame containing the geographic features (shapefile) of the rides.
#' @param dataVar character name of the column representing the "period" of the metadata file. 
#' For files from all_edges_monthly_*, this accounts for "month".
#'
#' @return a data.frame with the metadata merged with its geographic features.
#' 
#' @import data.table
#' 
#' @export

processing_strava = function(metadata, 
                             shape,
                             dateVar
                             ) {
    rides = copy(metadata)
    ruas = copy(shape)

    # 1. summing forward_* e backward_* variables:
    rides = rides[, .(
            edge_uid,
            osm_reference_id,
            dateVar = get(dateVar),
            trip_count = forward_trip_count + reverse_trip_count,
            people_count = forward_people_count + forward_people_count,
            commute_trip_count = forward_commute_trip_count + reverse_commute_trip_count,
            leisure_trip_count = forward_leisure_trip_count + reverse_leisure_trip_count,
            morning_trip_count = forward_morning_trip_count + reverse_morning_trip_count,
            evening_trip_count = forward_evening_trip_count + reverse_evening_trip_count,
            male_people_count = forward_male_people_count + reverse_male_people_count,
            female_people_count = forward_female_people_count + reverse_female_people_count,
            unspecified_people_count = forward_unspecified_people_count + reverse_unspecified_people_count,
            age_13_19_people_count = forward_13_19_people_count + reverse_13_19_people_count,
            age_20_34_people_count = forward_20_34_people_count + reverse_20_34_people_count,
            age_35_54_people_count = forward_35_54_people_count + reverse_35_54_people_count,
            age_55_64_people_count = forward_55_64_people_count + reverse_55_64_people_count,
            age_65_plus_people_count = forward_65_plus_people_count + reverse_65_plus_people_count,
            # take the avg of speed:
            average_speed = (forward_average_speed + reverse_average_speed)/2
        )
    ]
    setorderv(rides, c("edge_uid", "osm_reference_id", "dateVar"))
    rides
    
    # Assign all months for each 'edge_uid-osm_reference_id':
    # (some 'edge_uid-osm_reference_id' do not have obs every month).
    temp = unique(rides[, .(osm_reference_id, edge_uid, dateVar = list(unique(dateVar)))], 
                  by = c("osm_reference_id", "edge_uid"))
    temp = temp[, .(dateVar = unlist(dateVar)), .(edge_uid, osm_reference_id)]
    rides = merge(
        temp,
        rides,
        by = c("edge_uid", "osm_reference_id", "dateVar"),
        all.x = T
    )
    # assign 0 to columns with NA (no traffic in these months):
    nms_to_fill = setdiff(names(rides), c("edge_uid", "osm_reference_id", "dateVar"))
    rides[, (nms_to_fill) := purrr::map(.SD, ~nafill(.x, type = "const", fill = 0)),
          .SDcols = nms_to_fill][]
    
    # taking the avg of all months for each 'edge_uid-osm_reference_id':
    rides = rides[, purrr::map(.SD, mean), 
                  by = .(edge_uid, osm_reference_id), 
                  .SDcols = setdiff(names(rides), c("dateVar", "edge_uid", "osm_reference_id"))]
    
    # Categorizing variables:
    
    # trip count
    q1 = quantile(rides[trip_count>0]$trip_count, c(.33))
    q2 = quantile(rides[trip_count>0]$trip_count, c(.66))
    rides[, trip_count_cat := dplyr::case_when(trip_count < q1 ~ "baixo",
                                        trip_count < q2 ~ "médio",
                                        TRUE  ~ "alto")]
    rides[trip_count == 0, trip_count_cat := NA]
    rides[, trip_count_cat := factor(trip_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # people count:
    q1 = quantile(rides[people_count>0]$people_count, c(.33))
    q2 = quantile(rides[people_count>0]$people_count, c(.66))
    rides[, people_count_cat := dplyr::case_when(people_count < q1 ~ "baixo",
                                          people_count < q2 ~ "médio",
                                          TRUE  ~ "alto")]
    rides[people_count == 0, people_count_cat := NA]
    rides[, people_count_cat := factor(people_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # commute trip count
    q1 = quantile(rides[commute_trip_count > 0]$commute_trip_count, c(.33))
    q2 = quantile(rides[commute_trip_count > 0]$commute_trip_count, c(.66))
    rides[, commute_trip_count_cat := dplyr::case_when(commute_trip_count < q1 ~ "baixo",
                                                commute_trip_count < q2 ~ "médio",
                                                TRUE  ~ "alto")]
    rides[commute_trip_count == 0, commute_trip_count_cat := NA]
    rides[, commute_trip_count_cat := factor(commute_trip_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # leisure trip count
    q1 = quantile(rides[leisure_trip_count>0]$leisure_trip_count, c(.33))
    q2 = quantile(rides[leisure_trip_count>0]$leisure_trip_count, c(.66))
    rides[, leisure_trip_count_cat := dplyr::case_when(leisure_trip_count < q1 ~ "baixo",
                                                leisure_trip_count < q2 ~ "médio",
                                                TRUE  ~ "alto")]
    rides[leisure_trip_count == 0, leisure_trip_count_cat := NA]
    rides[, leisure_trip_count_cat := factor(leisure_trip_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # morning trip count
    q1 = quantile(rides[morning_trip_count>0]$morning_trip_count, c(.33))
    q2 = quantile(rides[morning_trip_count>0]$morning_trip_count, c(.66))
    rides[, morning_trip_count_cat := dplyr::case_when(morning_trip_count < q1 ~ "baixo",
                                                morning_trip_count < q2 ~ "médio",
                                                TRUE ~ "alto")]
    rides[morning_trip_count == 0, morning_trip_count_cat := NA]
    rides[, morning_trip_count_cat := factor(morning_trip_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # evening trip count:
    q1 = quantile(rides[evening_trip_count>0]$evening_trip_count, c(.33))
    q2 = quantile(rides[evening_trip_count>0]$evening_trip_count, c(.66))
    rides[, evening_trip_count_cat := dplyr::case_when(evening_trip_count < q1 ~ "baixo",
                                                evening_trip_count < q2 ~ "médio",
                                                TRUE ~ "alto")]
    rides[evening_trip_count == 0, evening_trip_count_cat := NA]
    rides[, evening_trip_count_cat := factor(evening_trip_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # male people count
    q1 = quantile(rides[male_people_count>0]$male_people_count, c(.33))
    q2 = quantile(rides[male_people_count>0]$male_people_count, c(.66))
    rides[, male_people_count_cat := dplyr::case_when(male_people_count < q1 ~ "baixo",
                                               male_people_count < q2 ~ "médio",
                                               TRUE ~ "alto")]
    rides[male_people_count == 0, male_people_count_cat := NA]
    rides[, male_people_count_cat := factor(male_people_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # female people count
    q1 = quantile(rides[female_people_count>0]$female_people_count, c(.33))
    q2 = quantile(rides[female_people_count>0]$female_people_count, c(.66))
    rides[, female_people_count_cat := dplyr::case_when(female_people_count < q1 ~ "baixo",
                                                 female_people_count < q2 ~ "médio",
                                                 TRUE ~ "alto")]
    rides[female_people_count == 0, female_people_count_cat := NA]
    rides[, female_people_count_cat := factor(female_people_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # age 13-19 
    q1 = quantile(rides[age_13_19_people_count>0]$age_13_19_people_count, c(.33))
    q2 = quantile(rides[age_13_19_people_count>0]$age_13_19_people_count, c(.66))
    rides[, age_13_19_people_count_cat := dplyr::case_when(age_13_19_people_count < q1 ~ "baixo",
                                                    age_13_19_people_count < q2 ~ "médio",
                                                    TRUE ~ "alto")]
    rides[age_13_19_people_count == 0, age_13_19_people_count_cat := NA]
    rides[, age_13_19_people_count_cat := factor(age_13_19_people_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # age 20-34
    q1 = quantile(rides[age_20_34_people_count>0]$age_20_34_people_count, c(.33))
    q2 = quantile(rides[age_20_34_people_count>0]$age_20_34_people_count, c(.66))
    rides[, age_20_34_people_count_cat := dplyr::case_when(age_20_34_people_count < q1 ~ "baixo",
                                                    age_20_34_people_count < q2 ~ "médio",
                                                    TRUE ~ "alto")]
    rides[age_20_34_people_count == 0, age_20_34_people_count_cat := NA]
    rides[, age_20_34_people_count_cat := factor(age_20_34_people_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # age 35-54
    q1 = quantile(rides[age_35_54_people_count>0]$age_35_54_people_count, c(.33))
    q2 = quantile(rides[age_35_54_people_count>0]$age_35_54_people_count, c(.66))
    rides[, age_35_54_people_count_cat := dplyr::case_when(age_35_54_people_count < q1 ~ "baixo",
                                                    age_35_54_people_count < q2 ~ "médio",
                                                    TRUE ~ "alto")]
    rides[age_35_54_people_count == 0, age_35_54_people_count_cat := NA]
    rides[, age_35_54_people_count_cat := factor(age_35_54_people_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # age 55-64
    q1 = quantile(rides[age_55_64_people_count>0]$age_55_64_people_count, c(.33))
    q2 = quantile(rides[age_55_64_people_count>0]$age_55_64_people_count, c(.66))
    rides[, age_55_64_people_count_cat := dplyr::case_when(age_55_64_people_count < q1 ~ "baixo",
                                                    age_55_64_people_count < q2 ~ "médio",
                                                    TRUE  ~ "alto")]
    rides[age_55_64_people_count==0, age_55_64_people_count_cat := NA]
    rides[, age_55_64_people_count_cat := factor(age_55_64_people_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # age 65+
    q1 = quantile(rides[age_65_plus_people_count>0]$age_65_plus_people_count, c(.33))
    q2 = quantile(rides[age_65_plus_people_count>0]$age_65_plus_people_count, c(.66))
    rides[, age_65_plus_people_count_cat := dplyr::case_when(age_65_plus_people_count < q1 ~ "baixo",
                                                      age_65_plus_people_count < q2 ~ "médio",
                                                      TRUE ~ "alto")]
    rides[age_65_plus_people_count==0, age_65_plus_people_count_cat := NA]
    rides[, age_65_plus_people_count_cat := factor(age_65_plus_people_count_cat, levels = c("baixo", "médio", "alto"))]
    rides
    
    # convert to DT:
    setDT(ruas)
    
    # 2. merge ruas (shapefile) to associate edge-osm ID its geometry
    rides = merge(
        rides, 
        ruas, 
        by.x = c("edge_uid", "osm_reference_id"), 
        by.y = c("edgeUID", "osmId")
    )
    
    # 3. merge recife_lines (OpenStreetMaps) with rides by osm_id 
    #    to associate the street name:
    recife_lines = readRDS("data/recife_lines.rds")
    rides = merge(
        rides,
        recife_lines[, .(osm_id, name)],
        by.x = "osm_reference_id",
        by.y = "osm_id",
        all.x = T
    )
    setcolorder(rides, c("name", "osm_reference_id", "edge_uid"))
    rides
}
