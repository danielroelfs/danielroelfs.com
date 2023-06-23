### SAVE SPLMAPS DATA TO FILE ########################


#-- Load data ------------------------

nor_lau2_map_b2019_default_dt <- splmaps::nor_lau2_map_b2019_default_dt
nor_lau2_map_b2019_default_sf <- splmaps::nor_lau2_map_b2019_default_sf
nor_lau2_map_b2019_insert_oslo_dt <- splmaps::nor_lau2_map_b2019_insert_oslo_dt
nor_lau2_map_b2020_default_dt <- splmaps::nor_lau2_map_b2020_default_dt
nor_lau2_map_b2020_default_sf <- splmaps::nor_lau2_map_b2020_default_sf
nor_lau2_map_b2020_insert_oslo_dt <- splmaps::nor_lau2_map_b2020_insert_oslo_dt
nor_lau2_map_b2020_split_dt <- splmaps::nor_lau2_map_b2020_split_dt
nor_lau2_position_geolabels_b2019_default_dt <- splmaps::nor_lau2_position_geolabels_b2019_default_dt
nor_lau2_position_geolabels_b2019_insert_oslo_dt <- splmaps::nor_lau2_position_geolabels_b2019_insert_oslo_dt
nor_lau2_position_geolabels_b2020_default_dt <- splmaps::nor_lau2_position_geolabels_b2020_default_dt
nor_lau2_position_geolabels_b2020_insert_oslo_dt <- splmaps::nor_lau2_position_geolabels_b2020_insert_oslo_dt
nor_nuts3_map_b2017_default_dt <- splmaps::nor_nuts3_map_b2017_default_dt
nor_nuts3_map_b2017_default_sf <- splmaps::nor_nuts3_map_b2017_default_sf
nor_nuts3_map_b2017_insert_oslo_dt <- splmaps::nor_nuts3_map_b2017_insert_oslo_dt
nor_nuts3_map_b2019_default_dt <- splmaps::nor_nuts3_map_b2019_default_dt
nor_nuts3_map_b2019_default_sf <- splmaps::nor_nuts3_map_b2019_default_sf
nor_nuts3_map_b2019_insert_oslo_dt <- splmaps::nor_nuts3_map_b2019_insert_oslo_dt
nor_nuts3_map_b2020_default_dt <- splmaps::nor_nuts3_map_b2020_default_dt
nor_nuts3_map_b2020_default_sf <- splmaps::nor_nuts3_map_b2020_default_sf
nor_nuts3_map_b2020_insert_oslo_dt <- splmaps::nor_nuts3_map_b2020_insert_oslo_dt
nor_nuts3_map_b2020_split_dt <- splmaps::nor_nuts3_map_b2020_split_dt
nor_nuts3_position_geolabels_b2017_default_dt <- splmaps::nor_nuts3_position_geolabels_b2017_default_dt
nor_nuts3_position_geolabels_b2017_insert_oslo_dt <- splmaps::nor_nuts3_position_geolabels_b2017_insert_oslo_dt
nor_nuts3_position_geolabels_b2019_default_dt <- splmaps::nor_nuts3_position_geolabels_b2019_default_dt
nor_nuts3_position_geolabels_b2019_insert_oslo_dt <- splmaps::nor_nuts3_position_geolabels_b2019_insert_oslo_dt
nor_nuts3_position_geolabels_b2020_default_dt <- splmaps::nor_nuts3_position_geolabels_b2020_default_dt
nor_nuts3_position_geolabels_b2020_insert_oslo_dt <- splmaps::nor_nuts3_position_geolabels_b2020_insert_oslo_dt
nor_xxx_position_title_insert_oslo_b2017_insert_oslo_dt <- splmaps::nor_xxx_position_title_insert_oslo_b2017_insert_oslo_dt
nor_xxx_position_title_insert_oslo_b2019_insert_oslo_dt <- splmaps::nor_xxx_position_title_insert_oslo_b2019_insert_oslo_dt
nor_xxx_position_title_insert_oslo_b2020_insert_oslo_dt <- splmaps::nor_xxx_position_title_insert_oslo_b2020_insert_oslo_dt
oslo_ward_map_b2020_default_dt <- splmaps::oslo_ward_map_b2020_default_dt
oslo_ward_map_b2020_default_sf <- splmaps::oslo_ward_map_b2020_default_sf
oslo_ward_position_geolabels_b2020_default_dt <- splmaps::oslo_ward_position_geolabels_b2020_default_dt

nor_locations_redistricting_2020 <- spldata::nor_locations_redistricting(border = 2020)
nor_locations_names_2020 <- spldata::nor_locations_names(border = 2020)
nor_population_by_age_cats_2020 <- spldata::nor_population_by_age_cats(cats = list("0_18" = 0:18), border = 2020)

#-- Save data ------------------------

save.image(file = "splmaps_data.RData")
