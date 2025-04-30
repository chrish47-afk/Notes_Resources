# Joining Table for NID, Citation, Location, and Year metadata!
mydb <- dbConnect(MySQL(), user='dbview', password='E3QNSLvQTRJm', dbname='ghdx', host='ghdx-db-pi01.ihme.washington.edu')
#query_taxonomy <- dbGetQuery(mydb, "SELECT * FROM taxonomy_term_data")
#query_field_data <- dbGetQuery(mydb, 'SELECT * FROM field_data_field_location_id')
#query_node <- dbGetQuery(mydb, "SELECT * FROM node;")
#query_years <- dbGetQuery(mydb, "SELECT * FROM field_data_field_time;")
#query_geography <- dbGetQuery(mydb, "SELECT * FROM field_data_field_geography;")

query_join <- dbGetQuery(mydb, '
                         SELECT 
                         nid, 
                         title, 
                         tid as ghdx_location_id,
                         field_location_id_value gbd_location_id,
                         name as gbd_location_name, 
                         YEAR(field_time_value) as year_start, 
                         YEAR(field_time_value2) as year_end
                         FROM node as a
                         JOIN field_data_field_geography as b on b.entity_id = a.nid
                         JOIN field_data_field_time as c on c.entity_id = a.nid
                         JOIN taxonomy_term_data as d on d.tid = b.field_geography_tid
                         JOIN field_data_field_location_id as e on e.entity_Id = d.tid
                         WHERE d.vid = 5
                         ORDER BY year_start, year_end
                         ')

# query_gbd_locs <- dbGetQuery(mydb, 'SELECT t.tid, t.name, l.field_location_id_value
# FROM taxonomy_term_data AS t
# JOIN field_data_field_location_id AS l ON l.entity_id = t.tid
# WHERE t.vid = 5
# ORDER BY t.tid')

# NID test list
nid_metadata <- subset(query_join, nid %in% dt_original_test$nid)
# Issues?
# 1. Location metdatabase differs significantly from the metadatase in the crosswalk_version data bin. Sub-national vs. National differences.
# • The GHDx doesn't provide location_id or any other location hierarchy, so we only have the location name by NID to create subsets.
# 2. Same applies to year metadata. Year_start and year_end for the most part, at least from comparing/constracting the two data bins are almost identical. But again, they're not the same. They differ to a degree.
# 3. In discussion with Brent Bell, source counts generated using GHDx metadata is very improbable for an accurate count. Research databases, or in our case crosswalk_version data bins will provide more accurate results. As well as more detailed metadata.

# Table Names -- SEARCH
tables <- dbGetQuery(mydb, 'SELECT * FROM INFORMATION_SCHEMA.TABLES')

tables_entity_id <- dbGetQuery(mydb, 'SELECT 
  TABLE_SCHEMA, 
  TABLE_NAME 
FROM 
  INFORMATION_SCHEMA.COLUMNS 
WHERE 
  COLUMN_NAME = "field_geography_tid";')



# Entity_id and NID seems to be the JOIN key to 