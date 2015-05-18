##----- Retrieve zip codes

get_zip_codes <- function() {
  data(zipcode)
  ZIP <- data.table(na.omit(zipcode))
  setkey(ZIP, zip)
  setnames(ZIP, "city", "City.zip")
  setnames(ZIP, "state", "State.zip")
  setnames(ZIP, "latitude", "Latitude.zip")
  setnames(ZIP, "longitude", "Longitude.zip")
  return(ZIP)
}

##----- Download and create annual AQS database
## http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html

get_AQS_data_annual <- function(year = 1990:2013) {
  code <- "annual_all_"
  name <- "all"
  dirdata <- file.path("Data_AQS", name)
  dir.create(dirdata, showWarnings = FALSE)
  files <- paste("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/", code,
                 year, ".zip", sep = "")
  for (i in 1:length(files)) {
    #----- Loop stage
    print(year[i])
    url <- files[i]
    file <- basename(url)
    download.file(url, file)
    untar(file, compressed = 'gzip', exdir = dirdata)
  }
  print("Purge downloaded zip files")
  zipfiles <- dir(path = ".",  pattern = "\\.zip$")
  file.remove(zipfiles)
}

##----- Load annual data

load_annual_average <- function(year) {
  #----- Loop stage
  print(year)
  code <- "annual_all_"
  name <- "all"
  DO <- fread(file.path("Data_AQS", name, paste0(code, year, ".csv")))
  setnames(DO, make.names(colnames(DO)))
  return(DO)
}

##----- Spatial index distance matrix
## Format: ID1 ID2 Distance
## all within a given range in kilometers

spatial_link_index <- function(data1, lat1, lon1, id1, data2, lat2, lon2, id2, within = NULL) {
  if (!is.data.table(data1))
    data1 <- data.table(data1)
  if (!is.data.table(data2))
    data1 <- data.table(data2)
  data1u <- unique(data1, by = id1)
  data2u <- unique(data2, by = id2)
  spData1 <- data1u
  spData2 <- data2u
  coordinates(spData1) <- c(lon1, lat1)
  coordinates(spData2) <- c(lon2, lat2)
  pairwise_distances <- data.frame(spDists(spData1, spData2, longlat = TRUE))
  names(pairwise_distances) <- data2u[[id2]]
  sId1 <- id1
  sId2 <- id2
  id1 <- data1u[[id1]]
  pairwise_distances_with_index <- data.table(cbind(id1, pairwise_distances))
  melted_pairwise_distances <- melt(pairwise_distances_with_index, id.vars = "id1")
  setnames(melted_pairwise_distances, old = names(melted_pairwise_distances), new = c(sId1, sId2, "Distance"))
  if (is.null(within))
    return(melted_pairwise_distances)
  else
    return(subset(melted_pairwise_distances, Distance < within))
}

subset_monitors <- function(MONITORS, parameter_code, observation_percent) {
  M <- copy(MONITORS)
  ##----- Pick the 24 HOUR measurement
  # Note: There's always a 24 HOUR measurement, so we're not excluding monitors
  M <- subset(M, Sample.Duration != "1 HOUR")
  ##----- Get extra monitor information
  MONITOR_INFO <- fread("monitor_list.csv")[, V1 := NULL]
  MONITOR_NO_MICROSCALE <- subset(MONITOR_INFO, 
                                  MEASUREMENT_SCALE != "MICROSCALE" & 
                                    PARAMETER_CODE == parameter_code)
  MONITOR_NO_MICROSCALE[, Monitor := paste(sprintf("%02d", as.numeric(STATE_CODE)),
                                           sprintf("%03d", as.numeric(COUNTY_CODE)),
                                           "-",
                                           sprintf("%04d", as.numeric(SITE_NUMBER)), sep = "")]
  M <- subset(M, Monitor %in% MONITOR_NO_MICROSCALE$Monitor)
  ##----- Valid day count >= 273
  # Many monitors are lost
  # nrow(unique(PM[Valid.Day.Count >= 273], by = "Monitor"))
  # nrow(unique(PM, by = "Monitor"))
  M <- M[Observation.Percent >= observation_percent]
  ##----- If several POCs, pick first
  # Sort by Monitor (key1) and then POC (key2)
  setkeyv(M, c("Monitor", "POC", "Year"))
  # Take first POC
  M <- unique(M, by = c("Monitor", "Year"))
  return(M)
}
