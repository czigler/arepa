#' An R Package for EPA data retrieving and processing
#'
#' arepa allows for downloading and processing EPA AQS data.
#'
#' @docType package
#' @name arepa
NULL

#' Retrieve zip codes
#' 		
#' @return A data table with columns \code{city}, \code{state}, \code{latitude}
#' and \code{longitude}.
#' 
#' @examples
#' get_zip_codes()
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

#' Download annual AQS datasets
#'
#' @param \code{year} A year or list of years (from 1990 to 2018)
#' 
#' @return This function is used for its side-effects: it downloads annual AQS datasets
#' from \url{http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html}
#' in local folder 'Data_AQS'.
#' 
#' @examples
#' get_AQS_data_annual(2000:2002)
get_AQS_data_annual <- function(year = 1990:2013) {
  code <- "annual_conc_by_monitor_"
  name <- "all"
  dirdata <- file.path("Data_AQS", name)
  dir.create(dirdata, showWarnings = FALSE)
  files <- paste("https://aqs.epa.gov/aqsweb/airdata/", code,
                 year, ".zip", sep = "")
  for (i in 1:length(files)) {
    #----- Loop stage
    print(year[i])
    url <- files[i]
    file <- basename(url)
    download.file(url, file)
    unzip(file, exdir = dirdata)
  }
  print("Purge downloaded zip files")
  zipfiles <- dir(path = ".",  pattern = "\\.zip$")
  file.remove(zipfiles)
}

#' Load annual AQS datasets
#'
#' @param \code{year} A year or list of years (from 1990 to 2018)
#' 
#' @return This function returns a data table of annual AQS datasets previously
#' downloaded in local folder 'Data_AQS' with function \code{\link{get_AQS_data_annual}}. A unique
#' monitor key \code{Monitor} is created.
#' 
#' @details 
#' As part of the process in generating unique site codes, county and
#' site numbers are coerced to be strings to provide standard formatting.
#' As of the most recent package update, state codes are read in as strings
#' precluding the need to convert the file. However, in the future, the 
#' EPA may change the data format again, which may cause some errors in site
#' code formatting. 
#' 
#' The source of this data formatting is the inclustion of Canadian locations,
#' which have the state code "CC"
#' 
#' See this link for more information: 
#' https://aqs.epa.gov/aqsweb/helpfiles/state_code.htm
#' @examples
#' get_AQS_data_annual(2000:2002)
#' AQS <- load_annual_average(2000:2002)
load_annual_average <- function(year) {
  code <- "annual_conc_by_monitor_"
  name <- "all"
  LO <- list()
  for (i in seq(length(year))) {
    #----- Loop stage
    print(year[i])
    LO[[i]] <- fread(file.path("Data_AQS", name, paste0(code, year[i], ".csv")))
    setnames(LO[[i]], make.names(colnames(LO[[i]])))
  }
  DO <- rbindlist(LO)
  is.data.table(DO)
  ##----- Create unique monitor key 'Monitor'
  DO[, Monitor := paste(State.Code,
                        sprintf("%03d", as.numeric(County.Code)),
                        "-",
                        sprintf("%04d", as.numeric(Site.Num)), sep = "")]
  return(DO)
}

#' Download daily AQS datasets
#'
#' @param \code{parameter} Parameter code (e.g, 88101 for PM 2.5 FRM)
#' @param \code{year} A year or list of years (from 1990 to 2018)
#' 
#' @return This function is used for its side-effects: it downloads annual AQS datasets
#' from \url{http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html}
#' in local folder 'Data_AQS'.
#' 
#' @examples
#' get_AQS_data_daily(2000:2002)
get_AQS_data_daily <- function(parameter = 88101, year = 2002:2004) {
  code <- paste0("daily_", parameter, "_")
  name <- paste0("daily_", parameter)
  dirdata <- file.path("Data_AQS", name)
  dir.create(dirdata, showWarnings = FALSE)
  files <- paste("https://aqs.epa.gov/aqsweb/airdata/", code,
                 year, ".zip", sep = "")
  for (i in 1:length(files)) {
    #----- Loop stage
    print(year[i])
    url <- files[i]
    file <- basename(url)
    download.file(url, file)
    unzip(file, exdir = dirdata)
  }
  print("Purge downloaded zip files")
  zipfiles <- dir(path = ".",  pattern = "\\.zip$")
  file.remove(zipfiles)
}

#' Load daily AQS datasets
#' 
#' @param \code{parameter} Parameter code (e.g, 88101 for PM 2.5 FRM)
#' @param \code{year} A year or list of years (from 1990 to 2018)
#' 
#' @details 
#' As part of the process in generating unique site codes, county and
#' site numbers are coerced to be strings to provide standard formatting.
#' As of the most recent package update, state codes are read in as strings
#' precluding the need to convert the file. However, in the future, the 
#' EPA may change the data format again, which may cause some errors in site
#' code formatting. 
#' 
#' The source of this data formatting is the inclustion of Canadian locations,
#' which have the state code "CC"
#' 
#' See this link for more information: 
#' https://aqs.epa.gov/aqsweb/helpfiles/state_code.htm
#' 
#' @return This function returns a data table of annual AQS datasets previously
#' downloaded in local folder 'Data_AQS' with function \code{\link{get_AQS_data_annual}}. A unique
#' monitor key \code{Monitor} is created.
#' 
#' @examples
#' get_AQS_data_annual(2000:2002)
#' AQS <- load_daily_data(2000:2002)
load_daily_data <- function(parameter = 88101, year = 2002:2004) {
  code <- paste0("daily_", parameter, "_")
  name <- paste0("daily_", parameter)
  LO <- list()
  for (i in seq(year)) {
    #----- Loop stage
    print(year[i])
    LO[[i]] <- fread(file.path("Data_AQS", name, paste0(code, year[i], ".csv")))
    setnames(LO[[i]], make.names(colnames(LO[[i]])))
  }
  DO <- rbindlist(LO)
  is.data.table(DO)
  ##----- Create unique monitor key 'Monitor'
  DO[, Monitor := paste(State.Code,
                        sprintf("%03d", as.numeric(County.Code)),
                        "-",
                        sprintf("%04d", as.numeric(Site.Num)), sep = "")]
  return(DO)
}

#' Spatial index distance matrix
#'
#' @param \code{data1} Dataset 1
#' @param \code{lat1} Latitude variable in dataset 1
#' @param \code{lon1} Longitude variable in dataset 1
#' @param \code{id1} Unique key in dataset 1
#' @param \code{data2} Dataset 1
#' @param \code{lat2} Latitude variable in dataset 2
#' @param \code{lon2} Longitude variable in dataset 2
#' @param \code{id2} Unique key in dataset 2
#' @param \code{within} Link \code{within} kilometers
#' @param \code{closest} Closest within Boolean (unique key is id2, so typically zip code)
#' 
#' @return This function returns a data table of pairwise distances \code{within}
#' kilometers with column names \code{id1}, \code{id2} and \code{Distance}.
#' 
#' @examples
#' years <- 2000:2002 # possible range 1990-2018
#' within_km <- 9.656 # 6 miles
#' parameter_code <- 81102 # for PM 10
#' ##----- Get PM10 annual data and set unique Monitor key
#' # Download AQS data in Data_AQS/all.
#' # get_AQS_data_annual(years)
#' PM <- load_annual_average(years)
#' PM <- subset(PM, Parameter.Code == parameter_code)
#' ##----- Zip codes
#' ZIP <- get_zip_codes()
#' ##----- Spatial index to join monitors and Zip codes.  May take a couple of minutes
#' Link_PM_Zip_Index <- spatial_link_index(PM, "Latitude", "Longitude", "Monitor",
#'                                         ZIP, "Latitude.zip", "Longitude.zip", "zip",
#'                                         within = within_km) 
spatial_link_index <- function(data1, lat1, lon1, id1, data2, lat2, lon2, id2,
                               within = NULL, closest = FALSE) {
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
  if (!is.null(within))
    melted_pairwise_distances <- subset(melted_pairwise_distances, Distance < within)
  if (closest) {
    melted_pairwise_distances[, Include.in.Average := 0]
    setkeyv(melted_pairwise_distances, c("Distance"))
    setkeyv(melted_pairwise_distances, sId2)
    melted_pairwise_distances[melted_pairwise_distances[, .I[1], by = key(melted_pairwise_distances)]$V1, 
                              Include.in.Average := 1]
    melted_pairwise_distances <- subset(melted_pairwise_distances, Include.in.Average == 1)
    melted_pairwise_distances[, Include.in.Average := NULL]
  }
  return(melted_pairwise_distances)
}

#' Create a subset of AQS monitors
#'
#' @param \code{MONITORS} An AQS data table obtained created with \code{\link{load_annual_average}}
#' @param \code{parameter_code} A parameter code
#' @param \code{observation_percent} The required valid observation percent, e.g., 67
#' @param \code{monitor_info_file} The path of the file containing extra information
#'        about monitors: "monitor_list.csv" 
#' 
#' @return This function returns a data table of annual AQS datasets previously
#' downloaded in local folder 'Data_AQS' with function \code{\link{get_AQS_data_annual}}.
#' 
#' @examples
#' get_AQS_data_annual(2000:2002)
#' list_AQS <- lapply(2000:2002, load_annual_average)
#' AQS <- rbindlist(list_AQS)
#' sub_AQS <- subset_monitors(AQS, 81102, 67, "~/Google Drive/ARP/Arepa/monitor_list.csv")
subset_monitors <- function(MONITORS,
                            parameter_code,
                            observation_percent,
                            monitor_info_file = "monitor_list.csv") {
  M <- copy(MONITORS)
  ##----- Pick the 24 HOUR measurement
  # Note: There is always a 24 HOUR measurement, so we are not excluding monitors
  M <- subset(M, Sample.Duration != "1 HOUR")
  ##----- Get extra monitor information
  MONITOR_INFO <- fread(monitor_info_file)[, V1 := NULL]
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
  M <- M[Observation.Percent >= observation_percent & Parameter.Code == parameter_code]
  ##----- If several POCs, pick first
  # Sort by Monitor (key1) and then POC (key2)
  setkeyv(M, c("Monitor", "POC", "Year"))
  # Take first POC
  M <- unique(M, by = c("Monitor", "Year"))
  return(M)
}
