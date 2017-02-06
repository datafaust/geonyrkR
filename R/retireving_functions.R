
#' Extracting all Data from geoclient V1  with full partitioned address
#'
#' This function returns all raw data from geo client for a given address
#' @param housenum the house number.
#' @param street the street number.
#' @param borough borough.
#' @param app_id app id.
#' @param key key.
#' @keywords geony
#' @export
#' @examples
#' geony_raw("5123", "108st", "queens", "f2e57cb4", "68779dfdbac8b55ecbe179e6d64dd734")

#extract raw data
geony_raw = function(housenum, street, borough, app_id, key, simplify = T) {

  place = paste0("https://api.cityofnewyork.us/geoclient/v1/address.json?houseNumber=",housenum,"&street=",street,"&borough=",borough,"&app_id=",app_id,"&app_key=",key,"")

  place = gsub("[[:space:]]", "", place)

  target = RCurl::getURL(place)

  target = jsonlite::fromJSON(target)

  return(target)

}


#' Extracting longlat for a given address from geoclient V1 with full partitioned address
#'
#' This function returns longlat from geo client for a given address
#' @param housenum the house number.
#' @param street the street number.
#' @param borough borough.
#' @param app_id app id.
#' @param key key.
#' @keywords geony
#' @export
#' @examples
#' geony_latlong("5123", "108st", "queens", "f2e57cb4", "68779dfdbac8b55ecbe179e6d64dd734")

#extract only longitude and latitude
geony_latlong = function(housenum, street, borough, app_id, key, simplify = T) {

  place = paste0("https://api.cityofnewyork.us/geoclient/v1/address.json?houseNumber=",housenum,"&street=",street,"&borough=",borough,"&app_id=",app_id,"&app_key=",key,"")

  place = gsub("[[:space:]]", "", place)

  target = RCurl::getURL(place)

  target = jsonlite::fromJSON(target)


  final = c(q$address$longitude,
  q$address$latitude)

  return(final)

}



#' Extract longlat for a given address from geoclient v1 with a single string address
#'
#' @param address a single string address input, spaces are fine.
#' @param app_id app id.
#' @param key key.
#' @keywords geony
#' @export
#' @examples
#' geony_address("314west100stmanhattan", "f2e57cb4", "68779dfdbac8b55ecbe179e6d64dd734")

geony_address = function(address, app_id, key, simplify = T) {

  place = paste0("https://api.cityofnewyork.us/geoclient/v1/search.json?input=",address,"&app_id=",app_id,"&app_key=",key,"")

  place = gsub("[[:space:]]", "", place)

  target = RCurl::getURL(place)

  target = jsonlite::fromJSON(target)

  final = c(target$results$response$longitudeInternalLabel,target$results$response$latitudeInternalLabel)

  return(final)

}


#' Extract longlat for a given cross street
#'
#' @param cross_street_one a single string cross street input, spaces are fine.
#' @param cross_street_two a single string cross street input, spaces are fine.
#' @param borough a borough input.
#' @param app_id app id.
#' @param key key.
#' @keywords geony
#' @export
#' @examples
#' geony_intersect("broad st","beaver st","manhattan" ,"f2e57cb4", "68779dfdbac8b55ecbe179e6d64dd734")

geony_intersect = function(cross_street_one,cross_street_two,borough,app_id, key, simplify = T) {
  
  place = paste0("https://api.cityofnewyork.us/geoclient/v1/intersection.json?crossStreetOne=",cross_street_one,"&crossStreetTwo=",cross_street_two,"&borough=",borough,"&app_id=",app_id,"&app_key=",key,"")
  
  place = gsub("[[:space:]]", "", place)
  
  target = RCurl::getURL(place)
  
  target = jsonlite::fromJSON(target)
  
  return(target)
  
}



# FCC's Census Block Conversions API
#'
#' This function returns full fips code for a given long lat from the FCC API
#' @param housenum longitude.
#' @param street latitude.
#' @keywords geony
#' @export
#' @examples
#' geony_fips(40.75, -73.9)

geony_fips = function(latitude, longitude) {
  url = "https://data.fcc.gov/api/block/find?format=json&latitude=%f&longitude=%f"
  url = sprintf(url, latitude, longitude)
  json = RCurl::getURL(url, .opts = list(ssl.verifypeer = FALSE))
  json = jsonlite::fromJSON(json)
  #as.character(json$County['FIPS'])
  y = as.character(json$Block['FIPS'])
  return(y)
}







