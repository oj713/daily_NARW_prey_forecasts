#' Approximates solar declination angle, helper
#' https://www.pveducation.org/pvcdrom/properties-of-sunlight/declination-angle
get_soldecang <- function(day_of_year) {
  -23.45 * cos((360 / 365) * (day_of_year + 10) * pi/180)
}

#' Approximates length of day
#' www.researchgate.net/post/Geometrically_Derived_Formula_for_Day_Length_Based_on_Latitude_and_Time_of_Year
get_daylength <- function(day_of_year, latitude) {
  24/pi * acos(-tan(latitude * pi/180) * tan(get_soldecang(day_of_year) * pi/180))
}

#' Approximates derivative of length of day
get_ddx_daylength <- function(day_of_year, latitude) {
  p1 <- get_daylength(day_of_year, latitude)
  p2 <- get_daylength(day_of_year + 1, latitude)
  return(p2-p1)
}

#' Calculates velocity based on horizontal and vertical currents
get_velocity <- function(uo, vo) {
  sqrt(uo^2 + vo^2)
}