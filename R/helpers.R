getDenomText <- function(denomType) {
  checkmate::assert_choice(x = denomType, choices = c("pd1", "pd2", "pd3", "pd4"))
  if (denomType == "pd1") {
    txt <- "- Day 1 population (PD1): the number of persons in the population who were observed on the first day of the period of interest"
  }
  if (denomType == "pd2") {
    txt <- "- Complete-period population (PD2): the number of persons in the population who contribute all observable person-days in the period of interest."
  }
  if (denomType == "pd3") {
    txt <- "- Any-time population (PD3): the number of persons who who contributes at least 1 day in the period of interest."
  }
  if (denomType == "pd4") {
    txt <- "- Sufficient-time population (PD4): the number of persons who contributes sufficient time in the period of interest based on at least n observable person-days in the period of interest"
  }
  return(txt)
}


getNumText <- function(numType) {
  checkmate::assert_choice(x = numType, choices = c("pn1", "pn2"))
  if (numType == "pn1") {
    txt <- "- Cases before POI (PN1): the number of patients who have been observed to have the condition of interest prior to the period of interest, within the lookback time"
  }
  if (numType == "pn2") {
    txt <- "- Cases before and during POI (PN2): the number of patients who have been observed to have the condition of interest either within the lookback time or during the period of interest."
  }
  return(txt)
}



