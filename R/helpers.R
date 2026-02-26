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


# function to combine multiple Capr concept sets into one, with the same name
combineCaprCs <- function(csList,
                          csName) {
  csDf <- data.frame(conceptId = integer(),
                     conceptName = character(),
                     domainId = character(),
                     vocabularyId = character(),
                     standardConcept = character(),
                     includeDescendants = logical(),
                     isExcluded = logical(),
                     includeMapped = logical())
  for (i in csList) {
    cs <- Capr:::as.data.frame(i)
    csDf <- rbind(csDf, cs)
  }

  concepts <- c()
  desc <- c()
  excl <- c()
  excl_desc <- c()
  for (j in 1:nrow(csDf)) {
    if (csDf$includeDescendants[j] && !csDf$isExcluded[j]) {
      desc <- c(desc, csDf$conceptId[j])
    }
    if (csDf$isExcluded[j] && !csDf$includeDescendants[j]) {
      excl <- c(excl, csDf$conceptId[j])
    }
    if (csDf$isExcluded[j] && csDf$includeDescendants[j]) {
      excl_desc <- c(excl_desc, csDf$conceptId[j])
    }
    if (!csDf$isExcluded[j] && !csDf$includeDescendants[j]) {
      concepts <- c(concepts, csDf$conceptId[j])
    }
  }

  caprCs <- Capr::cs(
    unique(concepts),
    Capr::descendants(unique(desc)),
    Capr::exclude(unique(excl)),
    Capr::exclude(Capr::descendants(unique(excl_desc))),
    name = csName
  )
  return(caprCs)
}

# prepare druc concept sets for util query
prepDrugConceptSets <- function(drugConceptSets) {

  # prep cs in subGroups
  csList <- list()
  uniSubCat <- unique(drugConceptSets$subCategory)[!is.na(unique(drugConceptSets$subCategory))]
  for (subCat in uniSubCat) {
    subcatCs <- drugConceptSets |>
      dplyr::filter(subCategory == subCat)

    #import concepts as Capr
    covCs <- vector('list', length = nrow(subcatCs))
    for (i in seq_along(covCs)) {
      covCs[[i]] <- Capr::readConceptSet(
        path = subcatCs$path[i],
        name = subcatCs$label[i]
      )
    }

    comboCs <- combineCaprCs(covCs, csName = paste0("*", subCat))
    covCs <- append(covCs, comboCs)
    csList <- append(csList, covCs)
  }

  # prep those not in subgroups
  otherCs <- drugConceptSets |>
    dplyr::filter(is.na(subCategory))
  #import concepts as Capr
  covCs2 <- vector('list', length = nrow(otherCs))
  for (i in seq_along(covCs2)) {
    covCs2[[i]] <- Capr::readConceptSet(
      path = otherCs$path[i],
      name = otherCs$label[i]
    )
  }

  csList <- append(csList, covCs2)
  return(csList)

}