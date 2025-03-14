
#' Check that there is at least one tree for every observation entry and flag entries with more than 250 trees
#' @param data Aspen dataset to run through QC functions
missingTreeQC <- function(data = fetchAndWrangleAspen()){

  missingTreeQC <- data$data$Observations %>%
    # Add up all trees in a row
    dplyr::mutate(totalTreeCount = Class1+Class2+Class3+Class4+Class5+Class6) %>%
    # Filter for any rows that have less than one trees or more than 250 trees
    dplyr::filter(totalTreeCount < 1 | totalTreeCount > 250) %>%
    dplyr::select(Park, Site, VisitDate, SpeciesCode, Class1, Class2, Class3, Class4, Class5, Class6, totalTreeCount)

  return(missingTreeQC)
}

#' Check that no entries in the observation table have a SpeciesCode that is NA or missing
#' @param data Aspen dataset to run through QC functions
treeSpeciesQC <- function(data = fetchAndWrangleAspen()){

  treeSpeciesQC <- data$data$Observations %>%
    dplyr::filter(SpeciesCode == "" | is.na(SpeciesCode) | SpeciesCode == "NA") %>%
    dplyr::select(Park, Site, VisitDate, SpeciesCode, Class1, Class2, Class3, Class4, Class5, Class6)

  return(treeSpeciesQC)
}

#' Check that unknown species entries do not have any live trees
#' @param data Aspen dataset to run through QC functions
unknownSpeciesQC <- function(data = fetchAndWrangleAspen()){

  unknownSpeciesQC <- data$data$Observations %>%
    dplyr::filter(SpeciesCode == "UNK") %>%
    dplyr::filter(Class1 != 0 | Class2 != 0 | Class3 != 0 | Class4 != 0 | Class5 != 0) %>%
    dplyr::select(Park, Site, VisitDate, SpeciesCode, Class1, Class2, Class3, Class4, Class5, Class6)

  return(unknownSpeciesQC)
}

#' Check that all plots have at least one live aspen each time they are visited
#' @param data Aspen dataset to run through QC functions
checkAspenQC <- function(data = fetchAndWrangleAspen()){

  checkAspenQC <- data$data$Observations %>%
    # Filter for aspen trees
    dplyr::filter(SpeciesCode == "POTR5") %>%
    dplyr::select(parentglobalid, SpeciesCode, Class1, Class2, Class3, Class4, Class5) %>%
    # Join observations and visits
    dplyr::full_join(data$data$SiteVisit, by = c("parentglobalid" = "globalid")) %>%
    # Filter data for any visits that don't have aspen observations
    dplyr::filter(is.na(SpeciesCode)) %>%
    dplyr::select(Park, Site, VisitType, VisitDate, SpeciesCode)

  return(checkAspenQC)
}

#' Flag large changes in number of tree between years
#' @param data Aspen dataset to run through QC functions
treeChangeQC <- function(data = fetchAndWrangleAspen()){

  visits <- data$data$SiteVisit %>%
    dplyr::group_by(Site) %>%
    dplyr::arrange(Site, VisitDate) %>%
    dplyr::mutate(visitNumber = seq_along(Site)) %>%
    dplyr::select("globalid", "Park", "Site", "VisitType", "VisitDate", "visitNumber")

  treeChangeQC <- data$data$Observations %>%
    # join with site visit table
    #dplyr::left_join(visits, by = c("parentglobalid" = "globalid")) %>%
    dplyr::mutate(totalTrees = Class1+Class2+Class3+Class4+Class5) %>%
    dplyr::group_by(Site, VisitDate) %>%
    dplyr::summarise(totalTrees = sum(totalTrees)) %>%
    dplyr::group_by(Site) %>%
    dplyr::mutate(previousTrees = dplyr::lag(totalTrees),
                  treeChange = previousTrees - totalTrees)

    # find percent change (in total number of trees both conifer and aspen or separate??) between year ((year 1 - year 2)/year 1)
    # flag any with a large change (figure out what counts as a large change)
}

# Flag if there's more than one visit to a plot in a year
