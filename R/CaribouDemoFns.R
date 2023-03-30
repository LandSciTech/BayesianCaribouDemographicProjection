# TODO: These are not used any where else. Remove?

getParamsFromEacker <- function(path) {
  ###############
  # Use Eacker example data for sample sizes in each year.
  survData <- paste0(path, "/tte_caribouFEMALES.csv")
  ageRatio.herd <- paste0(path, "/ageRatio.herd.csv")
  ageRatio.herd2 <- read.csv(ageRatio.herd, header = T)
  tte_caribou2 <- read.csv(survData, header = T)

  # need table of observed number of cows each year as input for simulating
  # calf:cow ratios. Use Eaker as example.
  cowCounts <- subset(ageRatio.herd2, Class == "cow")
  write.csv(cowCounts, "tabs/cowCounts.csv")
  yrRange <- max(tte_caribou2$Year) - min(tte_caribou2$Year)

  # get survival sampling parameters from example data
  animalStarts <- subset(tte_caribou2, select = c(id, Year)) %>%
    group_by(id) %>%
    summarise(startYear = min(Year))
  freqStartsByYear <- as.data.frame(table(animalStarts$startYear))
  names(freqStartsByYear) <- c("Year", "numStarts")
  freqStartsByYear$Year <- as.numeric(as.character(freqStartsByYear$Year))
  sm <- tte_caribou2 %>%
    group_by(id) %>%
    summarize(startYear = min(Year), endYear = max(Year),
              numYears = max(Year) - min(Year), died = sum(event))
  # for simplicity, pick a single number of years that collars remain on
  collarNumYears <- median(subset(sm, !died)$numYears)

  addInfo <- unique(subset(tte_caribou2,
                           select = c(HerdDescription, HerdCode, Range_ID,
                                      RangeDescription, RangeCode)))

  freqStartsByYear <- merge(freqStartsByYear, addInfo)
  write.csv(freqStartsByYear, "tabs/freqStartsByYear.csv")

  offSet <- subset(sm, !died, select = c(id, endYear))
  names(offSet) <- c("id", "Year")
  offSet <- merge(offSet, tte_caribou2, all.x = T)
  # for simplicity, pick a single month that collars fall off
  collarOffTime <- median(offSet$exit)

  onSet <- subset(sm, select = c(id, startYear))
  names(onSet) <- c("id", "Year")
  onSet <- merge(onSet, tte_caribou2, all.x = T)
  # for simplicity, pick a single month that collars are put on
  collarOnTime <- median(onSet$enter)
  # TO DO: allow users to provide a file formatted as in Eacker from
  # which these parameters can be derived.

  # freqStartsByYear$numStarts=30
  return(list(cowCounts = cowCounts, freqStartsByYear = freqStartsByYear,
              collarOnTime = collarOnTime, collarOffTime = collarOffTime,
              collarNumYears = collarNumYears))
}

# TODO: used in paper only. move there?
makeInterceptPlots <- function(scResults, addBit = "", facetVars = c("P", "sQ"),
                               loopVars = NULL,
                               whichPlots = c("Adult female survival",
                                              "Population growth rate",
                                              "Recruitment",
                                              "Female population size"),
                               survLow = 0.6, type = "png", useNational = T) {
  # facetVars=c("lre","sre");loopVars="srv";scResults=scResultsHigh

  if (!is.null(loopVars)) {
    loopSet <- unique(subset(scResults$rr.summary.all, select = loopVars))
    loopSet$dummy <- 1
  } else {
    loopSet <- data.frame(dummy = 1)
  }


  for (l in 1:nrow(loopSet)) {
    # l = 1
    crow <- loopSet[l, ]

    aa <- ""
    for (n in names(crow)) {
      if (n == "dummy") {
        next
      }
      aa <- paste0(aa, n, crow[[n]])
    }

    addBitO <- paste0(addBit, aa)

    if (useNational) {
      simRange <- merge(scResults$sim.all, crow)
    } else {
      simRange <- NULL
    }

    if (is.element("Adult female survival", whichPlots)) {
      if (type == "png") {
        png(here::here(paste0("figs/Surv", addBitO, ".png")),
            height = 6, width = 7.48, units = "in", res = 600
        )
      } else {
        pdf(paste0("figs/Surv", addBitO, ".pdf"), width = 10, height = 7)
      }
      print(plotRes(merge(scResults$rr.summary.all, crow), "Adult female survival",
                    obs = merge(scResults$obs.all, crow),
                    lowBound = survLow, simRange = simRange, facetVars = facetVars
      ))
      dev.off()
    }

    if (is.element("Population growth rate", whichPlots)) {
      if (type == "png") {
        png(here::here(paste0("figs/Lambda", addBitO, ".png")),
            height = 6, width = 7.48, units = "in", res = 600
        )
      } else {
        pdf(paste0("figs/Lambda", addBitO, ".pdf"), width = 10, height = 7)
      }
      print(plotRes(merge(scResults$rr.summary.all, crow), "Population growth rate",
                    obs = merge(scResults$obs.all, crow),
                    lowBound = 0, simRange = simRange, facetVars = facetVars
      ))
      dev.off()
    }

    if (is.element("Recruitment", whichPlots)) {
      if (type == "png") {
        png(here::here(paste0("figs/Rec", addBitO, ".png")),
            height = 6, width = 7.48, units = "in", res = 600
        )
      } else {
        pdf(paste0("figs/Rec", addBitO, ".pdf"), width = 10, height = 7)
      }
      print(plotRes(merge(scResults$rr.summary.all, crow), "Recruitment",
                    obs = merge(scResults$obs.all, crow),
                    lowBound = 0, simRange = simRange, facetVars = facetVars
      ))
      dev.off()
    }

    if (is.element("Female population size", whichPlots)) {
      if (type == "png") {
        png(here::here(paste0("figs/FPOP", addBitO, ".png")),
            height = 6, width = 7.48, units = "in", res = 600
        )
      } else {
        pdf(paste0("figs/FPOP", addBitO, ".pdf"), width = 10, height = 7)
      }
      print(plotRes(merge(scResults$rr.summary.all, crow), "Female population size",
                    obs = merge(scResults$obs.all, crow),
                    lowBound = 0, facetVars = facetVars
      ))
      dev.off()
    }
  }
}









