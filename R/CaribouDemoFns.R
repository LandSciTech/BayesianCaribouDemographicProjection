

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









