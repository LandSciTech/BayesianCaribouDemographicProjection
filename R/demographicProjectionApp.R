# Copyright 2023 Daniel Eacker & Her Majesty the Queen in Right of Canada as represented by the Minister of the Environment
# License GPL-3
#NOTICE: This function has been modified from code provided in https://doi.org/10.1002/wsb.950

#' Run the Bayesian caribou demographic projection app
#'
#' The app will launch in the browser.
#'
#' @return The app will launch in the browser
#' @export
#'
#' @examples
#' demographicProjectionApp()
#'
#' @import caribouMetrics shiny dplyr shinydashboard
#'
demographicProjectionApp <- function(n = 1000) {
  rjags::load.module("glm")

  ##########
  # Get full set of sims for comparison, the results are cached
  getSimsNational()

  scn_defaults <- eval(formals(getScenarioDefaults))

  # defaults set to be uninformative
  obs_defaults <- list(
    cowMult = 6, collarCount = 30, collarInterval = 1, collarNumYears = 6,
    collarOnTime = 1, collarOffTime = 12
  )

  prior_defaults <- lapply(formals(getPriors), eval)

  # JAGS params
  jags_defaults <- eval(formals(caribouBayesianPM)[c("Niter", "Nchains", "Nthin", "Nburn")])

  # add JavaScript to add an id to the <section> tag
  # so we can overlay waiter on top of it
  add_id_to_section <- "
$( document ).ready(function() {
  var section = document.getElementsByClassName('content');
  section[0].setAttribute('id', 'waiter-content');
});"

  # UI  -------------------------------------
  ui <- dashboardPage(
    # Application title
    dashboardHeader(title = "Boreal Caribou Demographic Projection Informed by Local Monitoring & National Demographic-Disturbance Relationships",
                    titleWidth = 1150),

    # Sidebar --------------------------------------------------------------------
    dashboardSidebar(
      width = 400,
      h2("Define simulation inputs"),
      shinyjs::useShinyjs(),
      sidebarMenu(
        id = "sideBar",
        # disturbance --------------------------------
        menuItem(
          text = "Scenario",
          selectInput("scn_source",
            label = "Create disturbance scenario from...",
            choices = c("a csv file" = "file", "a simulation" = "sim"),
            selected = "sim"
          ),
          numericInput("curYear",
            label = "Last year of observations",
            value = scn_defaults$curYear
          ),
          shinyFiles::shinyFilesButton("scn_file", "Select File", "Please select a file",
            multiple = FALSE
          ),
          div(
            id = "nYears",
            numericInput("nYearObs",
              label = "Number of years of observations",
              value = scn_defaults$obsYears, min = 1
            ),
            numericInput("nYearProj",
              label = "Number of years of projections",
              value = scn_defaults$projYears, min = 0
            )
          ),
          div(
            id = "dist_sim",
            numericInput("iAnthro",
              label = "Initial % anthropogenic disturbance",
              value = scn_defaults$iAnthro, min = 0
            ),
            numericInput("obsAnthroSlope",
              label = "% increase in anthropogenic disturbance per year in observation period",
              value = scn_defaults$obsAnthroSlope, min = 0
            ),
            numericInput("projAnthroSlope",
              label = "% increase in anthropogenic disturbance per year in projection period",
              value = scn_defaults$projAnthroSlope, min = 0
            ),
            numericInput("iFire",
              label = "Initial % natural disturbance",
              value = scn_defaults$iFire, min = 0
            )
          )
        ),
        # True pop ---------------------------
        menuItem(
          "True population parameters",
          numericInput("N0",
            label = "Initial population size",
            value = scn_defaults$N0, min = 0
          ),
          sliderInput(
            inputId = "rQuantile", label = "Recruitment quantile",
            value = scn_defaults$rQuantile, min = 0.025, max = 0.975
          ),
          sliderInput(
            inputId = "sQuantile", label = "Survival quantile",
            value = scn_defaults$sQuantile, min = 0.025, max = 0.975
          ),
          sliderInput(
            inputId = "rSlopeMod", label = "Multiplier for effect of disturbance on recruitment",
            value = scn_defaults$rSlopeMod, min = 0, max = 5
          ),
          sliderInput(
            inputId = "sSlopeMod", label = "Multiplier for effect of disturbance on survival",
            value = scn_defaults$sSlopeMod, min = 0, max = 5
          )
        ),
        # Obs data ---------------------------------
        menuItem(
          "Observation model parameters",
          numericInput("collarCount",
            label = "Target number of collars",
            value = obs_defaults$collarCount, min = 0
          ),
          numericInput("collarInterval",
            label = "Number of years between collar deployments",
            value = obs_defaults$collarInterval, min = 1
          ),
          numericInput("cowMult",
            label = "Number of cows per collared cow in aerial surveys for calf:cow ratio each year",
            value = obs_defaults$cowMult, min = 0
          ),
          numericInput("collarNumYears",
            label = "Number of years until collar falls off",
            value = obs_defaults$collarNumYears, min = 1
          ),
          numericInput("collarOnTime",
            label = "Month that collars are deployed",
            value = obs_defaults$collarOnTime, min = 1, max = 12
          ),
          numericInput("collarOffTime",
            label = "Month that collars fall off",
            value = obs_defaults$collarOffTime, min = 1, max = 12
          ),
          numericInput("assessmentYrs",
                       label = "Number of years over which to assess lambda (growth rate)",
                       value = 3, min = 1, max = 5
          ),
          sliderInput(
            inputId = "qRange", label = "Ratio of bulls to cows in composition survey groups.",
            value = c(scn_defaults$qMin,scn_defaults$qMax), min = 0, max = 1
          ),
        sliderInput(
          inputId = "uRange", label = "Probability of misidentifying adult sex in composition survey.",
          value = c(scn_defaults$uMin,scn_defaults$uMax), min = 0, max = 1
          ),
        sliderInput(
          inputId = "zRange", label = "Probability of missing calves in composition survey.",
          value = c(scn_defaults$zMin,scn_defaults$zMax), min = 0, max = 0.99
          ),
        checkboxInput("redoSimsNational", "Update cached national simulations.", value = 0)
        ),
        # Priors ------------------------------------------------
        menuItem(
          "Model priors",
          selectInput("nat_model",
            label = "National version model to use",
            choices = c("default", "custom"),
            selected = "default"
          ),
          div(
            id = "custModel",
            h4(HTML("To use a different version of the national<br/>model select the model numbers below")),
            selectInput("recruitmentModelNumber",
              label = "Recruitment model number",
              choices = caribouMetrics::popGrowthTableJohnsonECCC %>%
                filter(responseVariable == "recruitment") %>%
                group_by(ModelNumber) %>%
                filter(all(Coefficient %in% c(
                  "Intercept", "Precision",
                  "Anthro", "fire_excl_anthro"
                ))) %>%
                pull(ModelNumber) %>% unique(),
              selected = "M4"
            ),
            selectInput("survivalModelNumber",
              label = "Female survival model number",
              choices = caribouMetrics::popGrowthTableJohnsonECCC %>%
                filter(responseVariable == "femaleSurvival") %>%
                group_by(ModelNumber) %>%
                filter(all(Coefficient %in% c(
                  "Intercept", "Precision",
                  "Anthro", "fire_excl_anthro"
                ))) %>%
                pull(ModelNumber) %>% unique(),
              selected = "M1"
            ),
            h4(HTML("To use a custom table of coefficients<br/>select a csv file")),
            shinyFiles::shinyFilesButton("popGrowth_file", "Select File", "Please select a file",
              multiple = FALSE
            )
          ),
          sliderInput(
            inputId = "sAnthroSlopeSE",
            label = "Standard deviation of the effect of disturbance on survival",
            value = prior_defaults$sAnthroSlopeSE, min = 0, max = 0.002
          ),
          sliderInput(
            inputId = "rAnthroSlopeSE",
            label = "Standard deviation of the effect of disturbance on recruitment",
            value = prior_defaults$rAnthroSlopeSE, min = 0, max = 0.02
          ),
          sliderInput(
            inputId = "sIntSE",
            label = "Standard deviation of the survival intercept",
            value = prior_defaults$sIntSE, min = 0, max = 0.5
          ),
          sliderInput(
            inputId = "rIntSE",
            label = "Standard deviation of the recruitment intercept",
            value = prior_defaults$rIntSE, min = 0, max = 0.5
          ),
          sliderInput(
            inputId = "sNuMin",
            label = "Min of uniform prior for coefficient of variation in survival among years",
            value = prior_defaults$sNuMin, min = 0.01, max = 1
          ),
          sliderInput(
            inputId = "sNuMax",
            label = "Max of uniform prior for coefficient of variation in survival among years",
            value = prior_defaults$sNuMax, min = 0.02, max = 1
          ),
          sliderInput(
            inputId = "rNuMin",
            label = "Min of uniform prior for coefficient of variation in recruitment among years",
            value = prior_defaults$rNuMin, min = 0.01, max = 1
          ),
          sliderInput(
            inputId = "rNuMax",
            label = "Max of uniform prior for coefficient of variation in recruitment among years",
            value = prior_defaults$rNuMax, min = 0.02, max = 1
          )
        ),

        # JAGS params ---------------------------
        menuItem(
          "Baysian model parameters",
          checkboxInput("getKSDists", "Calculate Kolmogorov-Smirnov Distances", value = 0),
          selectInput("survAnalysisMethod",
            label = "Survival analysis method to use",
            choices = c("KaplanMeier", "Exponential"),
            selected = "KaplanMeier"
          ),
          sliderInput(
            inputId = "Nchains", label = "Number of chains",
            value = jags_defaults$Nchains, min = 1, max = 5
          ),
          sliderInput(
            inputId = "Niter", label = "Number of iterations",
            value = jags_defaults$Niter, min = 1, max = 50000, step = 1000
          ),
          sliderInput(
            inputId = "Nburn", label = "Length of burn-in",
            value = jags_defaults$Nburn, min = 1, max = 20000, step = 1000
          ),
          sliderInput(
            inputId = "Nthin", label = "Thinning rate",
            value = jags_defaults$Nthin, min = 1, max = 10
          )
        ),
        # Display options ---------------------------
        menuItem(
          "Display options",
          checkboxInput("showTruePop" , "Show true population values",value=1),

          checkboxInput("showNationalBands" , "Show range of variation from national model",value=1)

        )

        # ------------------------
      ),
      h3("Run model/Update data"),
      actionButton("Run.model", "Run model", icon("paper-plane"),
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      actionButton("mcmc.output", "Open MCMC diagnostic plots"),
      shinyFiles::shinySaveButton("saveOutput",
        title = "Save results to csv or rds",
        label = "Save results to csv or rds",
        filetype = c("csv", "rds")
      )
      # This .rmd file is missing
      # radioButtons('format', 'Document format', c('HTML', 'PDF', 'Word'),
      #              selected="HTML",  inline = TRUE),
      # downloadButton("downloadOutput", label = "Download output"),
    ),
    # Body #---------------------------------------------------------------------
    dashboardBody(
      # import our custom JavaScript
      tags$head(
        tags$script(add_id_to_section)
      ),
      waiter::useWaiter(),
      navbarPage(
        id = "bodyTabs",
        title = "",
        tabPanel(
          "Instructions",

          withMathJax(includeMarkdown(system.file("app/Instructions.Rmd",
                                          package = "BayesianCaribouDemographicProjection"
          )))
        ),
        tabPanel(
          "Graphical summary",
          tabsetPanel(
            id = "graphPanel",
            tabPanel("Disturbance", plotOutput("plot6")),
            tabPanel("Recruitment", plotOutput("plot2")),
            tabPanel("Adjusted recruitment", plotOutput("plot3")),
            tabPanel("Adult female survival", plotOutput("plot1")),
            tabPanel("Population growth rate", plotOutput("plot4")),
            tabPanel("Female population size", plotOutput("plot5")),
            tabPanel("Recruitment KS Distance", plotOutput("plot8")),
            tabPanel("Adult female survival KS Distance", plotOutput("plot7")),
            tabPanel("Population growth rate KS Distance", plotOutput("plot9"))
          ),
        ),
        tabPanel(
          "Tabular summary",
          tabsetPanel(
            id = "tablePanel",
            tabPanel("Summary", dataTableOutput("table")),
            tabPanel("Adult female survival", tableOutput("table2")),
            tabPanel("Recruitment", tableOutput("table3")),
            tabPanel("Adjusted recruitment", tableOutput("table4")),
            tabPanel("Population growth rate", tableOutput("table5")),
            tabPanel("Female population size", tableOutput("table7")),
            tabPanel("JAGS output", dataTableOutput("table6"))
          ),
        )
      )
    )
  )

  # Define server logic
  server <- function(input, output, session) {
    .x <- NULL

    # show appropriate inputs based on file or sim scenario source
    observe({
      switch(input$scn_source,
             sim = {
               shinyjs::hide("scn_file")
               shinyjs::show("nYears")
               shinyjs::show("dist_sim")
             },
             file = {
               shinyjs::show("scn_file")
               shinyjs::hide("dist_sim")
               shinyjs::hide("nYears")
             }
      )

      switch(input$nat_model,
             default = {
               shinyjs::hide("custModel")
             },
             custom = {
               shinyjs::show("custModel")
             }
      )
    })

    volumes <- eventReactive(c(input$scn_file, input$saveOutput, input$popGrowth_file),
                             ignoreInit = TRUE,
                             {
                               message(
                                 "Accessing local file structure.",
                                 "\nIf this takes a long time it is because you connected and then ",
                                 "disconnected from VPN. Either reconnect or restart your computer to ",
                                 "speed up"
                               )

                               # Set up file selection
                               # Note this time out is because when I disconnected from VPN it
                               # made the getVolumes function hang forever because it was looking for
                               # drives that were no longer connected. Now it will give an error
                               timeout <- R.utils::withTimeout(
                                 {
                                   volumes <- c(
                                     wd = getwd(),
                                     shinyFiles::getVolumes()()
                                   )
                                 },
                                 timeout = 200,
                                 onTimeout = "silent"
                               )

                               if (is.null(timeout)) {
                                 stop("The app is unable to access your files because you were connected",
                                      " to the VPN and then disconnected. To fix this either reconnect to",
                                      " the VPN or restart your computer and use the app with out connecting",
                                      " to VPN. See issue https://github.com/see24/ccviR/issues/36 for more ",
                                      "information",
                                      call. = FALSE
                                 )
                               }

                               return(volumes)
                             }
    )

    observeEvent(c(input$scn_file, input$saveOutput, input$popGrowth_file), {
      shinyFiles::shinyFileChoose(input, "scn_file",
                                  roots = volumes(), session = session,
                                  filetypes = "csv"
      )

      shinyFiles::shinyFileChoose(input, "popGrowth_file",
                                  roots = volumes(), session = session,
                                  filetypes = "csv"
      )

      shinyFiles::shinyFileSave(input, "saveOutput",
                                roots = volumes(), session = session,
                                filetypes = c("csv", "rds")
      )
    })

    scn_df <- eventReactive(input$scn_file, {
      # if(!is.integer(input$scn_file)){
      df <- read.csv(shinyFiles::parseFilePaths(volumes, input$scn_file)$datapath)
      # check colnames match expected for caribouMetrics
      missed_nms <- setdiff(c("Anthro", "fire_excl_anthro", "Year"), colnames(df))
      if (length(missed_nms) > 0) {
        stop("The scenario file loaded is missing the columns ",
             paste(missed_nms, collapse = ", "),
             call. = FALSE
        )
      }

      return(df)
    })

    popGrow_df <- eventReactive(input$popGrowth_file, {
      if (!is.integer(input$popGrowth_file)) {
        df <- read.csv(shinyFiles::parseFilePaths(volumes, input$popGrowth_file)$datapath,
                       blank.lines.skip = TRUE
        )
        # check table matches expected for caribouMetrics
        df <- testPopGrowthTable(df)

        return(df)
      }
    })
    observe(print(popGrow_df()))

    observeEvent(input$scn_file, {
      if (!is.integer(input$scn_file)) {
        show("nYears")
        # set the inputs to these values based on the input data and enforce a max
        updateNumericInput(
          inputId = "nYearObs",
          value = input$curYear - min(scn_df()$Year) + 1,
          max = input$curYear - min(scn_df()$Year) + 1
        )

        updateNumericInput(
          inputId = "nYearProj",
          value = max(scn_df()$Year) - input$curYear,
          max = max(scn_df()$Year) - input$curYear
        )
      }
    })

    waiter <- waiter::Waiter$new(id = c("waiter-content"))

    observeEvent(input$Run.model, updateNavbarPage(
      inputId = "bodyTabs",
      selected = "Graphical summary"
    ))

    # Prepare national model
    simBig <- reactive({
      waiter$show()
      waiter$update(html = tagList(
        waiter::spin_1(),
        h4("Getting national model...")
      ))
      on.exit(waiter$hide())

      if(input$redoSimsNational){
        getSimsNational(forceUpdate = T,
                        fire_excl_anthro = input$iFire,cPars=list(cowMult=input$cowMult,
                                                               qMin = input$qRange[1],qMax=input$qRange[2],
                                                               uMin = input$uRange[1],uMax=input$uRange[2],
                                                               zMin = input$zRange[1],zMax=input$zRange[2],
                                                               assessmentYrs=input$assessmentYrs))
      } else {
        getSimsNational()
      }
    })


    dataInput <- eventReactive(input$Run.model, {

      waiter$show()
      waiter$update(html = tagList(
        waiter::spin_1(),
        h4("Running model...")
      ))
      on.exit(waiter$hide())

      startYear <- input$curYear - input$nYearObs + 1

      endYear <- input$curYear + input$nYearProj

      scns <- expand.grid(
        obsYears = input$nYearObs, projYears = input$nYearProj,
        obsAnthroSlope = input$obsAnthroSlope, projAnthroSlope = input$projAnthroSlope, rSlopeMod = input$rSlopeMod,
        sSlopeMod = input$sSlopeMod, iAnthro = input$iAnthro, iFire = input$iFire,
        rQuantile = input$rQuantile, sQuantile = input$sQuantile, N0 = input$N0,
        startYear = startYear,
        cowMult = input$cowMult, collarCount = input$collarCount,
        collarInterval = input$collarInterval,assessmentYrs=input$assessmentYrs,
        qMin = input$qRange[1],qMax=input$qRange[2],
        uMin = input$uRange[1],uMax=input$uRange[2],
        zMin = input$zRange[1],zMax=input$zRange[2]
      )

      scns <- getScenarioDefaults(scns)

      if (is.integer(input$scn_file)) {
        scn_df2 <- NULL
      } else {
        scn_df2 <- scn_df()
      }

      if (input$nat_model == "default") {
        popGrow_df2 <- caribouMetrics::popGrowthTableJohnsonECCC
      } else {
        if (is.integer(input$popGrowth_file)) {
          popGrow_df2 <- caribouMetrics::popGrowthTableJohnsonECCC
        } else {
          popGrow_df2 <- popGrow_df()
        }
      }

      oo <- simulateObservations(scns,
                                 distScen = scn_df2,
                                 collarNumYears = input$collarNumYears,
                                 collarOffTime = input$collarOffTime,
                                 collarOnTime = input$collarOnTime,
                                 populationGrowthTable = popGrow_df2,
                                 survivalModelNumber = input$survivalModelNumber,
                                 recruitmentModelNumber = input$recruitmentModelNumber
      )

      betaPriors <- getPriors(
        modList = c(scns,isolate(reactiveValuesToList(input))),
        populationGrowthTable = popGrow_df2
      )

      out <- caribouBayesianPM(
        survData = oo$simSurvObs, ageRatio = oo$ageRatioOut,
        disturbance = oo$simDisturbance, betaPriors = betaPriors,
        startYear = startYear, endYear = endYear,
        inputList = input
      )
      out$oo <- oo
      out$startYear <- startYear
      out$endYear <- endYear
      out
    })

    # TABLES #######
    dataInput1 <- eventReactive(input$Run.model, {

      out <- dataInput()
      if(input$getKSDists){
        waiter$show()
        waiter$update(html = tagList(
          waiter::spin_1(),
          h4("Calculating Kolmogorov-Smirnov distances...")
        ))
        on.exit(waiter$hide())
      }

      getOutputTables(
        out, exData = out$oo$exData, paramTable = out$oo$paramTable,
        simNational = simBig(), getKSDists = input$getKSDists
      )

    })

    output$table <- renderDataTable({
      dataInput1()$rr.summary.all %>%
        # remove params as they are included in label
        select(1:"fire_excl_anthro") %>%
        mutate(across(tidyselect::where(is.numeric), ~ round(.x, 2)))
    })

    dataInput2 <- eventReactive(input$Run.model, {
      df <- caribouMetrics:::getSumStats("S.annual.KM", dataInput()$result, dataInput()$startYear, dataInput()$endYear)

      names(df)[3] <- "Survival"

      df
    })

    output$table2 <- renderTable({
      dataInput2()
    })

    dataInput3 <- eventReactive(input$Run.model, {
      caribouMetrics:::getSumStats("R", dataInput()$result, dataInput()$startYear, dataInput()$endYear)
    })

    output$table3 <- renderTable({
      dataInput3()
    })

    dataInput4 <- eventReactive(input$Run.model, {
      caribouMetrics:::getSumStats("Rfemale", dataInput()$result, dataInput()$startYear, dataInput()$endYear)
    })

    output$table4 <- renderTable({
      dataInput4()
    })

    dataInput5 <- eventReactive(input$Run.model, {
      caribouMetrics:::getSumStats("pop.growth", dataInput()$result, dataInput()$startYear, dataInput()$endYear)
    })

    output$table5 <- renderTable({
      dataInput5()
    })

    dataInput7 <- eventReactive(input$Run.model, {
      caribouMetrics:::getSumStats("fpop.size", dataInput()$result, dataInput()$startYear, dataInput()$endYear)
    })

    output$table7 <- renderTable({
      dataInput7()
    })

    output$table6 <- renderDataTable({
      columnNAMES <- colnames(dataInput()$result$BUGSoutput$summary)

      x <- data.frame(
        row.names(dataInput()$result$BUGSoutput$summary),
        dataInput()$result$BUGSoutput$summary
      )

      names(x) <- c("Parameters", columnNAMES)
      x
    })

    observeEvent(input$mcmc.output, {
      req(dataInput())
      p <- mcmcplots::mcmcplot(dataInput()$result)
      print(p)
    })


    # PLOTS #######
    modTables <- reactive({
      req(dataInput1())
      scResults <- dataInput1()
      if(!input$showTruePop){
        scResults$obs.all <- NULL
      }
      if(!input$showNationalBands){
        scResults$sim.all <- NULL
      }
      scResults
    })

    # Adult female survival
    output$plot1 <- renderPlot({
      plotRes(modTables(), "Adult female survival", lowBound=0.6)
    })

    # Recruitment
    output$plot2 <- renderPlot({
      plotRes(modTables(), "Recruitment", lowBound=0)
    })

    output$plot3 <- renderPlot({
      plotRes(modTables(), "Adjusted recruitment", lowBound=0)
    })


    # lambda
    output$plot4 <- renderPlot({
      plotRes(modTables(), "Population growth rate", lowBound=0)
    })

    # size
    output$plot5 <- renderPlot({
      temp = modTables()
      temp$sim.all <- NULL
      plotRes(temp, "Female population size", lowBound=0)
    })

    output$plot6 <- renderPlot({
      out <- dataInput()
      dist <- out$oo$simDisturbance
      dist %>%
        tidyr::pivot_longer(c(.data$Anthro, .data$fire_excl_anthro),
                            names_to = "dist_type",
                            values_to = "dist"
        ) %>%
        mutate(dist_type = ifelse(.data$dist_type == "Anthro", "Anthropogenic",
                                  "Fire excluding anthropogenic"
        )) %>%
        ggplot2::ggplot(ggplot2::aes(.data$Year, .data$dist)) +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(~.data$dist_type, nrow = 1) +
        ggplot2::theme_classic() +
        ggplot2::xlab("Year") +
        ggplot2::ylab("% disturbance") +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = 14),
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = 14),
          axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
          axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
          strip.text = ggplot2::element_text(size = 14),
          strip.background = ggplot2::element_blank()
        ) +
        ggplot2::ylim(0, 100)
    })

    output$plot7 <- renderPlot({
      scResults <- dataInput1()
      plotRes(scResults, ksDists = TRUE, "Adult female survival")
    })
    output$plot8 <- renderPlot({
      scResults <- dataInput1()
      plotRes(scResults, ksDists = TRUE, "Recruitment")
    })
    output$plot9 <- renderPlot({
      scResults <- dataInput1()
      plotRes(scResults, ksDists = TRUE, "Population growth rate")
    })

    # Download output #######
    observeEvent(input$saveOutput, {
      if (!is.integer(input$saveOutput)) {
        path <- shinyFiles::parseSavePath(volumes(), input$saveOutput)$datapath
        if (grepl("\\.csv$", path)) {
          write.csv(dataInput1()$rr.summary.all, path, row.names = FALSE)
        } else if (grepl("\\.rds$", path)) {
          saveRDS(dataInput()$result, path)
        } else {
          warning(
            "file ", path, " could not be saved. Ensure that the file",
            " name includes .csv or .rds as the extension."
          )
        }
      }
    })

    # report.rmd is missing
    # output$downloadOutput <- downloadHandler(
    #   filename = function() {
    #     paste(input$inputIdCSV,".",sep = '', switch(
    #       input$format, HTML = 'html', PDF = 'pdf',  Word = 'docx'))
    #
    #
    #   },
    #
    #   content = function(file) {
    #     src <- normalizePath('report.Rmd')
    #
    #     # temporarily switch to the temp dir, in case you do not have write
    #     # permission to the current working directory
    #     owd <- setwd(tempdir())
    #     on.exit(setwd(owd))
    #     # on.exit(setwd(wdir)) # try this if download button won't work (write to working directory)
    #     file.copy(src, 'report.Rmd', overwrite = TRUE)
    #
    #     out <- render('report.Rmd', switch(
    #       input$format,
    #       PDF = pdf_document(), HTML = html_document(), Word = word_document()
    #     ))
    #     file.rename(out, file)
    #   }
    # )
  }


  # Run the application ---------------------------
  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
}


