library(shiny)
library(gamlss)
library(gamlss.data)
library(shiny)
library(gamlss)
library(gamboostLSS)
library(splines)
library(Sim.DiffProc)
library(BayesX)
library("R2BayesX")
library(shinyjs)
library(gamlss.tr)
library(stabs)
library(mboost)
library(parallel)
library(ggplot2)
options(shiny.maxRequestSize = 30*1024^2)

shinyServer(function(input, output, session) {
  my.data <- reactive({
    tryCatch(                       # Applying tryCatch
      expr = {                      # Specifying expression
        if (input$pilihdat == "IMPOR") {
          MyData <<- input$datasetr
          if (is.null(MyData))
            return(NULL)
          return(setDT(read.csv(
            MyData$datapath, sep = input$sep, header = TRUE
          )))
        }
        else if (input$pilihdat == "Excel") {
          MyData <<- input$datasetxl
          return(data.table(readXL(MyData$datapath, sheet = input$sheetn)))
        }
        else{
          switch(
            input$pilihdat,
            "CD4" = CD4,
            "LGAclaims" = LGAclaims,
            "alveolar" = alveolar,
            "computer" = computer,
            "iris" = iris,
            "rock" = rock,
            "mtcars" = mtcars,
            "orange" =Orange,
            "trees" = trees
            # "UnasSim" = un.as,
            # "lung(survival)" = lung(survival)
          )
        }
      },
      
      error = function(e){          # Specifying error message
        print("There was an error message.")
      },
      
      warning = function(w){        # Specifying warning message
        print("There was a warning message.")
      },
      
      finally = {                   # Specifying final message
        print("tryCatch is finished.")
      }
    )
    
    
  })
  
  output$var.sel.y <- renderUI({
    if (identical(my.data(), '') ||
        identical(my.data(), data.table()))
      return(NULL)
    # Variable selection:
    selectInput(
      "y",
      "Variabel Y",
      names(my.data()),
      names(my.data()),
      selectize = FALSE,
      multiple = FALSE
    )
  })
  
  output$var.sel.yfit <- renderUI({
    if (identical(my.data(), '') ||
        identical(my.data(), data.table()))
      return(NULL)
    # Variable selection:
    selectInput(
      "yfit",
      "Variabel Y untuk fitting",
      names(my.data()),
      names(my.data()),
      selectize = FALSE,
      multiple = FALSE
    )
  })
  
  output$var.sel.xGamboostLSS2 <- renderUI({
    if(input$toogle_formula_action == TRUE){
      if (identical(my.data(), '') ||
          identical(my.data(), data.table()))
        return(NULL)
      # Variable selection:
      selectInput(
        "xGamboostLSS2",
        "Daftar Variabel untuk fitting",
        names(my.data()),
        names(my.data()),
        selectize = FALSE,
        multiple = TRUE
        
      )
    }
  })
  
  output$informasi_penggunaan <- renderUI({
    if(input$toogle_custom_parameter == TRUE) {
      label = "Informasi Penggunaan, Silahkan Aktifkan toogle sesuai kebutuhan"
    }
  })
  
  output$informasi_penggunaan2 <- renderUI({
      label = "Note*: Informasi Penggunaan, Silahkan Aktifkan toogle sesuai kebutuhan\n
      & toggle hanya bisa dipilih salah satu untuk dapat melakukan proses data."
  })
  
  
  output$aktif_toogle_sigma <- renderUI({
    if(input$toogle_custom_parameter == TRUE) {
      materialSwitch(
        inputId = "toogle_sigma_exists",
        label = withMathJax(sprintf("Use Sigma \\(\\sigma\\)")),
        value = FALSE,
        status = "success"
      )
    }
  })
  
  output$aktif_toogle_mu <- renderUI({
    if(input$toogle_custom_parameter == TRUE) {
      materialSwitch(
        inputId = "toogle_mu_exists",
        label = withMathJax(sprintf("Use Mu \\(\\mu\\)")),
        value = FALSE,
        status = "success"
      )
    }
  })
  
  output$aktif_toogle_nu <- renderUI({
    if(input$toogle_custom_parameter == TRUE) {
      materialSwitch(
        inputId = "toogle_nu_exists",
        label = withMathJax(sprintf("Use Nu \\(\\nu\\)")),
        value = FALSE,
        status = "success"
      )
    }
  })
  
  output$aktif_toogle_tau <- renderUI({
    if(input$toogle_custom_parameter == TRUE) {
      materialSwitch(
        inputId = "toogle_tau_exists",
        label = withMathJax(sprintf("Use Tau \\(\\tau\\)")),
        value = FALSE,
        status = "success"
      )
    }
  })
  
  
  output$render_formula_input <- renderUI({
    if(input$toogle_formula_action == TRUE) {
      textInput("formula_input",
                "Insert Formula",
                value = "",
                placeholder = "Masukkan Formula ex: L_Popdensity ~ SD")
    }
  })
  
  output$render_nilai_sigma <- renderUI({
    if(input$toogle_sigma_exists == TRUE && input$toogle_custom_parameter== TRUE) {
      textInput("nilai_sigma",
                withMathJax(sprintf("Insert Nilai Sigma \\(\\sigma\\)")),
                value = "",
                placeholder = "Masukkan Nilai ex: exp(1.5 +1 * x1 +0.5 * x2)")
    }
  })
  output$render_formula_sigma <- renderUI({
    if(input$toogle_sigma_exists == TRUE && input$toogle_custom_parameter== TRUE) {
      textInput(
        "formula_sigma",
        withMathJax(sprintf("Insert Formula Sigma \\(\\sigma\\)")),
        value = "",
        placeholder = "Masukkan Formula ex: L_Popdensity ~ SD"
      )
    }
  })
  
  output$render_nilai_mu <- renderUI({
    if(input$toogle_mu_exists == TRUE && input$toogle_custom_parameter== TRUE) {
      textInput("nilai_mu",
                withMathJax(sprintf("Insert Nilai Mu \\(\\mu\\)")),
                value = "",
                placeholder = "Masukkan Nilai ex: exp(-0.4 * x3 -0.2 * x4)")
    }
  })
  output$render_formula_mu <- renderUI({
    if(input$toogle_mu_exists == TRUE && input$toogle_custom_parameter== TRUE) {
      textInput("formula_mu",
                withMathJax(sprintf("Insert Formula Mu \\(\\mu\\)")),
                value = "",
                placeholder = "Masukkan Formula ex: L_Popdensity ~ SD")
    }
  })
  
  output$render_nilai_nu <- renderUI({
    if(input$toogle_nu_exists == TRUE && input$toogle_custom_parameter== TRUE) {
      textInput("nilai_nu",
                withMathJax(sprintf("Insert Nilai Nu \\(\\nu\\)")),
                value = "",
                placeholder = "Masukkan Nilai ex: exp(-0.1 * x1 -0.2 * x4)")
    }
  })
  output$render_formula_nu <- renderUI({
    if(input$toogle_nu_exists == TRUE && input$toogle_custom_parameter== TRUE) {
      textInput("formula_nu",
                withMathJax(sprintf("Insert Formula Nu \\(\\nu\\)")),
                value = "",
                placeholder = "Masukkan Formula ex: L_Popdensity ~ SD")
    }
  })
  
  output$render_nilai_tau <- renderUI({
    if(input$toogle_tau_exists == TRUE && input$toogle_custom_parameter== TRUE) {
      textInput("nilai_tau",
                withMathJax(sprintf("Insert Nilai Tau \\(\\tau\\)")),
                value = "",
                placeholder = "Masukkan Nilai ex: exp(0.6 * x3 -0.2 * x1)")
    }
  })
  output$render_formula_tau <- renderUI({
    if(input$toogle_tau_exists == TRUE && input$toogle_custom_parameter== TRUE) {
      textInput("formula_tau",
                withMathJax(sprintf("Insert Formula Tau \\(\\tau\\)")),
                value = "",
                placeholder = "Masukkan Formula ex: L_Popdensity ~ SD")
    }
  })
  
  
  output$var.sel.yfitGamboostLSS <- renderUI({
    
    if (identical(my.data(), '') ||
        identical(my.data(), data.table()))
      return(NULL)
    # Variable selection:
    selectInput(
      "yfitGamboostLSS",
      "Variabel Y untuk fitting",
      names(my.data()),
      names(my.data()),
      selectize = FALSE,
      multiple = FALSE
      
    )
    
  })
  
  output$var.sel.x <- renderUI({
    if (identical(my.data(), '') ||
        identical(my.data(), data.table()))
      return(NULL)
    # Variable selection:
    selectInput(
      "x",
      "Variabel X untuk prediktor",
      names(my.data()),
      names(my.data()),
      selectize = FALSE,
      multiple = TRUE
    )
  })
  
  output$var.sel.xGamboostLSS <- renderUI({
    if(input$toogle_formula_action == FALSE){
      if (identical(my.data(), '') ||
          identical(my.data(), data.table()))
        return(NULL)
      # Variable selection:
      selectInput(
        "xGamboostLSS",
        "Variabel X Boosting untuk prediktor",
        names(my.data()),
        names(my.data()),
        selectize = FALSE,
        multiple = TRUE
      )
    }
  })
  
  output$sum.tot <- renderPrint({
    summary(my.data())
  })
  
  output$simple.reg <- renderPrint({
    y <- my.data()[, input$yfit]
    fm <- formula_run()
    cat("y~", paste(input$x, collapse = "+"))
    summary(lm(formula = fm, data = my.data()))
  })
  
  output$fit.dist <- renderPrint({
    y <- my.data()[, input$yfit]
    fit1 <- fitDist(
      y,
      k = 2,
      type = input$JenisDist,
      try.gamlss = FALSE,
      extra = NULL,
      data = NULL,
      trace = TRUE
    )
    return(fit1['fits'])
  })
  
  output$table.tot <- renderTable({
    my.data()[1:5, ]
  })
  
  output$table.y <- renderTable({
    my.data()[1:5, input$y]
  })
  
  output$plot <- renderPlot({
    x <- rnorm(100, 50, 5)
    y <- 3 * x + 5 + rnorm(100, 0, 5)
    plot(x, y, type = 'p')
  })
  
  output$plotT <- renderPlot({
    x <- rnorm(100, 50, 5)
    plot(x, type = input$JGraf)
  })
  
  output$summary <- renderPrint({
    x <- rnorm(100, 50, 5)
    y <- 3 * x + 5 + rnorm(100, 0, 5)
    dt <- data.table(x = x, y = y)
    summary(dt)
  })
  
  output$table <- renderTable({
    x <- rnorm(100, 50, 5)
    y <- 3 * x + 5 + rnorm(100, 0, 5)
    dt <- data.table(x = x, y = y)
    dt
  })
  
  output$FitDataGAMLSS <- renderPlot({
    y <- my.data()[, input$yfit]
    tesgam <-
      gamlssML(
        formula = y,
        family = input$GAMLSSdistT,
        data = my.data()
      )
    aicgam <- round(AIC(tesgam), 3)
    histDist(
      y,
      family = input$GAMLSSdistT,
      freq = NULL,
      density = TRUE,
      nbins = 10,
      xlim = NULL,
      ylim = NULL,
      main = paste(
        "DATA :",
        input$pilihdat,
        "Y :" ,
        input$yfit,
        "DISTRIBUSI :",
        input$GAMLSSdistT,
        "AIC :",
        aicgam
      ),
      xlab = NULL,
      ylab = paste(input$y),
      data = NULL,
      col.hist = "gray",
      border.hist = "blue",
      fg.hist = rainbow(12)[9],
      line.wd = 2,
      line.ty = c(1, 2),
      line.col = c(2, 3),
      col.main = "blue4",
      col.lab = "blue4",
      col.axis = "blue"
    )
  })
  output$man1 <- renderUI({
    div(
      # withMathJax($$f(x)= 1/(\beta^\alpha \\Gamma(\alpha)) x^{(\alpha-1)} e^{-(x/\beta)}$$),
      HTML(
        "
   <font color='blue' face='arial' size=6>
   <center>
    <h1><b>TES</b></h1>

	 <h1><b>UNIVERSITAS JEMBER</b></h1>
     </center>
	 </font>
	<br>



	 "
      )
    )
  })
  
  ##GamboostLSS ==>>>>>>>>>>>>>>>
  familiesInput <- reactive({
    data <- input$JenisDistGamboosLSS
    switch(data,
           "----Families for continuous response----" = "x",
           "----Families for continuous non-negative response----" = "y",
           "----Families for fractions and bounded continuous response----" = "z",
           "----Families for count data----" = "a",
           "----Families for survival models----" = "b",
           "GaussianLSS" = GaussianLSS(),
           "StudentTLSS" = StudentTLSS(),
           "GammaLSS" = GammaLSS(),
           "BetaLSS" = BetaLSS(),
           "NBinomialLSS" = NBinomialLSS(),
           "ZIPoLSS" = ZIPoLSS(),
           "ZINBLSS" = ZINBLSS(),
           "LogNormalLSS" = LogNormalLSS(),
           "LogLogLSS" = LogLogLSS(),
           "WeibullLSS" = WeibullLSS())
    
  })
  
  methodInput <- reactive({
    data <- input$JenisMethodGamboosLSS
    switch(data,
           "Cyclic" = "cyclic",
           "Non-Cyclic" = "noncyclic")
  })
  
  # output$text <- renderText({
  #   input$formula_input
  # })
  
  formula_run <- reactive({
    if(input$toogle_formula_action == TRUE) {
      formula(input$formula_input)
    } else if (input$toogle_formula_action == FALSE && input$toogle_custom_parameter == TRUE){
      #persiapan untuk custom parameter
    } 
    else {
      formula(paste(input$yfitGamboostLSS," ~ ", paste(input$xGamboostLSS, collapse = " + ")))
    }
  })
  #### kumpulan function
   print_deskripsi <- function () {
    cat("Informasi Formula:\n")
    print(formula_run())
    cat(paste("\nInformasi Data Menggunakan: ", input$pilihdat))
   }
  
  output$tes_dist_gamboosLSS <- renderPrint({
    y <- my.data()[, input$yfitGamboostLSS]
    fit1 <- fitDist(
      y,
      k = 2,
      type = input$JenisDist_testGamboosLSS,
      try.gamlss = FALSE,
      extra = NULL,
      data = NULL,
      trace = FALSE,
      show.progress = FALSE
    )
    return(fit1['fits'])
  })
  
  # cek_custom_parameter <- function () {
  #   if (exists(input$nilai_sigma) && exists(input$formula_sigma)) {
  #     
  #   }
  # }
  
  fitting_by_boosting <- reactive({
    if(familiesInput() != "x" || familiesInput() != "y" || familiesInput() != "z" ||
       familiesInput() != "a" || familiesInput() != "b"){
      model <- gamboostLSS(formula_run(), data = my.data(), families = familiesInput(),
                           control = boost_control(trace = TRUE, center = TRUE), weights = NULL, 
                           method = methodInput()) 
    }
  })
  
  
  get_var_x_prediktor <- reactive ({
    y <- toString(my.data()[, input$x])
  })
  
  output$var.sel.yfitGamboosLSS <- renderUI({
    if (identical(my.data(), '') ||
        identical(my.data(), data.table()))
      return(NULL)
    # Variable selection:
    selectInput(
      "yfit",
      "Variabel Y untuk fitting",
      names(my.data()),
      names(my.data()),
      selectize = FALSE,
      multiple = FALSE
    )
  })
  
  output$var.sel.xGamboosLSS <- renderUI({
    if (identical(my.data(), '') ||
        identical(my.data(), data.table()))
      return(NULL)
    # Variable selection:
    selectInput(
      "x",
      "Variabel X untuk prediktor",
      names(my.data()),
      names(my.data()),
      selectize = FALSE,
      multiple = TRUE
    )
  })
  
  output$as_family_gamboosLSS <- renderPrint({
    ## simulate small example
    set.seed(123)
    x <- runif(1000)
    y <- rnorm(mean = 2 + 3 * x,        # effect on mu
               sd   = exp( 1 - 1 * x ), # effect on sigma
               n    = 1000)
    if (require("gamlss.dist")) {
      glmss <- glmboostLSS(y ~ x, 
                           families = as.families(NO()),data=my.data(),
                           method=methodInput())
    }
    
    coef(glmss, off2int = TRUE)
    
    ## compare to gamlss
    library(gamlss)
    glmss2 <- gamlss(y ~ x, sigma.formula = ~x, family = "NO")
    coef(glmss2)
    glmss2$sigma.coef
  })
  
  output$formula_reg <- renderPrint({
    print_deskripsi()
    summary(lm(formula = formula_run(), data = my.data()))
  })
  
  
  
  # output$cv_risk <- renderPrint({
  #   ## Data generating process:
  #   set.seed(1907)
  #   x1 <- rnorm(1000)
  #   x2 <- rnorm(1000)
  #   x3 <- rnorm(1000)
  #   x4 <- rnorm(1000)
  #   x5 <- rnorm(1000)
  #   x6 <- rnorm(1000)
  #   mu    <- exp(1.5 +1 * x1 +0.5 * x2 -0.5 * x3 -1 * x4)
  #   sigma <- exp(-0.4 * x3 -0.2 * x4 +0.2 * x5 +0.4 * x6)
  #   y <- numeric(1000)
  #   for( i in 1:1000)
  #     y[i] <- rnbinom(1, size = sigma[i], mu = mu[i])
  #   dat <- data.table(x1, x2, x3, x4, x5, x6, y)
  #   
  #   ## linear model with y ~ . for both components: 100 boosting iterations
  #   model <- glmboostLSS(y ~ ., families = familiesInput(), data = dat,
  #                        method=methodInput(),
  #                        center = TRUE)
  #   
  #   ## set up a grid
  #   grid <-  make.grid(mstop(model), length.out = 5, dense_mu_grid = FALSE)
  #   plot(grid)
  #   
  #   ### a tiny toy example (5-fold bootsrap with maximum stopping value 100)
  #   ## (to run it on multiple cores of a Linux or Mac OS computer remove
  #   ##  set papply = mclapply (default) and set mc.nodes to the
  #   ##  appropriate number of nodes)
  #   cvr <- cvrisk(model, folds = cv(model.weights(model), B = 5),
  #                 papply = lapply, grid = grid)
  #   cvr
  #   ## plot the results
  #   par(mfrow = c(1, 2))
  #   plot(cvr)
  #   plot(cvr, type = "lines")
  #   ## extract optimal mstop (here: grid to small)
  #   mstop(cvr)
  #   
  #   ### a more realistic example
  #   grid <- make.grid(c(mu = 400, sigma = 400), dense_mu_grid = FALSE)
  #   plot(grid)
  #   cvr <- cvrisk(model, grid = grid)
  #   mstop(cvr)
  #   ## reset model to mstop values:
  #   model[mstop(cvr)]
  #   
  #   ### Other grids:
  #   plot(make.grid(mstop(model), length.out = 3, dense_mu_grid = FALSE))
  #   plot(make.grid(c(mu = 400, sigma = 400), log = FALSE, dense_mu_grid = FALSE))
  #   plot(make.grid(c(mu = 400, sigma = 400), length.out = 4,
  #                  min = 100, log = FALSE, dense_mu_grid = FALSE))
  #   
  #   
  #   ### Now use dense mu grids
  #   # standard grid
  #   plot(make.grid(c(mu = 100, sigma = 100), dense = FALSE),
  #        pch = 20, col = "red")
  #   # dense grid for all mstop_mu values greater than mstop_sigma
  #   grid <- make.grid(c(mu = 100, sigma = 100))
  #   points(grid, pch = 20, cex = 0.2)
  #   abline(0,1)
  #   
  #   # now with three parameters
  #   grid <- make.grid(c(mu = 100, sigma = 100, df = 30),
  #                     length.out = c(5, 5, 2), dense = FALSE)
  #   densegrid <- make.grid(c(mu = 100, sigma = 100, df = 30),
  #                          length.out = c(5, 5, 2))
  #   par(mfrow = c(1,2))
  #   # first for df = 1
  #   plot(grid[grid$df == 1, 1:2], main = "df = 1", pch = 20, col = "red")
  #   abline(0,1)
  #   abline(v = 1)
  #   # now expand grid for all mu values greater the corresponding sigma
  #   # value (i.e. below the bisecting line) and above df (i.e. 1)
  #   points(densegrid[densegrid$df == 1, 1:2], pch = 20, cex = 0.2)
  #   
  #   # now for df = 30
  #   plot(grid[grid$df == 30, 1:2], main = "df = 30", pch = 20, col = "red")
  #   abline(0,1)
  #   abline(v = 30)
  #   # now expand grid for all mu values greater the corresponding sigma
  #   # value (i.e. below the bisecting line) and above df (i.e. 30)
  #   points(densegrid[densegrid$df == 30, 1:2], pch = 20, cex = 0.2)
  # })
  
  
  
  
  
  output$fitting_gamlss_by_boosting <- renderPrint({
    model3 <- fitting_by_boosting()
    # summary(coef(model3, off2int = TRUE))
    print_deskripsi()
    summary(model3)
  })
  
  output$plot_fitting_gamlss_by_boosting <- renderPlot({
    model2<-fitting_by_boosting()
    model2[c(400, 300)]
    n_parameter <- length(input$xGamboostLSS)
    par(mfrow = c(n_parameter, 2))
    plot(model2, xlim = c(0, max(mstop(model2))))
    ## all.equal(coef(model2), coef(model3)) # same!
  })
  
  output$GamboosLSSdistT <- renderUI({
    # if (identical(my.data(), '') ||
    #     identical(my.data(), data.table()))
    #   return(NULL)
    # # Variable selection:
    # selectInput(
    #   "yfit",
    #   "Variabel Y untuk fitting",
    #   names(data_drop_GamboosLSSdistT),
    #   names(data_drop_GamboosLSSdistT),
    #   selectize = FALSE,
    #   multiple = FALSE
    # )
  })
  
  
  runGamboosLSSRisk <- reactive({
    model <- fitting_by_boosting()
  })
  
  output$sigma <- renderPlot({
    mod1 <- runGamboosLSSRisk()
    plot(risk(mod1)$sigma)
  })
  output$mu <- renderPlot({
    mod1 <- runGamboosLSSRisk()
    plot(risk(mod1)$mu)
  })
  output$summary_gamma <- renderPrint({
    mod1 <- runGamboosLSSRisk()
    print_deskripsi()
    summary(mod1)
  })
  
  run_cv_risk_gamboostLSS <- reactive({
    model <- fitting_by_boosting()
    
  })
  
  output$plot_cv_risk_gamboostLSS <- renderPlot({
    model<- run_cv_risk_gamboostLSS()
    grid <-  make.grid(mstop(model), dense_mu_grid = FALSE)
    plot(grid)
  })
  
  # output$summary_plot_cv_risk_gamboostLSS <- renderPrint({
  #   model<- run_cv_risk_gamboostLSS()
  #   grid <-  make.grid(mstop(model), length.out = 5, dense_mu_grid = FALSE)
  #   cvr <- cvrisk(model)
  #   cvr
  # })
  
  output$tes_family_gamboosLSS<- renderPrint({
    y <- my.data()[, input$yfit]
    fm <- formula_run()
    try(gamboostLSS(fm, data = my.data(), families = familiesInput(),
                             control = boost_control(trace = TRUE), weights = NULL, 
                             method = methodInput()))
  })
  
  run_method_gamboostLSS <- reactive({
    model <- fitting_by_boosting()
  })
  
  output$sum_coef<- renderPrint({
    model <- run_method_gamboostLSS()
    model[400]
    print_deskripsi()
    summary(model)
  })
  # output$sum_distpar<- renderPrint({
  #   model <- run_method_gamboostLSS()
  #   summary(model, parameter = "mu")
  # })
  # 
  # output$sum_covariate<- renderPrint({
  #   model <- run_method_gamboostLSS()
  #   summary(model, which = input$x)
  # })
  
  output$plot_model <- renderPlot({
    model <- run_method_gamboostLSS()
    plot(model)
  })
  output$plot_marginal_prediction <- renderPlot({
    model <- run_method_gamboostLSS()
    n_parameter <- length(input$xGamboostLSS)
    pi <- predint(model, pi = 0.9, which = input$xGamboostLSS)
    pi <- predint(model, pi = c(0.8, 0.9), which = input$xGamboostLSS)
    plot(pi, log = "y", ylim = c(1, 100))
  })
  output$plot_risk_model <- renderPlot({
    model <- run_method_gamboostLSS()
    par(mfrow = c(2, 2))
    plot(risk(model, parameter = "mu")[[1]])
    plot(risk(model, parameter = "sigma")[[1]])
    
    ### get back to orignal fit
    model[400]
    plot(risk(model, parameter = "mu")[[1]])
    plot(risk(model, parameter = "sigma")[[1]])
  })
  output$plot_effect_parameter <- renderPlot({
    model <- run_method_gamboostLSS()
    n_parameter <- length(input$xGamboostLSS)
    par(mfrow = c(n_parameter, 2))
    plot(model, which = input$xGamboostLSS)
    ### now plot only effect of x3 of both parameters
  })
  
  ## datatables menu
  run_data <- reactive({
    
    if (input$toggle_spesifik_filter == TRUE && input$toggle_custom_parameter == FALSE) {
      tryCatch(                       # Applying tryCatch
        expr = {                      # Specifying expression
          data <- as.data.table(my.data())
          filter <- paste0(input$variabel_filter, input$nilai_filter)
          return (data[eval(parse(text = filter))])
          message("Everything was fine.")
        },
        
        error = function(e){          # Specifying error message
          message("There was an error message.")
        },
        
        warning = function(w){        # Specifying warning message
          message("There was a warning message.")
        },
        
        finally = {                   # Specifying final message
          message("tryCatch is finished.")
        }
      )
    
      
    } else if(input$toggle_custom_parameter == TRUE && input$toggle_spesifik_filter == FALSE){
      tryCatch(                       # Applying tryCatch
        expr = {                      # Specifying expression
          data <- as.data.table(my.data())
          filter <- paste0(input$nilai_custom_parameter)
          return (data[eval(parse(text = filter))])
          message("Everything was fine.")
        },
        
        error = function(e){          # Specifying error message
          message("There was an error message.")
        },
        
        warning = function(w){        # Specifying warning message
          message("There was a warning message.")
        },
        
        finally = {                   # Specifying final message
          message("tryCatch is finished.")
        }
      )
    }
    
    
    else{
      data <- as.data.table(my.data())
      return (data)
    }
  })
  
  
  output$informasi_datatable <- renderPrint({
    cat("Check Is Datatable :", is.data.table(run_data()),"\n")
    cat(paste("Berikut Informasi Ringkasan Data : ", input$pilihdat),"\n\n")
    cat("Dia milih ", input$variabel_filter,"\n\n")
    return (summary(run_data()))
  })
  
  output$grafik1 <- renderPlot({
    ggplot(run_data(), aes_string(input$variabel1)) + geom_bar(stat = "count")
  })
  
  output$grafik2 <- renderPlot({
    ggplot(run_data(), aes_string(x=input$variabel1, fill=input$variabel2)) + geom_bar(stat = "count", position="dodge")
  })
  
  output$grafik3 <- renderPlot({
    ggplot(data = run_data(), mapping = aes_string(x=input$variabel1, y=input$variabel2)) + 
      geom_point(mapping = aes_string(color = input$variabel2)) + 
      geom_smooth()
  })
  
  output$grafik4 <- renderPlot({
    ggplot(data = run_data()) + 
      stat_summary(
        mapping = aes_string(x=input$variabel1, y=input$variabel2),
        fun.min = min,
        fun.max = max,
        fun = median
      )
  })
  output$grafik5 <- renderPlot({
    ggplot(data = run_data(), mapping = aes_string(x=input$variabel1, y=input$variabel2)) +
      geom_boxplot(alpha = 0) +
      geom_jitter(alpha = 0.3, color = "red")
  })
  
  output$var.sel.filter <- renderUI({
    if (identical(my.data(), '') ||
        identical(my.data(), data.table()))
      return(NULL)
    # Variable selection:
    selectInput(
      "variabel1",
      "Kelompok",
      names(my.data()),
      selectize = FALSE,
      multiple = FALSE
    )
  })
  
  output$var.sel.filter2 <- renderUI({
    if (identical(my.data(), '') ||
        identical(my.data(), data.table()))
      return(NULL)
    # Variable selection:
    selectInput(
      "variabel2",
      "Kelompok Fill data",
      names(my.data()),
      selectize = FALSE,
      multiple = FALSE
    )
  })
  
  output$spesific_value <- renderUI({
    if(input$toggle_spesifik_filter == TRUE && input$toggle_custom_parameter == FALSE) {
      textInput("nilai_filter",
                withMathJax(sprintf("Insert Parameter value \\(\\theta\\)")),
                value = "",
                placeholder = "Ex: > 5")
    }
  })
  
  output$spesific_variabel <- renderUI({
    if(input$toggle_spesifik_filter == TRUE && input$toggle_custom_parameter == FALSE) {
      if (identical(my.data(), '') ||
          identical(my.data(), data.table()))
        return(NULL)
      # Variable selection:
      selectInput(
        "variabel_filter",
        "Choose Custom Variabel",
        names(my.data()),
        selectize = FALSE,
        multiple = FALSE
      )
    }
  })
  
  output$custom_parameter <- renderUI({
    if(input$toggle_custom_parameter == TRUE && input$toggle_spesifik_filter==FALSE) {
      textInput("nilai_custom_parameter",
                withMathJax(sprintf("Insert Custom Parameter value \\(\\theta\\)")),
                value = "",
                placeholder = "Ex :Data > 5 & Data % 2 == 0")
    }
  })
  
})