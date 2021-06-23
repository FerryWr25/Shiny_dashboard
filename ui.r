library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(data.table)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Welcome to Statistic"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Input data",
      tabName = "tab_input_data",
      icon = icon("cog"),
      badgeColor = "green"
    ), menuItem(
      "Review",
      tabName = "tab_review",
      icon = icon("dashboard"),
      badgeColor = "orange"
    ),
    menuItem(
      "GAMLSS",
      tabName = "tab_gamlss",
      icon = icon("th"),
      badgeColor = "green"
    ),
    menuItem(
      "GAMBOOSTLSS",
      tabName = "tab_gamboostLSS",
      icon = icon("th"),
      badgeLabel = "team",
      badgeColor = "green"
    ),
    menuItem(
      "Data.Tables",
      tabName = "tab_datatables",
      icon = icon("th"),
      badgeLabel = "team",
      badgeColor = "green"
    )
  )),
  dashboardBody(tabItems(
    tabItem("tab_review",
            tabPanel(
              "TAB PANEL",
              sidebarLayout(
                box(
                  title = "Inputs",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  "SIDE BAR",
                  br(),
                  "Plain Teks",
                  HTML("<font color='blue'> <b>HTML Format</b></font>"),
                  selectInput(
                    "JGraf",
                    "Jenis Grafik Tunggal:",
                    choices = c(
                      "Titik" = 'p',
                      "Garis" = 'l',
                      "Dobel" = 'b'
                    )
                  )
                ),
                mainPanel(tabsetPanel(
                  tabPanel("Tabel", tableOutput('table')),
                  tabPanel("Teks", verbatimTextOutput('summary')),
                  tabPanel("Plot", plotOutput('plot')),
                  tabPanel("Plot Tunggal", plotOutput('plotT'))
                ))
                
              )
            )),
    tabItem("tab_input_data",
            tabPanel(
              titlePanel("Input Data"),
              sidebarLayout(
                box(
                  title = "Input Data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  collapsible = TRUE,
                  "Dibagian ini pengguna dapat membaca data terutama dalam format excel dan teks (csv)",
                  br(),
                  "Tunggu sampai daftar data lengkap muncul baru bisa diproses !!!",
                  br(),
                  br(),
                  selectInput(
                    "pilihdat",
                    "Pilih Data:",
                    choices = c(
                      "Import  Excel" = "Excel",
                      "Import CSV" = "IMPOR",
                      "Simulasi",
                      "CD4",
                      "LGAclaims",
                      "alveolar",
                      "computer",
                      "iris",
                      "rock",
                      "mtcars",
                      "orange",
                      "trees",
                      "UnasSim",
                      "lung(survival)"
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "input.pilihdat == 'Excel'",
                    fileInput('datasetxl', 'Pilih File EXCEL ...'),
                    numericInput(
                      'sheetn',
                      "Nomor Sheet yang Dibaca",
                      1,
                      min = 1,
                      max = 10,
                      step = 1
                    )
                  ),
                  
                  
                  conditionalPanel(
                    condition = "input.pilihdat == 'IMPOR'",
                    fileInput(
                      'datasetr',
                      'Pilih File CSV/ Teks',
                      # FILEINPUT
                      accept = c('text/csv', 'text/comma-separated-values,text/plain')
                    ),
                    tags$hr(),
                    checkboxInput('header', 'Header', TRUE),
                    radioButtons(
                      'sep',
                      'Separator',
                      c(
                        Comma = ',',
                        Semicolon = ';',
                        Tab = '\t'
                      ),
                      'Comma'
                    ),
                    radioButtons(
                      'quote',
                      'Quote',
                      c(
                        None = '',
                        'Double Quote' = '"',
                        'Single Quote' = "'"
                      ),
                      'Double Quote'
                    )
                  ),
                  br(),
                  "Pilih Variabel",
                  uiOutput("var.sel.y"),
                  br(),
                  HTML("<font color='blue'> <b></b></font>")
                ),
                
                #numericInput("obs", "Banyaknya observasi:", 10)),
                #numericInput('ns', "n-sampel: ", 10, min = 10, max = 100, step = 5),
                #if(F){
                #),   }
                box(
                  title = "Histogram",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  collapsible = TRUE,
                  tabsetPanel(
                    #tabPanel("Pengantar", uiOutput("introProfil")),
                    tabPanel("Ringkasan Data", verbatimTextOutput('sum.tot')),
                    tabPanel("Dafar 5 Data pertama", tableOutput('table.tot')),
                    tabPanel("Variabel Terpilih", tableOutput('table.y'))
                  )
                )
              )
            )),
    tabItem("tab_gamlss",
            tabPanel(
              "Fit Distribusi",
              sidebarLayout(
                box(
                  title = "Inputs",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  "SIDE BAR",
                  br(),
                  "Plain Teks",
                  HTML("<font color='blue'> <b>HTML Format</b></font>"),
                  selectInput(
                    "JenisDist",
                    "Pilih Jenis Distribusi :",
                    choices =
                      c(
                        "realAll",
                        "realline",
                        "realplus",
                        "real0to1",
                        "counts",
                        "binom"
                      )
                  ),
                  
                  uiOutput("var.sel.yfit"),
                  uiOutput("var.sel.x"),
                  
                  selectInput(
                    "GAMLSSdistT",
                    "Pilih Distribusi Teori Pencocok:",
                    choices =
                      c(
                        "---Semua Real ---",
                        "NO",
                        "GU",
                        "RG" ,
                        "LO",
                        "NET",
                        "TF",
                        "TF2",
                        "PE",
                        "PE2",
                        "SN1",
                        "SN2",
                        "exGAUS",
                        "SHASH",
                        "SHASHo",
                        "SHASHo2",
                        "EGB2",
                        "JSU",
                        "JSUo",
                        "SEP1",
                        "SEP2",
                        "SEP3",
                        "SEP4",
                        "ST1",
                        "ST2",
                        "ST3",
                        "ST4",
                        "ST5",
                        "SST",
                        "GT",
                        "---Positif Real ---",
                        "EXP",
                        "GA",
                        "IG",
                        "LOGNO",
                        "LOGNO2",
                        "WEI",
                        "WEI2",
                        "WEI3",
                        "IGAMMA",
                        "PARETO2",
                        "PARETO2o",
                        "GP",
                        "BCCG",
                        "BCCGo",
                        "exGAUS",
                        "GG",
                        "GIG",
                        "LNO",
                        "BCTo",
                        "BCT",
                        "BCPEo",
                        "BCPE",
                        "GB2",
                        "--Kontinu [0 1] ---",
                        "BE",
                        "BEo",
                        "BEINF0",
                        "BEINF1",
                        "BEOI",
                        "BEZI",
                        "BEINF",
                        "GB1",
                        "--Cacahan Tak hingga---",
                        "PO",
                        "GEOM",
                        "GEOMo",
                        "LG",
                        "YULE",
                        "ZIPF",
                        "WARING",
                        "GPO",
                        "DPO",
                        "BNB",
                        "NBF",
                        "NBI",
                        "NBII",
                        "PIG",
                        "ZIP",
                        "ZIP2",
                        "ZAP",
                        "ZALG",
                        "DEL",
                        "ZAZIPF",
                        "SI",
                        "SICHEL",
                        "ZANBI",
                        "ZAPIG",
                        "ZINBI",
                        "ZIPIG",
                        "ZINBF",
                        "ZABNB",
                        "ZASICHEL",
                        "ZINBF",
                        "ZIBNB",
                        "ZISICHEL",
                        "--Cacahan Terbatas---",
                        "BI",
                        "BB",
                        "DB",
                        "ZIBI",
                        "ZIBB",
                        "ZABI",
                        "ZABB"
                      )
                  )
                  
                )
                ,
                mainPanel(
                  tabsetPanel(
                    tabPanel("Tes Distribusi", verbatimTextOutput('fit.dist')),
                    tabPanel("Fit GAMLSS Dist", plotOutput('FitDataGAMLSS')),
                    tabPanel("Simple Regresi", verbatimTextOutput('simple.reg')),
                    tabPanel("Manual Ringkas", uiOutput("man1"))
                  ),
                  withMathJax(
                    helpText(
                      "Fungsi kepdatan Gamma $$f(x)= 1/(\\beta^\\alpha \\Gamma(\\alpha)) x^{(\\alpha-1)} e^{-(x/\\beta)}$$"
                    )
                  ),
                  withMathJax(
                    helpText(
                      "Fuangsi kepdatan Gamma $$f(x)= 1/(\\beta^\\alpha \\Gamma(\\alpha)) x^{(\\alpha-1)} e^{-(x/\\beta)}$$"
                    )
                  )
                )
              )
            )),
    tabItem("tab_gamboostLSS",
            tabPanel(
              "Fit Distribusi",
              sidebarLayout(
                box(
                  title = "Inputs Data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  collapsible = TRUE,
                  selectInput(
                    "JenisDist_testGamboosLSS",
                    "Pilih Jenis Distribusi Testing:",
                    choices =
                      c(
                        "realAll",
                        "realline",
                        "realplus",
                        "real0to1",
                        "counts",
                        "binom"
                      )
                  ),
                  selectInput(
                    "JenisDistGamboosLSS",
                    "Pilih Jenis Distribusi Boosting GamboosLSS:",
                    choices =
                      c(
                        "----Families for continuous response-----" = "x",
                        "Gaussian distribution" = "GaussianLSS",
                        "Student's t-distribution" = "StudentTLSS",
                        "----Families for continuous non-negative response----" =
                          "y",
                        "Gamma distribution" = "GammaLSS",
                        "----Families for fractions and bounded continuous response----" =
                          "z",
                        "Beta distribution" = "BetaLSS",
                        "----Families for count data----" = "a",
                        "Negative binomial distribution" = "NBinomialLSS",
                        "Zero-inflated Poisson distribution" = "ZIPoLSS",
                        "Zero-inflated negative binomial distribution" = "ZINBLSS",
                        "----Families for survival models----" = "b",
                        "Log-normal distribution" = "LogNormalLSS",
                        "Log-logistic distribution" = "LogLogLSS",
                        "Weibull distribution" = "WeibullLSS"
                      )
                  ),
                  selectInput(
                    "JenisMethodGamboosLSS",
                    "Pilih Jenis Method GamboosLSS :",
                    choices =
                      c("Cyclic" = "cyclic",
                        "Non-Cyclic" = "noncyclic")
                  ),
                  uiOutput("GamboosLSSdistT"),
                  uiOutput("var.sel.yfitGamboostLSS"),
                  uiOutput("var.sel.xGamboostLSS2"),
                  materialSwitch(
                    inputId = "toogle_formula_action",
                    label = "Switch to Custom General Formula",
                    value = FALSE,
                    status = "info"
                  ),
                  uiOutput("render_formula_input"),
                  uiOutput("var.sel.xGamboostLSS"),
                  materialSwitch(
                    inputId = "toogle_custom_parameter",
                    label = "Switch to advanced Custom Parameter",
                    value = FALSE,
                    status = "info"
                  ),
                  uiOutput("informasi_penggunaan"),
                  tags$style(type="text/css", "#informasi_penggunaan {color: red}"),
                  br(),
                  uiOutput("aktif_toogle_sigma"),
                  uiOutput("aktif_toogle_mu"),
                  uiOutput("aktif_toogle_nu"),
                  uiOutput("aktif_toogle_tau"),
                  uiOutput("render_nilai_sigma"),
                  uiOutput("render_formula_sigma"),
                  
                  uiOutput("render_nilai_mu"),
                  uiOutput("render_formula_mu"),
                  
                  uiOutput("render_nilai_nu"),
                  uiOutput("render_formula_nu"),
                  
                  uiOutput("render_nilai_tau"),
                  uiOutput("render_formula_tau"),
                  # verbatimTextOutput("text")
                  # prettyCheckbox(
                  #   inputId = "pretty_3", label = "Check me!", icon = icon("users"),
                  #   animation = "pulse", plain = TRUE, outline = TRUE
                  # )
                ),
                box(
                  title = "Hasil Pengolahan Data",
                  status = "primary",
                  width = 8,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  tabsetPanel(
                    tabPanel("Tes Distibusi", verbatimTextOutput('tes_dist_gamboosLSS')),
                    tabPanel("Tes Families", verbatimTextOutput('tes_family_gamboosLSS')),
                    tabPanel("Formula Regresi", verbatimTextOutput('formula_reg')),
                    tabPanel(
                      "Risk Sigma & Mu",
                      fluidRow(
                        column(
                          width = 12,
                          align = "center",
                          box(
                            height = 400,
                            width = 500,
                            title = "Plotting Sigma",
                            solidHeader = TRUE,
                            status = "primary",
                            collapsible = TRUE,
                            plotOutput("sigma", height = 330)
                          )
                        ),
                        column(
                          width = 12,
                          align = "center",
                          box(
                            height = 400,
                            width = 500,
                            title = "Plotting Mu",
                            solidHeader = TRUE,
                            status = "primary",
                            collapsible = TRUE,
                            plotOutput("mu", height = 330)
                          )
                        )
                      ),
                      verbatimTextOutput('summary_gamma')
                    ),
                    tabPanel(
                      "Run Method",
                      fluidRow(
                        column(
                          width = 12,
                          align = "center",
                          box(
                            height = 400,
                            width = 500,
                            title = "Plot marginal prediction interval",
                            solidHeader = TRUE,
                            status = "primary",
                            collapsible = TRUE,
                            plotOutput(
                              "plot_marginal_prediction",
                              height = 330,
                              width = 500
                            )
                          )
                        ),
                        column(
                          width = 12,
                          align = "center",
                          div(style = "height: auto"),
                          box(
                            # height = "auto",
                            width = 500,
                            title = "Plot effect parameter",
                            solidHeader = TRUE,
                            status = "primary",
                            collapsible = TRUE,
                            plotOutput("plot_effect_parameter", height = 500, width = 500)
                          )
                        ),
                        column(
                          width = 12,
                          align = "center",
                          box(
                            height = 480,
                            width = 500,
                            title = "Plot Model",
                            solidHeader = TRUE,
                            status = "primary",
                            collapsible = TRUE,
                            br(),
                            plotOutput("plot_model", height = 380, width = 500)
                          )
                        ),
                        column(
                          width = 12,
                          align = "center",
                          box(
                            height = 600,
                            width = 500,
                            title = "Plot Risk Model",
                            solidHeader = TRUE,
                            status = "primary",
                            collapsible = TRUE,
                            plotOutput("plot_risk_model", height = 500)
                          )
                        )
                      ),
                      box(
                        width = 12,
                        title = "Summary extract",
                        solidHeader = TRUE,
                        status = "primary",
                        collapsible = TRUE,
                        verbatimTextOutput('sum_coef')
                      )
                      # box(
                      #   width = 12,
                      #   title = "Summary distribution parameter",
                      #   solidHeader = TRUE,
                      #   status = "primary",
                      #   collapsible = TRUE,
                      #   verbatimTextOutput('sum_distpar')
                      # ),
                      # box(
                      #   width = 12,
                      #   title = "Summary covariate",
                      #   solidHeader = TRUE,
                      #   status = "primary",
                      #   collapsible = TRUE,
                      #   verbatimTextOutput('sum_covariate')
                      # )
                    ),
                    # tabPanel("AS Family", verbatimTextOutput('as_family_gamboosLSS')),
                    tabPanel(
                      "Fitting GAMLSS",
                      fluidRow(column(
                        width = 12,
                        plotOutput("plot_fitting_gamlss_by_boosting")
                      )),
                      verbatimTextOutput('fitting_gamlss_by_boosting')
                    ),
                    tabPanel(
                      "CV Risk",
                      fluidRow(column(
                        width = 12,
                        plotOutput("plot_cv_risk_gamboostLSS")
                      )),
                      verbatimTextOutput('summary_plot_cv_risk_gamboostLSS')
                    )
                    
                  )
                )
              )
            )),
    tabItem("tab_datatables",
            tabPanel(
              "TAB PANEL",
              sidebarLayout(
                box(
                  title = "Filtering Data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  collapsible = TRUE,
                  
                  uiOutput("var.sel.filter"),
                  uiOutput("var.sel.filter2"),
                  materialSwitch(
                    inputId = "toggle_spesifik_filter",
                    label = "Switch to Spesific Parameter",
                    value = FALSE,
                    status = "info"
                  ),
                  uiOutput("spesific_variabel"),
                  uiOutput("spesific_value"),
                  uiOutput("xx"),
                  materialSwitch(
                    inputId = "toggle_custom_parameter",
                    label = "Switch to advanced Custom Parameter",
                    value = FALSE,
                    status = "info"
                  ),
                  uiOutput("custom_parameter"),
                  hr(),
                  uiOutput("informasi_penggunaan2"),
                  tags$style(type="text/css", "#informasi_penggunaan2 {color: red}")
                  # verbatimTextOutput("text")
                  # prettyCheckbox(
                  #   inputId = "pretty_3", label = "Check me!", icon = icon("users"),
                  #   animation = "pulse", plain = TRUE, outline = TRUE
                  # )
                ),
                box(
                  title = "Hasil Pengolahan Data",
                  status = "primary",
                  width = 8,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  tabsetPanel(
                    tabPanel("Informasi Data", verbatimTextOutput('informasi_datatable')),
                    tabPanel(
                      "Grafik Data",
                      fluidRow(
                        column(
                          width = 12,
                          align = "center",
                          box(
                            height = 400,
                            width = 500,
                            title = "Standard chart",
                            solidHeader = TRUE,
                            status = "primary",
                            collapsible = TRUE,
                            plotOutput("grafik1", height = 330)
                          )
                        ),
                        column(
                          width = 12,
                          align = "center",
                          box(
                            height = 400,
                            width = 500,
                            title = "Standard chart with fill",
                            solidHeader = TRUE,
                            status = "primary",
                            collapsible = TRUE,
                            plotOutput("grafik2", height = 330)
                          )
                        ),
                        column(
                          width = 12,
                          align = "center",
                          box(
                            height = 400,
                            width = 500,
                            title = "Standard chart with mapping",
                            solidHeader = TRUE,
                            status = "primary",
                            collapsible = TRUE,
                            plotOutput("grafik3", height = 330)
                          )
                        ),
                        column(
                          width = 12,
                          align = "center",
                          box(
                            height = 400,
                            width = 500,
                            title = "Standard chart with mapping & Stat summary",
                            solidHeader = TRUE,
                            status = "primary",
                            collapsible = TRUE,
                            plotOutput("grafik4", height = 330)
                          )
                        ),
                        column(
                          width = 12,
                          align = "center",
                          box(
                            height = 400,
                            width = 500,
                            title = "Grafik5",
                            solidHeader = TRUE,
                            status = "primary",
                            collapsible = TRUE,
                            plotOutput("grafik5", height = 330)
                          )
                        )
                      ),
                      verbatimTextOutput('hasil6')
                    ),
                    tabPanel("Uji Chi-Sq", verbatimTextOutput('hasil3'))
                    # tabPanel("AS Family", verbatimTextOutput('as_family_gamboosLSS')),
                  )
                )
                
              )
            ))
  ))
)
