library(shiny)
library(shinydashboard)
library(shinycssloaders)
#library(shinyjs)
options(shiny.error = browser)

# load the functions from external file
source("helpers.R")

# Define UI element for application ----
# sidebar ----
sidebar <- dashboardSidebar(
  #useShinyjs(),
  sidebarMenu(
    id = "sidebarmenu",
    menuItem("Data files", tabName = "datafiles"),
    menuItem("Sessions", tabName = "sessions"),
    menuItem("Inter Observers Reliability", 
             #tabName = "irr",
             menuSubItem("Compare sessions", tabName = "irr"), #, selected = FALSE
             menuSubItem("Multi observers", tabName = "irr_multi"),
             menuSubItem("Static plot", tabName = "staticPlot"),
             menuSubItem("Raw proportions plot", tabName = "plotsProp"),
             menuSubItem("Track progresses", tabName = "trackProgress")),
    conditionalPanel(
      "input.sidebarmenu == 'datafiles'",
      div(
        selectizeInput(
          'datafile',
          label = "Select a file",
          choices = list.files(path = "data/",
                               full.names = FALSE)),
        actionButton("load", label = "Load data"))),
    conditionalPanel(
      "input.sidebarmenu == 'sessions' || input.sidebarmenu == 'irr' ",
      div(
        selectizeInput('session1', label = "Session 1", choices = c(52:60)),
        selectizeInput('session2', label = "Session 2", choices = c(52:60)),
        actionButton("compare", "Compare sessions")#,

    )),
    conditionalPanel(
      "input.sidebarmenu == 'sessions' || input.sidebarmenu == 'irr_multi' ",
      div(
        selectizeInput('session_ids', label = "Choose the sessions (if > 2)", choices = c(52:60), multi = TRUE),
        actionButton("compareMulti", "Compare all sessions")#,
        
      )),
    conditionalPanel(
      "input.sidebarmenu == 'staticPlot' ",
      div(
        numericInput("start", "Plot from (sec):", min = 0, value = 0),
        numericInput("end", "To (sec):", min = 0, value = 300),
        checkboxInput("labs", "Show labels", value = TRUE),
        radioButtons("units", "Time unit in axis:", c("Minutes" = "min",
                                                      "Seconds" = "sec"))#,
       # actionButton("update", "Update plot")
      )),
    conditionalPanel(
      "input.sidebarmenu == 'irr'",
      # div(
      #   p("Brief information on selected sessions...") # add id observers and participant, n. tasks recorded by each, % time in multitasking, n. interruptions
      # ),
      # uiOutput("sessionsInfo"), # or uiOutput("sessionsInfo") to be rendered in server
      actionButton("save", "Save results")
    ),
    conditionalPanel(
      "input.sidebarmenu == 'trackProgress'",
      div(
        selectizeInput('results_ids', label = "Choose the IORA ids to plot", choices = c(1:10), multi = TRUE),
        actionButton("plotSelectedResults", "Plot selected results")#
      )
    )
    #)
  )
)

# body ----
body <- dashboardBody(
  tags$style(".small-box {height: 20; width: 150; }"),
                      
                      
                      # first page (datafiles) ----
                      tabItems(
                        # first page (files list)
                        tabItem(
                          tabName = "datafiles",
                          h2("Lisf of available files"),
                          "Choose the file to load in the sidebar and click on \"Load data\"" ,
                          dataTableOutput("files")
                        ),
                        # second page (session list) ----
                        tabItem(
                          tabName = "sessions",
                          h2("List of observation sessions in data file"),
                          p("To perform an IORA for two observation sessions, choose the sessions to compare in the sidebar and click on the \"Compare\" button in the sidebar. Please note that the comparison is only meaningful for observation sessions of the same subject conducted in parallel by two observers. Moreover, the system assumes that the session started at the same time."),
                          p("If you want to compare more than 2 sessions at the time (group observation sessions), select all the ids of the sessions to compare from the third dropdown menu in the sidebar, and click on the \"Compare all sessions\" button. Please note that beside the assumptions about sessions start/end, all the sessions to compare must have been recorded by a different observer." ),
                          dataTableOutput("sessions")
                        ),
                        # third page, irr dashboard ----
                        tabItem(
                          tabName = "irr",
                          # first row: iteractive plot
                            #fluidRow( # uncomment to remove spate outside the box
                              box(
                                title = "Observed task sequences",
                                width = NULL,
                                collapsible = TRUE,
                                         withSpinner(
                                           plotlyOutput("interactivePlot", width = "98%")
                                         )
                                #)  # uncomment to remove spate outside the box
                              ),
                         # second row (3 columns)
                          fluidRow(
                            column( # first column
                              width = 4,
                              withSpinner(valueBoxOutput("k1", width= NULL), proxy.height = "100px"), # FleissK1
                              valueBoxOutput("agr1", width= NULL),
                              box(
                                title = "Details (task 1)",
                                width = NULL,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = FALSE,
                                status = "primary",
                                tableOutput("kappa1Details")
                              ),
                              valueBoxOutput("k2", width= NULL), # FleissK1
                              valueBoxOutput("agr2", width= NULL),
                              box(
                                title = "Details (task 2)",
                                width = NULL,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE,
                                status = "primary",
                                tableOutput("kappa2Details")
                              ),
                              valueBoxOutput("iota", width= NULL)
                              ),
                            column( # second column
                              width = 4,
                              withSpinner(valueBoxOutput("nK", width= NULL), proxy.height = "100px"), # FleissK on naming
                              valueBoxOutput("agrK", width= NULL),
                              box(
                                title = "Naming agreement",
                                width = NULL,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = FALSE,
                                status = "success",
                                tableOutput("namingDetails")
                              ),
                              valueBoxOutput("seqNW", width= NULL),
                              box(
                                title = "Best aligned sequences",
                                width = NULL,
                                #height = 630,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE,
                                status = "info",
                                plotOutput("alignedSequences", height = "600px")
                              )),
                            column( # third column
                              width = 4,
                              withSpinner(valueBoxOutput("DCCC", width= NULL), proxy.height = "100px"), # DCCC
                              valueBoxOutput("offset", width= NULL),
                              box(
                                title = "Duration",
                                width = NULL,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = FALSE,
                                status = "warning",
                                plotOutput("durationsPlot")
                              ),
                              box(
                                title = "Timing (matched tasks)",
                                width = NULL,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE,
                                status = "warning",
                                plotOutput("timingPlot")
                              ))
                          )#, # end second row
                        ), #)
                        # fourth page (compare 3+ observers) ----
                        tabItem(
                          tabName = "irr_multi",
                            # first row: iteractive plot
                            fluidRow(box(width = NULL,
                                         withSpinner(
                                           plotOutput("interactivePlotMulti", width = "98%", height = "550px")
                                         ))),
                            # second row (3 columns)
                            fluidRow(
                              column( # first column
                                width = 4,
                                withSpinner(valueBoxOutput("k1multi", width= NULL), proxy.height = "100px"), # FleissK1
                                #valueBoxOutput("agr1", width= NULL),
                                box(
                                  title = "Details (task 1)",
                                  width = NULL,
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  collapsed = TRUE,
                                  status = "primary",
                                  tableOutput("kappa1DetailsMulti")
                                )
                              ),
                              column( # second column
                                width = 4,
                                withSpinner(valueBoxOutput("k2multi", width= NULL), proxy.height = "100px"), # FleissK on naming
                                #valueBoxOutput("agrK", width= NULL),
                                box(
                                  title = "Details (task 2)",
                                  width = NULL,
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  collapsed = TRUE,
                                  status = "primary",
                                  tableOutput("kappa2DetailsMulti")
                                )),
                              column( # third column
                                width = 4,
                                withSpinner(valueBoxOutput("iota2", width= NULL), proxy.height = "100px")
                            )#, # end second row
                          )
                          ),
                        # fifth IRR tab (static plot of 2 sessions) ----
                        tabItem(
                          tabName = "staticPlot",
                          #h2("A Plot"),
                          #"Plot"#,
                          box(
                            title = "Comparison of task sequences recorded by observers",
                            width = NULL,
                            solidHeader = TRUE,
                            withSpinner(plotOutput("staticPlot"))
                          )
                        ),
                        # sixth (plot of proportions) ----
                        tabItem(
                          tabName = "plotsProp",
                          fluidRow(
                            box(
                              title = "Proportion of time on different tasks",
                              width = 6,
                              solidHeader = TRUE,
                              withSpinner(plotOutput("propPlot"))
                            ),
                            box(
                              title = "Proportion of time on subcategories",
                              width = 6,
                              solidHeader = TRUE,
                              withSpinner(plotOutput("propPlotSub"))
                            )
                          )
                          # h2("Plots titles"),
                          # "Two plots"
                        ), 
                        # seventh tab (plot irr measures across time) ----
                        tabItem(
                          tabName = "trackProgress",
                          fluidRow( # comment this to add the space outside of the box
                            box(
                              width = NULL,
                              title = "List of saved IORA results",
                              p("Select the session files to track in the dropdown menu in the sidebar, and click on the \"Plot results\" button."),
                              dataTableOutput("results")
                            ) # comment this to add the space outside of the box
                          ),
                          #fluidRow(
                            box(
                              width = NULL,
                              title = "Inter Observers Reliability Measures across time",
                              plotOutput("resultsPlot")
                            )
                          #)
                        )
                      ) # end tabsets
) # end body

# put ui elements together -----
ui <- dashboardPage(
  dashboardHeader(title = "WOMBAT Sessions IORA",
                  titleWidth = 300),
  sidebar,
  body
)


# Define server logic  ----
server <- function(input, output, session) { #
  fullFilenames <- list.files(path = "data/", full.names = FALSE)
  dati_completi <-
    read_csv(paste("data/", fullFilenames[1], sep = ""))
  session_ids <- unique(dati_completi$`session id`)
  
  # saved_IOTA <- loadData()
  
  data <- eventReactive(input$load, {
    read_csv(paste("data/", input$datafile, sep = ""))
  })
  
  prepared_data <- eventReactive(input$compare, {
    prepare_data(data(), c(input$session1, input$session2))
  })
  
  prepared_data_2 <- eventReactive(input$compareMulti, {
    prepare_data_multi(data(), input$session_ids)
  })
  
  durata <- eventReactive(input$compare, {
    get_sessions_maxlenght(data(), c(input$session1, input$session2))
  })
  
  prepared_results <- eventReactive(input$plotSelectedResults, {
    # code to load and reshape the selected results, to use in renderplot
    loadSelectedData(saved_results(), input$results_ids)
    # input$results_ids
  })
  # saved_results <- eventReactive(input$save, {
  #   loadData()
  # })
  
  fit_measures <- reactive({
    list(pK1 = PK1()$stats,
         pAg1 = PK1()$agreement,
         pK1d = PK1()$detail,
         pK2 =  ifelse(is.null(PK2()), NULL, PK2()$stats),
         pAg2 = ifelse(is.null(PK2()), NULL, PK2()$agreement),
         pK2d = ifelse(is.null(PK2()), NULL, PK2()$detail),
         pKm = ifelse(is.null(PKM()), NULL, PKM()$value),
         NK = NK()$stats,
         NAg = NK()$agreement,
         NKd = NK()$detail,
         DCCCr = DCCC()$rho.c$est,
         DCCCcb = DCCC()$C.b,
         NW_score = NW()$score,
         NW_seq = NW()$sequences)
  })
  PK1 <- reactive({
    prepared_data()$wide %>% concordanza_2_task(., 1)
  }) 
  PK2 <- reactive({
    if (max(prepared_data()$wide$n_tasks) > 1) {
      prepared_data()$wide %>% concordanza_2_task(., 2)
    } else {
      #return(NULL)
      list(agreement = NA, stats = NA, detail = NA, contigency_table = NA)
    }
  })   
  # PKmulti <- reactive({
  # # check for multitasking here?
  # if (max(prepared_data()$wide$n_tasks)>1) {
  #   lapply(2:max(prepared_data()$wide$n_tasks), prepared_data()$wide %>% concordanza_2_task(., i))
  # } else {
  #   return(NULL)
  # }})
  PKM <- reactive({
    if (max(prepared_data()$wide$n_tasks)>1) {
      prepared_data()$wide %>% concordanza_multi()
    } else {
      return(list(value = NA))
    }
  })
  NK <- reactive({
    prepared_data()$matched_pairs %>% concordanza_naming()
    })
  DCCC <- reactive({
    prepared_data()$matched_pairs %>% D_CCC()
  })
  NW <- reactive({
     SNW(prepared_data()$long_aggregated, prepared_data()$tasks)
  })

  fit_measures_group <- reactive({
    list(
      pK1 = PK1_group()$stats,
      pK1d = PK1_group()$detail,
      pK2 =  ifelse(is.null(PK2_group()), NULL, PK2_group()$stats),
      pK2d = ifelse(is.null(PK2_group()), NULL, PK2_group()$detail),
      pKm = ifelse(is.null(PKM_group()), NULL, PKM_group()$value)
    )
  })
  PK1_group <- reactive({
    prepared_data_2()$wide %>% concordanza_2_task_multi(., 1)
  }) 
  PK2_group <- reactive({
    if (max(prepared_data_2()$wide$n_tasks) > 1){
      prepared_data_2()$wide %>% concordanza_2_task_multi(., 2)
    } else {
      return(list(stats = NA, detail = NA))
      }
  })
  PKM_group <- reactive({
    if (max(prepared_data_2()$wide$n_tasks) > 1) {
      prepared_data_2()$wide %>% concordanza_multi_group()
    } else {
      return(list(value = NA))
    }
  })
  
  rv <- reactiveValues(sessions = unique(dati_completi$`session id`))
  # fullFilenames <- list.files(path = "../data", full.names = FALSE)
  # dati_completi <- read_csv(paste("data/", fullFilenames[1], sep = ""))
  # session_ids <- unique(dati_completi$`session id`)
  
  # saved_results <- reactiveValues(
  #   iota_id = loadData()$`iota id`
  # )
  
  saved_results <- eventReactive(n_saved_results(), {
    loadData()
  }
    #iota_id = loadData()$`iota id`
  )
  
  n_saved_results <- reactiveVal(nrow(loadData()))
  
  observeEvent(input$load, {
    #dati_completi <- read_csv(paste("data/", input$datafile, sep = ""))
    rv$session_ids <- unique(data()$`session id`)
    #rv$session_ids <- unique(dati_completi$`session id`) # update rv with session for dynamic menu
    updateTabItems(session, "sidebarmenu", selected = "sessions") # change page
  })
  
  observeEvent(input$compare, {
    updateTabItems(session, "sidebarmenu", selected = "irr") # change page
    #shinyjs::hide(selector = "ul.menu-open");
    updateNumericInput(session, "end", value = durata())
  })
  
  observeEvent(input$compareMulti, {
    updateTabItems(session, "sidebarmenu", selected = "irr_multi") # change page
    #shinyjs::hide(selector = "ul.menu-open");
  })
  observeEvent(input$save, {
    data <- as.data.frame(fit_measures()[c(1:2, 4:5, 7:9, 11:13)])
    #print(str(data))
    saveData(data, data(), c(input$session1, input$session2)) 
    #saved_results
    # print(names(fit_measures()))
    n_saved_results(nrow(loadData()))
  })
  
  
  # populate sidebar menus 
  observe({
    updateSelectizeInput(session,
                         'session1',
                         choices = rv$session_ids,
                         server = TRUE)
    updateSelectizeInput(session,
                         'session2',
                         choices = rv$session_ids,
                         server = TRUE)
    updateSelectizeInput(session,
                         'session_ids',
                         choices = rv$session_ids,
                         server = TRUE)
    updateSelectizeInput(session,
                         'results_ids',
                         choices = saved_results()$`iota id`,
                         server = TRUE)
  })
  
  output$files <- renderDataTable({
    file.info(list.files(path = "data", full.names = TRUE)) %>% select(size, mtime) %>% rownames_to_column(., var = "Filename") %>% rename(time = mtime) %>% mutate(size = utils:::format.object_size(size, "auto"), day = as.Date.POSIXct(time), time = as_hms(time)) # %>% as_tibble() %>% View()
    
    # ) #DT::datatable(
  })
  output$sessions <- renderDataTable({
    get_sessions_info_2(data())
  })
  # compute and display IORA measures
  output$k1 <- renderValueBox({ # concordanza_2_task(prepared_data()$wide, 1)$stats
    valueBox(round(fit_measures()$pK1, digits = 2), width= NULL, subtitle = "K on task (1s windows)", color = "light-blue") 
  })
  output$agr1 <- renderValueBox({ # concordanza_2_task(prepared_data()$wide, 1)$agreement
    valueBox(paste0(round(fit_measures()$pAg1, digits = 1), "%"), width= NULL, subtitle = "% Agreement on task 1", color = "light-blue") 
  })
  output$k2 <- renderValueBox({ # concordanza_2_task(prepared_data()$wide, 2)$stats
    valueBox(round(fit_measures()$pK2, digits = 2), width= NULL, subtitle = "K on second task (1s windows)", color = "light-blue") 
  })
  output$agr2 <- renderValueBox({ # concordanza_2_task(prepared_data()$wide, 2)$agreement
    valueBox(paste0(round(fit_measures()$pAg2, digits = 1), "%"), width= NULL, subtitle = "% Agreement on second task", color = "light-blue") 
  })
  output$iota <- renderValueBox({ # concordanza_multi(prepared_data()$wide)$value
    valueBox(round(fit_measures()$pKm, digits = 2), width= NULL, subtitle = "Multivariate IOTA", color = "light-blue")
  })
  output$nK <- renderValueBox({ # concordanza_naming(prepared_data()$matched_pairs)$stats
    valueBox(round(fit_measures()$NK, digits = 2), width= NULL, subtitle = "K on naming (matched tasks)", color = "green")
  })
  output$agrK <- renderValueBox({ # concordanza_naming(prepared_data()$matched_pairs)$agreement
    valueBox(paste0(round(fit_measures()$NAg, digits = 1), "%"), width= NULL, subtitle = "% Agreement on naming", color = "green")
  })
  output$DCCC <- renderValueBox({ # D_CCC(prepared_data()$matched_pairs)$rho.c$est
    valueBox(round(fit_measures()$DCCCr, digits = 2), width= NULL, subtitle = "Duration agreement D-CCC (matched tasks)", color = "yellow")
  })
  output$offset <- renderValueBox({ # D_CCC(prepared_data()$matched_pairs)$C.b
    valueBox(round(fit_measures()$DCCCcb, digits = 2), width= NULL, subtitle = "Offset", color = "yellow")
  })
  output$durationsPlot <- renderPlot({
    plot_D_CCC(prepared_data()$matched_pairs, 
               prepared_data()$tasks)
  })
  output$timingPlot <- renderPlot({
    plot_timing(prepared_data()$matched_pairs, 
                prepared_data()$tasks)
  })
  output$interactivePlot <- renderPlotly({
    labels_plot <- str_c(prepared_data()$long_aggregated$Cosa,
                         ifelse(prepared_data()$long_aggregated$`Cosa (subcategories)` != "0", 
                                prepared_data()$long_aggregated$task_details, ""),
                         ifelse(prepared_data()$long_aggregated$Dove != "0", 
                                prepared_data()$long_aggregated$Dove, ""), sep = "\n")
    observers <- unique(prepared_data()$long_aggregated$obs_id)
    set1 <- which(prepared_data()$long_aggregated$obs_id == observers[1] & prepared_data()$long_aggregated$interruzione == 0)
    set2 <- which(prepared_data()$long_aggregated$obs_id == observers[2] & prepared_data()$long_aggregated$interruzione == 0)
    set3 <- which(prepared_data()$long_aggregated$obs_id == observers[1] & prepared_data()$long_aggregated$interruzione == 1)
    set4 <- which(prepared_data()$long_aggregated$obs_id == observers[2] & prepared_data()$long_aggregated$interruzione == 1)
    
    
    static_plot <- plot_sequences_rect_ok(prepared_data()$long_aggregated, 
                                          prepared_data()$tasks, 
                                          show.labels = FALSE)  + theme(legend.position='none')
    interactive_plot <- ggplotly(static_plot)
    # stype ggplotly object to add hover info about tasks/subtasks and locations
    n_tracce <- length(interactive_plot$x$data)
    step1 <- style(interactive_plot, hoverinfo = "none", traces = 1:(n_tracce - 4))
    # step2 <- style(step1, text=labels_plot, hoverinfo = "text", traces = 15:18)
    # step2
    step2 <- style(step1, text=rep(labels_plot[set1], each = 3), hoverinfo = "text", traces = n_tracce - 3) # 
    step3 <- style(step2, text=rep(labels_plot[set2], each = 3), hoverinfo = "text", traces = n_tracce - 2)
    step4 <- style(step3, text=rep(labels_plot[set3], each = 3), hoverinfo = "text", traces = n_tracce - 1)
    step5 <- style(step4, text=rep(labels_plot[set4], each = 3), hoverinfo = "text", traces = n_tracce)
    step5
  })
  # proportion kappa for task 1 details
  output$kappa1Details <- renderUI({
    kappa_detail <- fit_measures()$pK1d #concordanza_2_task(prepared_data()$wide, 1)$detail
    #print(kappa_detail) this is a tibble, ok
    kappa_detail <- kappa_detail %>% left_join(prepared_data()$tasks)
    format_details_table(kappa_detail)
  }) 
  # proportion kappa for task 2 details
  output$kappa2Details <- renderUI({
    if (is.na(fit_measures$pK2d))
      return(NULL)
    #kappa_detail <- fit_measures()$pK2d # concordanza_2_task(prepared_data()$wide, 2)$detail
    kappa_detail <- concordanza_2_task(prepared_data()$wide, 2)$detail
    # print(kappa_detail) # why a list? this is the source of the error in the next line
    kappa_detail <- kappa_detail %>% left_join(prepared_data()$tasks)
    format_details_table(kappa_detail)
  }) 
  # naming kappa details table
  output$namingDetails <- renderUI({
    kappa_detail <- fit_measures()$NKd # concordanza_naming(prepared_data()$matched_pairs)$detail
    kappa_detail <- kappa_detail %>% left_join(prepared_data()$tasks)
    format_details_table(kappa_detail)
  }) 
  # sequence similarity
  output$seqNW <- renderValueBox({ # SNW(prepared_data()$long_aggregated, prepared_data()$tasks)$score
    valueBox(round(fit_measures()$NW_score, digits = 2), width= NULL, subtitle = "N-W sequence similarity score", color = "aqua")
  })
  # plot best aligned sequences
  output$alignedSequences <- renderPlot({ # SNW(prepared_data()$long_aggregated, prepared_data()$tasks)$sequences
    plot_aligned_sequences(fit_measures()$NW_seq, prepared_data()$tasks)
  },
  height = 600)
  # raw proportions plots
  output$propPlot <- renderPlot({
    plot_prop_time_on_tasks(data(), input$session1, input$session2)
  })
  output$propPlotSub <- renderPlot({
    plot_prop_time_on_tasks_sub(data(), input$session1, input$session2)
  })
  # measure for group sessions
  output$k1multi <- renderValueBox({ 
    valueBox(round(fit_measures_group()$pK1, digits = 2), width= NULL, subtitle = "K on task (1s windows)", color = "light-blue") 
  })
  output$k2multi <- renderValueBox({ 
    valueBox(round(fit_measures_group()$pK2, digits = 2), width= NULL, subtitle = "K on second task (1s windows)", color = "light-blue") 
  })
  output$iota2 <- renderValueBox({ 
    valueBox(round(fit_measures_group()$pKm, digits = 2), width= NULL, subtitle = "Multivariate IOTA", color = "light-blue")
  })
  output$kappa1DetailsMulti <- renderUI({
    kappa_detail <- fit_measures_group()$pK1d 
    kappa_detail <- kappa_detail %>% left_join(prepared_data_2()$tasks)
    format_details_table(kappa_detail)
  }) 
  output$kappa2DetailsMulti <- renderUI({
    # kappa_detail <- fit_measures_group()$pK2d 
    kappa_detail <- concordanza_2_task_multi(prepared_data_2()$wide, 2)$detail
    kappa_detail <- kappa_detail %>% left_join(prepared_data_2()$tasks)
    format_details_table(kappa_detail)
  }) 
  # plot group sessions
  output$interactivePlotMulti <- renderPlot({
    plot_time_windows(prepared_data_2()$long)
  })
  # static plots paired sequences
  output$staticPlot <- renderPlot({
    plot_time_windows(prepared_data()$long, start_sec = input$start, end_sec = input$end, axis_unit = input$units, show.labels = input$labs)
  })
  # saved results table anbd plot
  output$results <- renderDataTable({
    #loadData()
    saved_results()
    # %>% 
  })
  output$resultsPlot <- renderPlot({
    prepared_results() %>% 
      plot_IOTA_measures()
  })
  # session info box in sidebar 
  output$sessionsInfo <- renderUI({
    
    # tabella1 <- get_selected_sessions_info_common(data(), c(input$session1, input$session2))
    # tabella2 <- get_selected_sessions_info_2(prepared_data()$durations) %>% 
    #   select(n_task:n_interruptions, int_rate, time_multitasking, prop_multi) %>%
    #   mutate(prop_multi = round(prop_multi*100, digits = 1)) %>%
    #   t() %>% 
    #   as.data.frame() %>% 
    #   rownames_to_column()
    # format_summary_table(tabella1, tabella2)
  })
  
}


# Run the application ----
shinyApp(ui = ui, server = server)