library(shinydashboard)
library(shiny)
library(shinycssloaders)
#library(shinyjs)

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
    menuItem("Inter Raters Reliability", 
             #tabName = "irr",
             menuSubItem("Compare sessions", tabName = "irr"), #, selected = FALSE
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
        actionButton("compare", "Compare sessions"),
        actionButton("save", "Save results")
      )
    ),
    conditionalPanel(
      "input.sidebarmenu == 'irr' || input.sidebarmenu == 'staticPlot' || input.sidebarmenu == 'plotsProp",
      div(
        p("Brief information on selected sessions...")
      )
    )
  )
)

# body ----
body <- dashboardBody(tags$style(".small-box {height: 20; width: 150; }"),
                      
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
                          p("Choose the sessions to compare in the sidebar and click on the \"Compare\" button in the sidebar. Please note that the comparison is only meaningful for observation sessions of the same subject conducted in parallel by two observers. Moreover, the system assumes that the session started at the same time."),
                          dataTableOutput("sessions")
                        ),
                        # third page, irr dashboard ----
                        tabItem(
                          tabName = "irr",
                          # first row
                          fluidRow(
                          # first row (3 columns)
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
                          ), # end second row
                          # third row: iteractive plot
                          fluidRow(
                            box(width = NULL,
                                withSpinner(plotlyOutput("interactivePlot", width = "98%"))))
                          # end third row
                        )), 
                        # second IRR tab (static plot of 2 sessions)
                        tabItem(
                          tabName = "staticPlot",
                          h2("A Plot"),
                          "Plot"#,
                          #dataTableOutput("files") 
                        ),
                        # fourthpage (plot of proportions) ----
                        tabItem(
                          tabName = "plotsProp",
                          fluidRow(
                            box(
                              title = "Proportion of time on different tasks",
                              width = 6,
                              solidHeader = TRUE,
                              plotOutput("propPlot")
                            ),
                            box(
                              title = "Proportion of time on different tasks",
                              width = 6,
                              solidHeader = TRUE,
                              plotOutput("propPlotSub")
                            )
                          )
                          # h2("Plots titles"),
                          # "Two plots"
                        ),
                        tabItem(
                          tabName = "trackProgress",
                          h2("Plots IRR over time"),
                          "A few plots..."
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
  
  data <- eventReactive(input$load, {
    read_csv(paste("data/", input$datafile, sep = ""))
  })
  
  prepared_data <- eventReactive(input$compare, {
    prepare_data(data(), input$session1, input$session2)
  })
  
  rv <- reactiveValues(sessions = unique(dati_completi$`session id`))
  # fullFilenames <- list.files(path = "../data", full.names = FALSE)
  # dati_completi <- read_csv(paste("data/", fullFilenames[1], sep = ""))
  # session_ids <- unique(dati_completi$`session id`)
  
  observeEvent(input$load, {
    #dati_completi <- read_csv(paste("data/", input$datafile, sep = ""))
    rv$session_ids <- unique(data()$`session id`)
    #rv$session_ids <- unique(dati_completi$`session id`) # update rv with session for dynamic menu
    updateTabItems(session, "sidebarmenu", selected = "sessions") # change page
  })
  
  observeEvent(input$compare, {
    updateTabItems(session, "sidebarmenu", selected = "irr") # change page
    #shinyjs::hide(selector = "ul.menu-open");
  })
  
  # load sessions
  observe({
    updateSelectizeInput(session,
                         'session1',
                         choices = rv$session_ids,
                         server = TRUE)
    updateSelectizeInput(session,
                         'session2',
                         choices = rv$session_ids,
                         server = TRUE)
  })
  
  output$files <- renderDataTable({
    file.info(list.files(path = "data/", full.names = TRUE)) %>% select(size, mtime:atime, uname) %>% rownames_to_column(., var = "Filename")
    # ) #DT::datatable(
  })
  output$sessions <- renderDataTable({
    get_sessions_info_2(data())
  })
  # compute and display IORA measures
  output$k1 <- renderValueBox({
    valueBox(round(concordanza_2_task(prepared_data()$wide, 1)$stats, digits = 2), width= NULL, subtitle = "K on task (1s windows)", color = "light-blue") 
  })
  output$agr1 <- renderValueBox({
    valueBox(paste0(round(concordanza_2_task(prepared_data()$wide, 1)$agreement, digits = 1), "%"), width= NULL, subtitle = "% Agreement on task 1", color = "light-blue") 
  })
  output$k2 <- renderValueBox({
    valueBox(round(concordanza_2_task(prepared_data()$wide, 2)$stats, digits = 2), width= NULL, subtitle = "K on second task (1s windows)", color = "light-blue") 
  })
  output$agr2 <- renderValueBox({
    valueBox(paste0(round(concordanza_2_task(prepared_data()$wide, 2)$agreement, digits = 1), "%"), width= NULL, subtitle = "% Agreement on second task", color = "light-blue") 
  })
  output$iota <- renderValueBox({
    valueBox(round(concordanza_multi(prepared_data()$wide)$value, digits = 2), width= NULL, subtitle = "Multivariate IOTA", color = "light-blue")
  })
  output$nK <- renderValueBox({
    valueBox(round(concordanza_naming(prepared_data()$matched_pairs)$stats, digits = 2), width= NULL, subtitle = "K on naming (matched tasks)", color = "green")
  })
  output$agrK <- renderValueBox({
    valueBox(paste0(round(concordanza_naming(prepared_data()$matched_pairs)$agreement, digits = 1), "%"), width= NULL, subtitle = "% Agreement on naming", color = "green")
  })
  output$DCCC <- renderValueBox({
    valueBox(round(D_CCC(prepared_data()$matched_pairs)$rho.c$est, digits = 2), width= NULL, subtitle = "Duration agreement D-CCC (matched tasks)", color = "yellow")
  })
  output$offset <- renderValueBox({
    valueBox(round(D_CCC(prepared_data()$matched_pairs)$C.b, digits = 2), width= NULL, subtitle = "Offset", color = "yellow")
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
    kappa_detail <- concordanza_2_task(prepared_data()$wide, 1)$detail
    kappa_detail <- kappa_detail %>% left_join(prepared_data()$tasks)
    format_details_table(kappa_detail)
  }) 
  # proportion kappa for task 2 details
  output$kappa2Details <- renderUI({
    kappa_detail <- concordanza_2_task(prepared_data()$wide, 2)$detail
    kappa_detail <- kappa_detail %>% left_join(prepared_data()$tasks)
    format_details_table(kappa_detail)
  }) 
  # naming kappa details table
  output$namingDetails <- renderUI({
    kappa_detail <- concordanza_naming(prepared_data()$matched_pairs)$detail
    kappa_detail <- kappa_detail %>% left_join(prepared_data()$tasks)
    format_details_table(kappa_detail)
  }) 
  # sequence similarity
  output$seqNW <- renderValueBox({
    valueBox(round(SNW(prepared_data()$long_aggregated, prepared_data()$tasks)$score, digits = 2), width= NULL, subtitle = "N-W sequence similarity score", color = "aqua")
  })
  # plot best aligned sequences
  output$alignedSequences <- renderPlot({
    plot_aligned_sequences(SNW(prepared_data()$long_aggregated, prepared_data()$tasks)$sequences, prepared_data()$tasks)
  },
  height = 600)
  # raw proportions plots
  output$propPlot <- renderPlot({
    plot_prop_time_on_tasks(data(), input$session1, input$session2)
  })
  output$propPlotSub <- renderPlot({
    plot_prop_time_on_tasks_sub(data(), input$session1, input$session2)
  })
}


# Run the application ----
shinyApp(ui = ui, server = server)