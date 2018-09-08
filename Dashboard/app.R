library(shinydashboard)
library(shiny)
library(shinycssloaders)

source("helpers.R")

# Define UI for application ----
sidebar <- dashboardSidebar(
  # sidebar ----
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
                              withSpinner(valueBoxOutput("k1", width= NULL)), # FleissK1
                              valueBoxOutput("agr1", width= NULL),
                              box(
                                title = "Details (task 1)",
                                width = NULL,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE,
                                status = "primary",
                                plotOutput("numVehiclesTable1")
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
                                plotOutput("numVehiclesTable2")
                              ),
                              valueBox(width= NULL, "0.73", subtitle = "Multivariate IOTA", color = "light-blue")
                              ),
                            column( # second column
                              width = 4,
                              valueBoxOutput("nK", width= NULL), # FleissK on naming
                              valueBoxOutput("agrK", width= NULL),
                              box(
                                title = "Naming agreement",
                                width = NULL,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE,
                                status = "success",
                                plotOutput("numVehiclesTable3")
                              )),
                            column( # third column
                              width = 4,
                              valueBoxOutput("DCCC", width= NULL), # DCCC
                              valueBoxOutput("offset", width= NULL),
                              box(
                                title = "Duration",
                                width = NULL,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE,
                                status = "warning",
                                plotOutput("numVehiclesTable4")
                              ),
                              box(
                                title = "Timing (matched tasks)",
                                width = NULL,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE,
                                status = "warning",
                                plotOutput("numVehiclesTable5")
                              ))
                          ), # end second row
                          # third row: iteractive plot
                          fluidRow(
                            box(width = NULL,
                                plotOutput("interactivePlot")))
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
                          h2("Plots titles"),
                          "Two plots"
                        ),
                        tabItem(
                          tabName = "trackProgress",
                          h2("Plots IRR over time"),
                          "A few plots..."
                        )
                      ) # end tabsets
) # end body

ui <- dashboardPage(
  dashboardHeader(title = "WOMBAT Sessions IRR Assessment"),
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
    data_frame(
      id = 1:length(list.files(
        path = "../data", full.names = FALSE
      )),
      filename = list.files(path = "../data", full.names = FALSE)
    ) #DT::datatable(
  })
  output$sessions <- renderDataTable({
    get_sessions_info_2(data())
  })
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
  # output$interactivePlot <- renderPlotly({
  #   static_plot <- plot_sequences_rect(dati_to_plot_compare_new_long_fm_final, 0, 1800) + theme(legend.position='none')
  #   ggplotly(static_plot, tooltip = c("task", "task_start","task_id")) 
  # })
}


# Run the application ----
shinyApp(ui = ui, server = server)