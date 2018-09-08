library(shinydashboard)
library(shiny)
library(tidyverse)
library(hms)
library(irr)
library(sjPlot) # non obbligatorio per ora
library(plotly) # per plot interattivi
library(htmlTable)
library(listviewer) # to debug plotly
library(DescTools)

# Define functions used by the app ----
get_sessions_info_2 <- function(df) {
  df %>% group_by(`session id`) %>%
    summarise(
      observer_id = first(`observer id`),
      participant = first(`participant id`),
      setting = first(`location id`),
      date = first(`session date`),
      start = first(`session start`),
      end = first(`session end`),
      duration = as.hms(first(`session end`) - first(`session start`)),
      n_task_recorded = n(),
      n_interruptions = sum(`num of interrupts`)
    ) #%>%
  #htmlTable(header = c("Session ID", "Obs. ID", "Participant ID", "Setting ID", "Date", "Start", "End", "Duration", "Tasks", "Interruptions"), rnames = FALSE, caption = "Summary information about all the observation sessions conducted during the study")
}

# list available data files

# auxiliary function append_rows_df() used inside create_long_time_windows() to create sequence of time windows ----
append_rows_df <- function(df, df2, i, ultimo, primo, fragment){
  n_finestre <- ultimo - primo + 1
  #print(str_c("finestra:", n_finestre, "primo:", primo, "ultimo:", ultimo, sep = " "))
  df2 <- rbind(df2, data.frame("wind_id" = primo:ultimo,
                               "obs_id" = rep(df[i, ]$`observer id`, times = n_finestre),
                               "task_id" = rep(df[i, ]$`observer/session/task id`, times = n_finestre),
                               "task" = rep(df[i, ]$Cosa, times = n_finestre),
                               "fragment" = rep(fragment, times = n_finestre)))
  return(df2)
}

# function create_long_time_windows() to divide data into 1 second time windows (long with multiple instances of the time windows in which there was multitasking) with df assumed to be aligned (i.g. that started simultaneously). It depends on append_rows_df() defined before ----
# input: df in long form with data from (at least) two parallel sessions (assumed to have started at the same time), IDs of the sessions to compare
# output: df in long format, including info on windows_id, obs_id, task_id, task, fragment
create_long_time_windows <- function(df, session_1_id, session_2_id){
  # filter dfs
  df <- df %>% filter(`session id` %in% c(session_1_id, session_2_id))
  # compute 2 columns representing the start/end time of the task in seconds, from the beginning of the session
  df$inizio <- df$`start time` - df$`session start`
  df$fine <- df$`end time` - df$`session start` 
  # trim observations beyond first ended session
  max_time <- min(as.numeric(df$`session end` - df$`session start`))
  
  # start the loop to create the restructured df
  df_new <- data.frame()
  for (i in 1:nrow(df)){
    primo <- as.numeric(df[i, ]$inizio) # + 1 #
    
    # leggo se il task è stato interrotto, 
    if (df[i, ]$`num of interrupts` == 0){ 
      # no
      fragment = 1
      ultimo <- as.numeric(df[i, ]$fine) - 1 # the last do not count
      df_new <- append_rows_df(df, df_new, i, ultimo, primo, fragment)
    } else { 
      # si task è stato interrotto, per prima cosa devo vedere quante volte 
      interruzioni <- df[i, ]$`num of interrupts`
      
      # calcolo la prima sequenza fino alla prima pausa
      ultimo <- as.numeric(df[i, ]$`int start 1`) - as.numeric(df[i, ]$`session start`) - 1 # the last do not count
      fragment = 1
      df_new <- append_rows_df(df, df_new, i, ultimo, primo, fragment)
      
      # calcolo le sequenze successive
      for (pausa in seq_len(interruzioni)){
        primo <- as.numeric(df[i, str_c("int end", pausa, sep = " ")]) - as.numeric(df[i, ]$`session start`)
        if (pausa < interruzioni) { # non è l'ultima 
          ultimo <- as.numeric(df[i, str_c("int start", pausa + 1, sep = " ")]) - as.numeric(df[i, ]$`session start`) - 1
          df_new <- append_rows_df(df, df_new, i, ultimo, primo, pausa + 1)
        } else { # ultima interruzione
          ultimo <- as.numeric(df[i, ]$fine) - 1
          df_new <- append_rows_df(df, df_new, i, ultimo, primo, pausa + 1)
        }
      }
    }
  } 
  df_new <- df_new %>% filter(wind_id < max_time)
  return(df_new)
}

# funzone flat_tasks_df() per appiattire il df con la sequenza delle finestre di 1 secondo creato con la funzione create_long_time_windows(), così da avere per ogni secondo uno o più task ----
# NOTE: quite slow. it could be fixed by avoiding loops 2 DO
# input: df in formato long
# output: df in formato flat, con n colonne variabile in base al massimo numero di task simultanei
flat_tasks_df <- function(df){
  #n_finestre <- max(df$wind_id)
  flat_df <- data.frame()
  for (obs in unique(df$obs_id)) {
    for (finestra in unique(df$wind_id)){
      # conto i task osservati in quella finestra
      n_task_simultanei <- nrow(df[df$wind_id == finestra & df$obs_id == obs, ])
      if (n_task_simultanei == 1){
        flat_df <- rbind(flat_df, data.frame("wind_id" = finestra, 
                                             "obs_id" = obs,
                                             "n_tasks" = n_task_simultanei, 
                                             "tasks" = df[df$wind_id == finestra & df$obs_id == obs, ]$task,
                                             "task_ids" = df[df$wind_id == finestra & df$obs_id == obs, ]$task_id))
      } else {
        task_simultanei <- str_c(df[df$wind_id == finestra & df$obs_id == obs, ]$task, collapse = ", ")
        id_task_simultanei <- str_c(df[df$wind_id == finestra & df$obs_id == obs, ]$task_id, collapse = ", ")
        flat_df <- rbind(flat_df, data.frame("wind_id" = finestra, 
                                             "obs_id" = obs,
                                             "n_tasks" = n_task_simultanei, 
                                             "tasks" = task_simultanei,
                                             "task_ids" = id_task_simultanei))
      }
    }
  }
  
  flat_df <- separate(flat_df, tasks, str_c("task", seq_len(max(flat_df$n_tasks)), sep = "_"), remove = FALSE, fill = "right", sep = ",")
  flat_df <- separate(flat_df, task_ids, str_c("id_task", seq_len(max(flat_df$n_tasks)), sep = "_"), remove = FALSE, fill = "right", sep = ",")
  return(flat_df)
}

# functions add_track_n() to add "track" info on a long df, in order to use with plot_sequences_3() function to position labels and vertical bars dividing tasks ----
# NOTE:
# 2 versions, the second is slower
# output is a df long with an extra column named track. 
# version A: long and flat dfs as input
add_track_n <- function(df_long, df_flat) {
  df_long_temp <- df_flat %>% select(wind_id, obs_id, contains("id_task"))
  colonne <- ncol(df_long_temp)
  df_long_temp <- df_long_temp %>% gather("track", "task_id", 3:colonne) %>% filter(!is.na(task_id)) %>% mutate(track = str_replace(track, "id_task_", "")) %>% mutate(task_id = str_trim(task_id), track = as.numeric(track))
  df_long <- df_long %>% left_join(df_long_temp, by = c("wind_id", "obs_id", "task_id"))
  return(df_long)
}
# version B: long df as input only
add_track_n_2 <- function(df_long) {
  df_long_temp <- df_long %>% flat_tasks_df() %>% select(wind_id, obs_id, contains("id_task"))
  colonne <- ncol(df_long_temp)
  df_long_temp <- df_long_temp %>% gather("track", "task_id", 3:colonne) %>% filter(!is.na(task_id)) %>% mutate(track = str_replace(track, "id_task_", "")) %>% mutate(task_id = str_trim(task_id), track = as.numeric(track))
  df_long <- df_long %>% left_join(df_long_temp, by = c("wind_id", "obs_id", "task_id"))
  return(df_long)
}

# function add_subcats_info() to augment further the df with additional info on the tasks ----
add_subcats_info <- function(df, raw_df){
  raw_df <- raw_df %>% 
    select(task_id = `observer/session/task id`, 9:10, Dove) %>% 
    mutate(task_details = if_else(`Cosa (subcategories)`=="0", 
                                  Cosa, 
                                  `Cosa (subcategories)`))
  df <-  left_join(df, raw_df)
  return(df)
}

# function add_extra_info ----
add_extra_info <- function(df) {
  df %>% 
    group_by(obs_id, task_id, fragment) %>% 
    summarise(midpoint = (first(wind_id) + last(wind_id))/2, 
              task_start = first(wind_id), 
              task_end = last(wind_id), 
              track = first(track),
              task_duration = task_end - task_start) %>% 
    mutate(interruzione = if_else(max(fragment) - fragment > 0, 1, 0),
           midpoint = if_else(midpoint < start_sec, 
                              (start_sec + task_end - 0.5)/2, 
                              if_else(midpoint > end_sec, 
                                      (task_start + end_sec)/2,
                                      midpoint)))  %>% 
    separate(task_id, sep = "_", remove = FALSE, into = c("obs","sess", "task_id_label")) %>%
    select(-obs, -sess)
} 


# function to prepare all data for IRR ----
prepare_data <- function(df_raw, sess_1id, sess_2id){
  long <- create_long_time_windows(df_raw, sess_1id, sess_2id)
  wide <- long %>% flat_tasks_df()
  long <- add_track_n(long, wide)
  # durations <- add_subcats_info(add_extra_info(long), df_raw)
  # task_table <- prepare_palette(long)
  out <- list(long = long, 
              wide = wide #, 
              # durations = durations, 
              # matched_pairs = matched_pairs, 
              # tasks = task_table
              )
}

# function to compute kappa from wide form data ----
concordanza_2_task <- function(df_flat, task_n){
  osservatori <- unique(df_flat$obs_id) # dovrebbero essere solo 2
  colonna <- 4 + task_n
  
  # task 1
  task_obs1 <- str_trim(df_flat[df_flat$obs_id == osservatori[1], colonna])
  task_obs2 <- str_trim(df_flat[df_flat$obs_id == osservatori[2], colonna])
  if (task_n > 1){
    task_obs1 <- ifelse(is.na(task_obs1), " NONE", task_obs1)
    task_obs2 <- ifelse(is.na(task_obs2), " NONE", task_obs2)
  } # recode missing a category for secondary tasks
  kappa_t1 <- kappam.fleiss(cbind(task_obs1, task_obs2), detail = TRUE) # Fleiss' kappa on task
  
  # percent agreement table
  livelli <- sort(union(unique(task_obs1), unique(task_obs2)))
  #livelli <- str_trim(livelli)
  print(livelli)
  a <- factor(task_obs1, levels = livelli)
  b <- factor(task_obs2, levels = livelli)
  contingenze_task <- table(a, b)
  perc_agreement <- sum(apply((contingenze_task * diag(nrow = length(livelli))), 1, "sum"))*100/sum(apply(contingenze_task, 1, "sum"))
  cat("\nAgreement on task ", task_n, ": ", round(perc_agreement, digits = 1), "%\n", sep = "")
  print(kappa_t1)
  print(contingenze_task)
  #return(kappa_t1)
  out <- list(agreement = perc_agreement, 
              stats = kappa_t1$value)
  return(out)
  # store kappa_t1$value kappa_t1$details
}

# tests -----
# dati_to_plot_compare_new_long <- create_long_time_windows(dati_prova_irr_2, 54, 60) %>% flat_tasks_df() # Giulio e Ste
# dati_to_plot_compare_new_long_final <- add_track_n(dati_to_plot_compare_new_long, dati_to_plot_compare_new_flat) 
# dati_to_plot_compare_new_long_fm_final <- add_track_n_2(dati_to_plot_compare_new_long_fm) # slower,
# View(add_subcats_info(dati_prova_extended, dati_prova_irr_2))
# add_extra_info(dati_to_plot_compare_new_long_final) %>% 
#   add_subcats_info(., dati_prova_irr_2) %>% 
#   View()

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
                              valueBoxOutput("k1", width= NULL), # FleissK1
                              valueBoxOutput("agr1", width= NULL),
                              box(
                                title = "Details (task 1)",
                                width = NULL,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = FALSE,
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
                              valueBox(width= NULL, "0.89", subtitle = "K on naming (matched tasks)", color = "green"),
                              valueBox(width= NULL, "92.2%", subtitle = "% Agreement on naming", color = "green"),
                              box(
                                title = "Naming agreement",
                                width = NULL,
                                solidHeader = TRUE,
                                status = "success",
                                plotOutput("numVehiclesTable3")
                              )),
                            column( # third column
                              width = 4,
                              valueBox(width= NULL, "x", subtitle = "Duration agreement D-CCC (matched tasks)", color = "yellow"),
                              valueBox(width= NULL, "1.7", subtitle = "Offset", color = "yellow"),
                              box(
                                title = "Duration",
                                width = NULL,
                                solidHeader = TRUE,
                                status = "warning",
                                plotOutput("numVehiclesTable4")
                              ),
                              box(
                                title = "Timing (matched tasks)",
                                width = NULL,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = FALSE,
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
  # output$interactivePlot <- renderPlotly({
  #   static_plot <- plot_sequences_rect(dati_to_plot_compare_new_long_fm_final, 0, 1800) + theme(legend.position='none')
  #   ggplotly(static_plot, tooltip = c("task", "task_start","task_id")) 
  # })
}


# Run the application ----
shinyApp(ui = ui, server = server)