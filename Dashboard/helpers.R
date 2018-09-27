library(tidyverse)
library(hms)
library(lubridate)
library(irr)
library(sjPlot) # non obbligatorio per ora
library(plotly) # per plot interattivi
library(htmlTable)
library(listviewer) # to debug plotly
library(DescTools)
library(scales)
library(NameNeedle)
library(grid)
library(gridExtra)

# color palette hex ----
# final palette:

# PK (light-blue): "#428bca" (very close but not identical, alternative is "steelblue", variante scura: "dodgerblue4")
# NK (green): "#28a745"  (second best: "00c851")
# D-CCC (orange):  "orange"
# NW (aqua): "deepskyblue" "#00BFFF"

# show_col(c("#428bca", "#28a745", "orange", "deepskyblue"))

# set parameters for sequence similarity matrix/score computation (NW algorithm)
defaultNeedleParams$MATCH <- 3
defaultNeedleParams$MISMATCH <- 0

# functions to extract sessions info to display in table in sessions tab and in sidebar for irr tab ----
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

get_sessions_maxlenght <- function(df, sessions) {
  d1 <- df %>% filter(`session id` %in% sessions) %>% 
    group_by(`session id`) %>%
    summarise(
      start = first(`session start`),
      end = first(`session end`),
      duration = as.numeric(first(`session end`) - first(`session start`))
    ) %>% 
    summarise(max(duration))
  as.numeric(d1)
}
#get_sessions_maxlenght(dati_prova_irr_2, c(54, 60))


# Functions to plot proportions of time on various task (ignore time ordered nature of data) ----
# main categories only
plot_prop_time_on_tasks <- function(df, session_1_id, session_2_id) {
  df %>%
    filter(`session id` %in% c(session_1_id, session_2_id)) %>%
    mutate(sess_duration = `session end`- `session start`) %>% 
    group_by(`observer id`, Cosa) %>%
    summarise(
      tot_time_on_act = sum(`total time`),
      session_duration = hms::as.hms(first(sess_duration)),
      pro_time_on_act = as.numeric(tot_time_on_act) * 100 /
        as.numeric(first(sess_duration)),
      n_obsd_tasks = n()
    ) %>%
    group_by(`observer id`) %>%
    mutate(tot_n_tasks = sum(n_obsd_tasks),
           prop_task = n_obsd_tasks * 100 / tot_n_tasks) %>% 
    ggplot( aes(x = Cosa, y = pro_time_on_act, alpha = factor(`observer id`), fill = Cosa)) +
    geom_bar(stat = "identity", position = position_dodge()) + 
    scale_alpha_manual(values = c(1,.75), guide = guide_legend(title = "Obs ID", reverse = TRUE)) +
    geom_text(aes(label = paste(round(pro_time_on_act, digits = 1), "%", sep = " ")), position = position_dodge(.9), size = 3, hjust = -0.2) + 
    scale_y_continuous(limits = c(0,55), breaks = seq(0,50, by = 10)) +
    ylab("Percentuale del tempo sul task") +
    xlab("Tipo di attività (categoria)" ) +
    guides(fill = "none") + 
    coord_flip()
}

# main categiories and subcategories
plot_prop_time_on_tasks_sub <- function(df, session_1_id, session_2_id) {
  df %>%
    filter(`session id` %in% c(session_1_id, session_2_id)) %>%
    mutate(sess_duration = `session end`- `session start`, sottocategoria = if_else(`Cosa (subcategories)`==0, Cosa, str_c(Cosa, `Cosa (subcategories)`, sep = ": "))) %>% 
    group_by(`observer id`, Cosa, sottocategoria) %>%
    summarise(
      tot_time_on_act = sum(`total time`),
      session_duration = hms::as.hms(first(sess_duration)),
      pro_time_on_act = as.numeric(tot_time_on_act) * 100 /
        as.numeric(first(sess_duration)),
      n_obsd_tasks = n()
    ) %>%
    group_by(`observer id`) %>%
    mutate(tot_n_tasks = sum(n_obsd_tasks),
           prop_task = n_obsd_tasks * 100 / tot_n_tasks) %>% 
    ggplot( aes(x = sottocategoria, y = pro_time_on_act, alpha = factor(`observer id`), fill = Cosa)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_alpha_manual(values = c(1,.75), guide = guide_legend(title = "Obs ID", reverse = TRUE)) +
    guides(fill = "none") +
    geom_text(aes(label = paste(round(pro_time_on_act, digits = 1), "%", sep = " ")), position = position_dodge(.9), size = 3, hjust = -0.2) +
    scale_y_continuous(limits = c(0,55), breaks = seq(0,50, by = 10)) +
    ylab("Percentuale del tempo sul task") +
    xlab("Tipo di attività (categoria/sottocategoria)" ) +
    coord_flip()
}

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
create_long_time_windows <- function(df, sessions){
  # filter dfs
  df <- df %>% filter(`session id` %in% sessions)
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
    mutate(interruzione = if_else(max(fragment) - fragment > 0, 1, 0)) %>%  #,
           # midpoint = if_else(midpoint < start_sec, 
           #                    (start_sec + task_end - 0.5)/2, 
           #                    if_else(midpoint > end_sec, 
           #                            (task_start + end_sec)/2,
           #                            midpoint)))  %>% 
    separate(task_id, sep = "_", remove = FALSE, into = c("obs","sess", "task_id_label")) %>%
    select(-obs, -sess)
} 

# lookup table for task colors (i should join it to the various dfs to add the color columns)
prepare_palette <- function(df_long){
  livelli <- levels(df_long$task)
  colori <- hue_pal()(length(livelli))
  livelli <- c(" NONE", livelli)
  colori <- c("grey", colori)
  irr_palette <- bind_cols(task = sort(livelli), color = colori)
  return(irr_palette)
}

# function to prepare all data for IRR ----
prepare_data <- function(df_raw, sessions){
  long <- create_long_time_windows(df_raw, sessions)
  wide <- long %>% flat_tasks_df() # used for proportion kappa
  long <- add_track_n(long, wide) # used for static plot
  long_aggregated <- add_subcats_info(add_extra_info(long), df_raw) # used for interactive plot
  matched_pairs <- match_pairs(long_aggregated) # used for naming kappa and D-CCC
  task_table <- prepare_palette(long) # used for colors in plots/tables
  long <- long %>% left_join(task_table)
  long_aggregated <- long_aggregated %>% left_join(task_table %>% rename(Cosa = task))
  matched_pairs <- matched_pairs %>% left_join(task_table %>% rename(Cosa = task))
  out <- list(long = long, 
              wide = wide, 
              long_aggregated = long_aggregated, 
              matched_pairs = matched_pairs, 
              tasks = task_table
  )
}

prepare_data_multi <- function(df_raw, sessions){ # sessions is a vector of session ids, with lenght 2 at least
  long <- create_long_time_windows(df_raw, sessions)
  wide <- long %>% flat_tasks_df() # used for proportion kappa
  long <- add_track_n(long, wide) # used for static plot and to aggregate further
  long_aggregated <- add_subcats_info(add_extra_info(long), df_raw) # used for interactive plot
  task_table <- prepare_palette(long) # used for colors in plots/tables
  long <- long %>% left_join(task_table)
  long_aggregated <- long_aggregated %>% left_join(task_table %>% rename(Cosa = task))
  out <- list(long = long, 
              wide = wide, 
              long_aggregated = long_aggregated,
              tasks = task_table
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
  # print(livelli)
  a <- factor(task_obs1, levels = livelli)
  b <- factor(task_obs2, levels = livelli)
  contingenze_task <- table(a, b)
  perc_agreement <- sum(apply((contingenze_task * diag(nrow = length(livelli))), 1, "sum"))*100/sum(apply(contingenze_task, 1, "sum"))
  # cat("\nAgreement on task ", task_n, ": ", round(perc_agreement, digits = 1), "%\n", sep = "")
  # print(kappa_t1)
  # print(contingenze_task)
  detail <- as.tibble(kappa_t1$detail) %>% filter(Var2 == "Kappa") %>% select(task = Var1, kappa = n)
  #return(kappa_t1)
  out <- list(agreement = perc_agreement, 
              stats = kappa_t1$value,
              detail = detail,
              contigency_table = contingenze_task)
  return(out)
}

# function to compute multirater kappa from wide data ----
# intended for more than 2 observers (group sessions), do not return percent agreement or contengencies table
concordanza_2_task_multi <- function(df_flat, task_n){
  osservatori <- unique(df_flat$obs_id) # dovrebbero essere più di 2
  colonna <- 4 + task_n
  observations <- matrix()
  primo <- TRUE
  for (i in osservatori){
    task <- str_trim(df_flat[df_flat$obs_id == i, colonna])
    task <- ifelse(is.na(task), " NONE", task)
    if (primo){
      observations <- task
    } else {
      observations <- cbind(observations, task, deparse.level = 0)
    }
    primo <- FALSE
  }
  kappa_t1 <- kappam.fleiss(observations, detail = TRUE) # Fleiss' kappa on task
  detail <- as.tibble(kappa_t1$detail) %>% filter(Var2 == "Kappa") %>% select(task = Var1, kappa = n)
  out <- list(stats = kappa_t1$value,
              detail = detail)
  return(out)
}

# multivariate agreement (Janson, H., & Olsson, U. 2001) ----
concordanza_multi <- function(df_flat){
  # prepare dataset (must be a list)
  max_n_tasks <- max(df_flat$n_tasks)
  osservatori <- unique(df_flat$obs_id)
  multi_df <- vector("list", max_n_tasks)
  for (tasks in (1:max_n_tasks)){
    colonna <- 4 + tasks
    task_obs1 <- str_trim(df_flat[df_flat$obs_id == osservatori[1], colonna])
    task_obs2 <- str_trim(df_flat[df_flat$obs_id == osservatori[2], colonna])
    task_obs1 <- ifelse(is.na(task_obs1), " NONE", task_obs1)
    task_obs2 <- ifelse(is.na(task_obs2), " NONE", task_obs2)
    multi_df[[tasks]] <- cbind(task_obs1, task_obs2)
  }
  # compute iota
  kappa_multi <- iota(multi_df, scaledata = "nominal")
  return(kappa_multi)
}

# multivariate agreement for group sessions ----
concordanza_multi_group <- function(df_flat){
  # prepare dataset (must be a list)
  max_n_tasks <- max(df_flat$n_tasks)
  osservatori <- unique(df_flat$obs_id)
  multi_df <- vector("list", max_n_tasks)
  for (tasks in (1:max_n_tasks)){
    colonna <- 4 + tasks
    observations <- matrix()
    primo <- TRUE
    for (i in osservatori){
      task <- str_trim(df_flat[df_flat$obs_id == i, colonna])
      task <- ifelse(is.na(task), " NONE", task)
      if (primo){
        observations <- task
      } else {
        observations <- cbind(observations, task, deparse.level = 0)
      }
      primo <- FALSE
    }
    multi_df[[tasks]] <- observations
  }
  # compute iota
  kappa_multi <- iota(multi_df, scaledata = "nominal")
  return(kappa_multi)
}

# task matching by most overlap for Naming Kappa ----
# input df is long format aggregated, output is 
match_pairs <- function(df) {
  obs_ids <- unique(df$obs_id)
  # determine reference (defined as the longest sequence)
  n1 <- nrow(df %>% filter(obs_id == obs_ids[1]))
  n2 <- nrow(df %>% filter(obs_id == obs_ids[2]))
  if (n1 >= n2) {
    # reference is the first observer
    reference <- df %>% filter(obs_id == obs_ids[1])
    n_reference <- n1
    comparison <- df %>% filter(obs_id == obs_ids[2])
    n_comparison <- n2
  } else {
    # comparison is the first observer
    reference <- df %>% filter(obs_id == obs_ids[2])
    n_reference <- n2
    comparison <- df %>% filter(obs_id == obs_ids[1])
    n_comparison <- n1    
  }
  matched_tasks <- data.frame()
  for (i in 1:n_reference) {
    # define overlapping set in comparison df
    overlappling_set <- data.frame()
    for (j in 1:n_comparison) {
      if (comparison[j, ]$task_end > reference[i, ]$task_start & comparison[j, ]$task_start < reference[i, ]$task_end) {
        if (comparison[j, ]$task_end <= reference[i, ]$task_end) {
          if (comparison[j, ]$task_start <= reference[i, ]$task_start) {
            overlap <- comparison[j, ]$task_end - reference[i, ]$task_start
          } else {
            overlap <- comparison[j, ]$task_duration
          }
        } else {
          if (comparison[j, ]$task_start <= reference[i, ]$task_start) {
            overlap <- reference[i, ]$task_duration
          } else {
            overlap <- reference[i, ]$task_end - comparison[j, ]$task_start
          }          
        }
        overlappling_set <- bind_rows(overlappling_set, 
                                      bind_cols(task_id = comparison[j, ]$task_id, 
                                                duration = comparison[j, ]$task_duration,
                                                overlap = overlap,
                                                start = comparison[j, ]$task_start,
                                                fragment = comparison[j, ]$fragment,
                                                track = comparison[j, ]$track,
                                                task = comparison[j, ]$Cosa,
                                                task_details = comparison[j, ]$task_details))
      }
    }
    # find most overlapping task
    # print(overlappling_set)
    max_overlapping <- which(overlappling_set$overlap == max(overlappling_set$overlap))
    matched_tasks <- bind_rows(matched_tasks, 
                               bind_cols(most_overlapping_id = overlappling_set[max_overlapping[1], ]$task_id,
                                         most_overlapping_start = overlappling_set[max_overlapping[1], ]$start,
                                         most_overlapping_fragment = overlappling_set[max_overlapping[1], ]$fragment,
                                         most_overlapping_task = overlappling_set[max_overlapping[1], ]$task,
                                         most_overlapping_task_details = overlappling_set[max_overlapping[1], ]$task_details,
                                         most_overlapping_duration = overlappling_set[max_overlapping[1], ]$duration))
  }
  #str(matched_tasks)
  matched_tasks <- bind_cols(reference %>% ungroup() %>% select(task_id, task_start, fragment, task_duration, Cosa, task_details), matched_tasks)
  #View(matched_tasks)
  return(matched_tasks)
  
}

# calcolo naming Kappa ----
concordanza_naming <- function(matched_tasks){
  t1_obs1 <- matched_tasks$Cosa
  t1_obs2 <- matched_tasks$most_overlapping_task
  kappa_t1 <- kappam.fleiss(cbind(t1_obs1, t1_obs2), detail = TRUE) # Fleiss' kappa on task 
  livelli <- sort(union(unique(t1_obs1), unique(t1_obs2)))
  a <- factor(t1_obs1, levels = livelli)
  b <- factor(t1_obs2, levels = livelli)
  contingenze_task <- table(a, b)
  perc_agreement <- sum(apply((contingenze_task * diag(nrow = length(livelli))), 1, "sum"))*100/sum(apply(contingenze_task, 1, "sum"))
  # cat("\nAgreement on matched tasks: ", round(perc_agreement, digits = 1), "%\n\n")
  # print(kappa_t1)
  detail <- as.tibble(kappa_t1$detail) %>% filter(Var2 == "Kappa") %>% select(task = Var1, kappa = n)
  out <- list(agreement = perc_agreement, 
              stats = kappa_t1$value,
              detail = detail,
              contigency_table = contingenze_task)
  return(out)
}

# D-CCC -----
D_CCC <- function(matched_tasks){
  # drop pairs for D-CCC
  t1_obs1 <- matched_tasks$Cosa
  t1_obs2 <- matched_tasks$most_overlapping_task
  agreed_task <- if_else(t1_obs1 == t1_obs2, TRUE, FALSE)
  matched_tasks <- matched_tasks[agreed_task, ] %>% distinct(task_id, fragment, .keep_all = TRUE) %>% 
    distinct(most_overlapping_id, most_overlapping_fragment, .keep_all = TRUE)
  DCCC <- CCC(matched_tasks$task_duration, matched_tasks$most_overlapping_duration, 
                     ci = "z-transform",
                     conf.level = 0.95)
  return(DCCC) # interesting values are $rho.c$est  and $C.b
}

# plot scatter of matched durations ----
plot_D_CCC <- function(matched_tasks, task_color_table){
  # drop pairs for D-CCC
  obs1_id <- matched_tasks %>% 
    separate(task_id, sep = "_", remove = FALSE, into = c("obs","sess", "task_id_label")) %>%
    select(obs) %>% 
    distinct() %>% 
    as.numeric()
  obs2_id <- matched_tasks %>% 
    separate(most_overlapping_id, sep = "_", remove = FALSE, into = c("obs","sess", "task_id_label")) %>%
    select(obs) %>% 
    distinct() %>% 
    as.numeric()
  t1_obs1 <- matched_tasks$Cosa
  t1_obs2 <- matched_tasks$most_overlapping_task
  agreed_task <- if_else(t1_obs1 == t1_obs2, TRUE, FALSE)
  matched_tasks <- matched_tasks[agreed_task, ] %>% distinct(task_id, fragment, .keep_all = TRUE) %>% 
    distinct(most_overlapping_id, most_overlapping_fragment, .keep_all = TRUE)
  ggplot(matched_tasks, aes(x = task_duration, y = most_overlapping_duration, colour = color)) +
    geom_point(size = 3) +
    scale_colour_identity("activity", labels = task_color_table$task, breaks = task_color_table$color,
                        guide = "legend") +
    xlab(str_c("Observer", obs1_id,  "(task duration in seconds)", sep = " ")) +
    ylab(str_c("Observer", obs2_id,  "(task duration in seconds)", sep = " ")) +
    geom_abline(intercept = 0, slope = 1, colour = "red") + theme(legend.position='none') # +
    #theme(legend = element_blank())
}
#View(matched_tasks)
# matched_tasks %>% separate(task_id, sep = "_", remove = FALSE, into = c("obs","sess", "task_id_label")) %>%
#   select(obs) %>% distinct()
# plot_D_CCC(matched_tasks %>% left_join(colori %>% rename(Cosa = task)), 
#            colori)

plot_timing <- function(matched_tasks, task_color_table){
  # drop pairs for D-CCC
  t1_obs1 <- matched_tasks$Cosa
  t1_obs2 <- matched_tasks$most_overlapping_task
  agreed_task <- if_else(t1_obs1 == t1_obs2, TRUE, FALSE)
  matched_tasks <- matched_tasks[agreed_task, ] %>% 
    distinct(task_id, fragment, .keep_all = TRUE) %>% 
    distinct(most_overlapping_id, most_overlapping_fragment, .keep_all = TRUE) %>% 
    mutate(delta_start = task_start - most_overlapping_start) # %>% print()
  matched_tasks %>% 
    ggplot(aes(y = delta_start, x = Cosa, fill = color)) +
    geom_boxplot() + 
    geom_jitter() + 
    scale_fill_identity("activity", labels = task_color_table$task, breaks = task_color_table$color,
                          guide = "legend") +
    xlab("") +
    ylab("offset (paired tasks starting time)") + 
    #scale_x_discrete(limits = task_color_table$color) +
    #ggtitle("Distribution of the difference in the start time recorded by the observers for paired tasks.", subtitle = "Positive indicates that second observer recorded the beginning of the task before than the first one") + 
    theme(legend.position = "none", axis.ticks.x = element_blank()) + #, axis.text.y = element_blank()
    coord_flip()
}
# matched_tasks %>% 
#   mutate(delta_start = task_start - most_overlapping_start) %>% 
#   group_by(Cosa) %>% 
#   summarise(median_delay = median(delta_start))
#

# plot function based on geom_rect ----
plot_sequences_rect_ok <- function(df, task_color_table, axis_unit = "min", show.labels = TRUE) {
  start_sec <- min(df$task_start)
  end_sec <- max(df$task_end)
  max_n_active_tasks <- max(df$track)
  
  if (axis_unit == "min"){
    titolo <- "Time (minutes in the session)"
    tacche <- seq(start_sec, end_sec, by = 60)
    etichette <- str_c(tacche/60, "'", sep = "")
  } else {
    titolo <- "Time (seconds in the session)"
    tacche <- seq(start_sec, end_sec, by = 10)
    etichette <- tacche
  }
  
  # ploting code
  sequence_plot <- ggplot(df, aes(xmin = task_start, xmax = task_end + 1, ymin = track - 0.5 , ymax = track + 0.5)) +
    geom_rect(aes(fill = color), colour = "grey50") + #, lineend = "square"
    xlab(titolo) +
    ylab("") +
    scale_fill_identity("activity", labels = task_color_table$task, breaks = task_color_table$color,
                        guide = "legend") +
    scale_x_continuous(
      breaks = tacche,
      label = etichette,
      minor_breaks = seq(start_sec, end_sec, by = 10),
      limits = c(start_sec, end_sec + 1)
    ) +
    scale_y_continuous( # control using max(track)
      breaks = 1:max_n_active_tasks,
      label = paste("Task", 1:max_n_active_tasks, sep = " "),
      minor_breaks = NULL,
      limits = c(0.5, max_n_active_tasks + 0.5)
    ) +
    geom_segment(aes(x = task_end + 1, xend = task_end + 1, y = (track - 0.5), yend = track + 0.5, linetype =  factor(interruzione)), data = df, inherit.aes = FALSE, show.legend = FALSE, colour = "black") +
    facet_wrap(~ obs_id, ncol = 1) 
  if (show.labels){
    sequence_plot <- sequence_plot +
      geom_text(aes(x = midpoint, y = track, label = task_id), size = 2, angle = 90, colour = "black")
  }
  sequence_plot
} 

# build html table for individual categories K (bootstrap style) ----
format_details_table <- function(table){
  tags$table(class = "table",
             tags$thead(tags$tr(
               tags$th(""),
               tags$th("Activity"),
               tags$th("K")
             )),
             tags$tbody(apply(table, 
                              1, 
                              function(x) tags$tr(eval(parse(text = paste0('list(',paste0(c(paste0('tags$td(span(style="width:1.1em; height:1.1em; display:inline-block; background-color:', x[3],';"))'), paste0('tags$td("', x[1:2],'")')),collapse=","),')')))))
             ))
}

# plot time windows (for static plot or group comparison plot) ----
# code
plot_time_windows <- function(df, start_sec = 0, end_sec = NULL, axis_unit = "min", gaps = FALSE, show.labels = TRUE, vertical = FALSE) {
  if (is.null(end_sec)){
    end_sec <- max(df$wind_id)
  }
  if (!"track" %in% names(df)) {
    df <- df %>% add_track_n_2()
  }
  max_n_active_tasks <- max(df$track)
  if (!gaps | (end_sec - start_sec >180)){
    space = 1
  } else {
    space = .9
  }
  if (axis_unit == "min"){
    titolo <- "Time (minutes in the session)"
    tacche <- seq(start_sec, end_sec, by = 60)
    etichette <- str_c(tacche/60, "'", sep = "")
  } else {
    titolo <- "Time (seconds in the session)"
    tacche <- seq(start_sec, end_sec, by = 10)
    etichette <- tacche
  }
  task_extra_info <- df %>% 
    group_by(obs_id, task_id, fragment) %>% 
    summarise(midpoint = (first(wind_id) + last(wind_id))/2, 
              task_start = first(wind_id),
              task_end = (last(wind_id) + 0.5), 
              track = first(track)) %>% 
    filter(task_end >= start_sec, 
           task_start <= end_sec) %>% 
    mutate(interruzione = if_else(max(fragment) - fragment > 0, 1, 0),
           midpoint = if_else(midpoint < start_sec, 
                              (start_sec + task_end - 0.5)/2, 
                              if_else(midpoint > end_sec, 
                                      (task_start + end_sec)/2,
                                      midpoint))) %>% 
    separate(task_id, sep = "_", remove = FALSE, into = c("obs","sess", "task_id_label")) %>% 
    select(-obs, -sess)
  # here I could do the left join with the orignal df, by task id obviously, but i don't like that it is external
  sequence_plot <- ggplot(df %>% filter(wind_id >= start_sec, wind_id <= end_sec), aes(wind_id, fill = task)) +
    geom_bar(aes(group = task_id), width = space, position = position_stack(reverse = TRUE)) +
    xlab(titolo) +
    ylab("") +
    scale_x_continuous(
      breaks = tacche,
      label = etichette,
      minor_breaks = seq(start_sec, end_sec, by = 10),
      limits = c(start_sec, end_sec)
    ) +
    scale_y_continuous(
      breaks = 1:max_n_active_tasks - 0.5,
      label = paste("Task", 1:max_n_active_tasks, sep = " "),
      minor_breaks = NULL
    ) +
    geom_segment(aes(x = task_end, xend = task_end, y = (track - 1), yend = track, linetype =  factor(interruzione)), data = task_extra_info, inherit.aes = FALSE, show.legend = FALSE) 
  if (show.labels){
    sequence_plot <- sequence_plot +
      geom_text(aes(x = midpoint, y = (track - 0.5), label = task_id_label), data = task_extra_info, inherit.aes = FALSE, size = 2, angle = if_else(vertical, 0, 90))
  }
  if (vertical) {
    sequence_plot <- sequence_plot + 
      facet_wrap(~ obs_id, ncol = 2) +
      coord_flip()
  } else {
    sequence_plot <- sequence_plot + 
      facet_wrap(~ obs_id, ncol = 1)
  }
  sequence_plot
} 

# Sequence Needleman-wunsch computation ----
SNW <- function(df, task_table) {
  # df is in long aggregated form
  n_tipi_task <- nrow(task_table) - 1
  task_table <- task_table %>% slice(-1) %>% bind_cols(string = letters[1:n_tipi_task])
  osservatori <- unique(df$obs_id) 
  df <- df %>% left_join(task_table %>% select(-color) %>% rename(Cosa = task))
  task_obs1 <- str_trim(df[df$obs_id == osservatori[1], ]$string)
  task_obs2 <- str_trim(df[df$obs_id == osservatori[2], ]$string)
  seq_obs1 <- str_c(task_obs1, collapse = "")
  seq_obs2 <- str_c(task_obs2, collapse = "")
  # print(defaultNeedleParams)
  results <- needles(seq_obs1, seq_obs2, params=defaultNeedleParams)
  seq1_vec <- strsplit(as.character(results$align1), split = "")
  seq2_vec <- strsplit(as.character(results$align2), split = "")
  seq_all_vec <- c(seq1_vec[[1]], seq2_vec[[1]])
  # print(seq_all_vec)
  obs <- c(rep(osservatori[1], times = length(seq1_vec[[1]])), rep(osservatori[2], times = length(seq2_vec[[1]])))
  y = rep(1:length(seq1_vec[[1]]), times = 2)
  sequences <- bind_cols(obs_id = obs, y = y, string = seq_all_vec) %>% left_join(task_table)
  # normalize score to 0-1 range
  t_max <- 3*length(seq1_vec[[1]])
  t_min <- -1*sum(is.na(sequences$task))
  n_score <- (results$score - t_min)/(t_max - t_min)
  out <- list(score = n_score,
              sequences = sequences)
  return(out) # servono $score $align1 $align2
}

# plot function for aligned sequences ----
# input is actually df sequences, the returned by SNW
plot_aligned_sequences <- function(df_long_aggr, task_color_table) {
  df_long_aggr <- df_long_aggr %>% mutate(obs_id = as.factor(obs_id))
  observers_ids <- levels(df_long_aggr$obs_id)
  df_long_aggr %>% 
    ggplot(aes(x = y, y = as.numeric(obs_id), fill = color)) + 
    geom_tile(colour = "grey") +
    scale_fill_identity("", labels = task_color_table$task, breaks = task_color_table$color,
                        guide = "legend") +
    guides(fill = guide_legend(ncol = 2)) +
    scale_x_reverse() + 
    #theme_void() +
    ylab("") + 
    xlab("") + 
    scale_y_continuous(breaks = c(1, 2), labels = str_c("obs.", observers_ids, sep = " "), sec.axis = dup_axis()) +
    theme(axis.text.x = element_text(face = "bold", size = 13), 
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(colour = NA, fill = NA),
          axis.text.y = element_blank(),
          legend.position = "bottom",
          axis.ticks = element_blank()) +
    coord_flip()
}

# function to save results and load saved ones ----
outputDir <- "saved_results"

prepare_filename <- function(df_raw, sessions) {  # date_session_obs1_id_obs2_id_sess1_id_sess2_id_part_id.csv
  df_raw <- df_raw %>% 
    filter(`session id` %in% sessions)
  obs_ids <- unique(df_raw$`observer id`)
  session_date <- df_raw[1, ]$`session date`
  session_time <- df_raw[1, ]$`session start`
  part_id <- df_raw[1, ]$`participant id`
  filename <- str_c(str_c(session_date, hour(session_time), minute(session_time), "observers", str_c(obs_ids, collapse = "_"), "sessions", str_c(sessions, collapse = "_"), part_id, sep = "_"), ".csv", sep = "")
  return(filename)
}
# prepare_filename(dati_prova_irr_2, c(54, 60))

saveData <- function(data, df_raw, sessions) {
  # Create a unique file name
  fileName <- prepare_filename(df_raw, sessions) # date_session_obs1_id_obs2_id_sess1_id_sess2_id_part_id.csv
  # Write the file to the local system
  write_csv(data, path = file.path(outputDir, fileName))
}

loadData <- function() {
  # Read all the files into a list
  filenames <- list.files(outputDir, full.names = FALSE)
  if (length(filenames) == 0) {
    data <- data.frame(`iota id` = NA)
  } else {
    data <- str_split_fixed(filenames, "_", n = 10)
    data <- data[, c(1:3,5:6,8:10)]
    data <- cbind(1:nrow(data), filenames, data)
    data <- as.data.frame(data)
    data[, c(1, 4:9)] <- apply(data[, c(1, 4:9)], 2, as.numeric)
    names(data) <- c("iota id", "filename", "date", "hour", "minutes", "obs1 id", "obs2 id", "sess1 id", "sess2 id", "participant id")
    data <- data %>% 
      separate(., `participant id`, sep = "\\.", into = c("participant id", "ext")) %>% 
      select(-ext)
  }
  return(data)
}

# outputDir <- "Dashboard/saved_results"
# filenames <- list.files(outputDir, full.names = FALSE)
# prova_saved <-  loadData()
# prova_saved <- prova_saved %>% filter(`iota id` %in% 1:2)
# str_c(outputDir, prova_saved$filename, sep = "/")
# str_c(prova_saved$`iota id`, 
#       str_c(prova_saved$date, 
#             str_c(prova_saved$hour, 
#                   prova_saved$minutes,
#                   sep = ":"),
#             sep = " "), 
#       sep = ": ")

loadSelectedData <- function(data, iota_ids) {
  data <- data %>% filter(`iota id` %in% iota_ids)
  files <- str_c(outputDir, data$filename, sep = "/")
  # Read all the files into a list
  # files <- list.files(outputDir, full.names = TRUE)
  dataout <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  
  # Concatenate all data together into one data.frame
  dataout <- do.call(rbind, dataout)
  # add timestamp as first columns (for x in plot)
  timing <- str_c(data$`iota id`, 
                  str_c(data$date, 
                        str_c(data$hour, 
                              data$minutes,
                              sep = ":"),
                        sep = " "), 
                  sep = ": ")
  dataout <- bind_cols(id = timing, dataout)
  # now need to reshape in long form for plotting
  dataout <- dataout %>% gather(., "measure", "value", 2:11)
  dataout # a tibble after reshaping
}

#loadSelectedData(prova_saved, c(1,2,3)) %>% View() # plot_IOTA_measures #

plot_IOTA_measures <- function(df) {
  left <- df %>% filter(measure %in% c("pK1", "pK2", "pKm", "NK")) %>%
    ggplot(aes(x = id, y = value, colour = measure, group = measure)) +
    geom_line(aes(size = measure, linetype = measure)) + 
    geom_point() +
    scale_y_continuous(limits = c(-0.1, 1)) + 
    ggtitle("Kappa (proportion of time on tasks and naming)") +
    xlab("") +
    ylab("") +
    scale_colour_manual(values = c("#28a745", rep("#428bca", times = 3)), breaks = c("NK", "pK1", "pK2", "pKm")) + 
    scale_size_manual(breaks = c("NK", "pK1", "pK2", "pKm"), values = c(1, 1, 1, 2)) + 
    scale_linetype_manual(breaks = c("NK", "pK1", "pK2", "pKm"), values = c("solid", "solid", "dotted", "solid")) + 
    theme(title = element_text(size = 10))
  middle <- df %>% filter(measure %in% c("pAg1", "pAg2", "NAg")) %>% 
    ggplot(aes(x = id, y = value, fill = measure)) +
    geom_bar(stat = "identity", position = position_dodge()) + 
    scale_y_continuous(limits = c(0, 100)) +
    scale_fill_manual(breaks = c("NAg", "pAg1", "pAg2"), values = c("#28a745", "#428bca", "dodgerblue4")) + 
    ggtitle("Percent agreement (prop. time and naming)") +
    xlab("") +
    ylab("") +
    theme(title = element_text(size = 10))
  right <- df %>% filter(measure %in% c("DCCCr", "DCCCcb", "NW_score")) %>% 
    ggplot(aes(x = id, y = value, colour = measure, group = measure)) +
    geom_line(aes(linetype = measure)) + 
    geom_point() +
    scale_y_continuous(limits = c(-0.1, 1)) + 
    scale_colour_manual(breaks = c("DCCCcb", "DCCCr", "NW_score"), values = c("orange", "orange", "deepskyblue")) + 
    scale_linetype_manual(breaks = c("DCCCcb", "DCCCr", "NW_score"), values = c("dotted", "solid", "solid")) + 
    ggtitle("Tasks duration and sequence agreement") +
    xlab("") +
    ylab("") +
    theme(title = element_text(size = 10))
  panel <- grid.arrange(left, middle, right, top = textGrob("Inter Observers Reliability Measures across time", gp = gpar(fontsize = 14, fontface = "bold")), nrow = 1)
  #panel
}

# outputDir <- "Dashboard/saved_results"
#loadSelectedData(prova_saved, c(1,2,3)) %>% plot_IOTA_measures #

# tests -----
# aggregati_prova <- add_extra_info(dati_to_plot_compare_new_long_final) %>%
#   add_subcats_info(., dati_prova_irr_2) %>%
#   left_join(colori %>% rename(Cosa = task))
# View(aggregati_prova)
# plot_sequences_rect_ok(aggregati_prova)
# 
# osser <- unique(aggregati_prova$obs_id)
# set1 <- which(aggregati_prova$obs_id == 4 & aggregati_prova$interruzione == 0)
# set2 <- which(aggregati_prova$obs_id == 32 & aggregati_prova$interruzione == 0)
# set3 <- which(aggregati_prova$obs_id == 4 & aggregati_prova$interruzione == 1)
# set4 <- which(aggregati_prova$obs_id == 32 & aggregati_prova$interruzione == 1)
# aggregati_prova[set1,]
# min(aggregati_prova$task_start)
# max(aggregati_prova$task_end)
# 
# seq(min(aggregati_prova$task_start), max(aggregati_prova$task_end), by = 60)
# 
# plot_da_convertire_2 <- plot_sequences_rect_ok(aggregati_prova, colori, show.labels = FALSE)
# interactive_plot <- ggplotly(plot_da_convertire_2 + theme(legend.position='none'))
# interactive_plot <- plotly_build(plot_da_convertire_2 + theme(legend.position='none'))
# labels_plot <- str_c(aggregati_prova$Cosa,
#       ifelse(aggregati_prova$`Cosa (subcategories)` != "0", aggregati_prova$task_details, ""),
#       ifelse(aggregati_prova$Dove != "0", aggregati_prova$Dove, ""), sep = "\n")
# style(interactive_plot, text=labels_plot, hoverinfo = "text", hoveron = "points", traces = 15:18)
# plotly_json(step1)
# plotly_json(step2)
# step1 <- style(interactive_plot, hoverinfo = "none", traces = 1:14)
# step2 <- style(step1, text=rep(labels_plot[set1], each = 3), hoverinfo = "text", traces = 15)
# step3 <- style(step2, text=rep(labels_plot[set2], each = 3), hoverinfo = "text", traces = 16)
# step4 <- style(step3, text=rep(labels_plot[set3], each = 3), hoverinfo = "text", traces = 17)
# step5 <- style(step4, text=rep(labels_plot[set4], each = 3), hoverinfo = "text", traces = 18)
# plotly_json(step5)
# step2 <- style(step1, text=labels_plot, hoverinfo = "text", traces = 15:18)
# step3 <- style(step2, text=labels_plot, hoverinfo = "fills", traces = 1:14)
# step4 <- style(step3, text=labels_plot, hoverinfo = "text", traces = 1:14)

# aggregati_prova_2 <- add_extra_info(dati_to_plot_compare_new_long_fm_final) %>%
#   add_subcats_info(., dati_prova_irr_2) %>% 
#   left_join(colori_2 %>% rename(Cosa = task)) 
# colori_2 <- prepare_palette(dati_to_plot_compare_new_long_fm_final)
# plot_da_convertire_3 <- plot_sequences_rect_ok(aggregati_prova_2, colori_2, show.labels = FALSE)
# interactive_plot_fm <- ggplotly(plot_da_convertire_3 + theme(legend.position='none'))
# plotly_json(interactive_plot_fm)
# str(interactive_plot_fm$x$data)
# length(interactive_plot_fm$x$data)
# dati_to_plot_compare_new_long <- create_long_time_windows(dati_prova_irr_2, 54, 60) %>% flat_tasks_df() # Giulio e Ste
# dati_to_plot_compare_new_long_final <- add_track_n(dati_to_plot_compare_new_long, dati_to_plot_compare_new_flat) 
# dati_to_plot_compare_new_long_fm_final <- add_track_n_2(dati_to_plot_compare_new_long_fm) # slower,
# View(add_subcats_info(dati_prova_extended, dati_prova_irr_2))
# add_extra_info(dati_to_plot_compare_new_long_final) %>% 
#   add_subcats_info(., dati_prova_irr_2) %>% 
#   View()
