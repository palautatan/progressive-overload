library(shiny)
library(shinydashboard)
library(shinyFiles)
library(dplyr)
library(ggplot2)

# --------------- APP NAME -----------------------------
app_name <- 'Progressive Overload'



# --------------- TEMPORARY USER PROFILE ---------------
full_name <- 'Edie Espejo'
last_name <- strsplit(full_name, ' ')[[1]][2]
welcome <- paste0('Welcome back, ', full_name, '!')



# --------------- EXERCISE DATABASE ---------------------
exercise_db <- readr::read_csv('../exercises/exercises-db.csv')
all_exercises <- exercise_db$exercise

all_equipment <- c('barbell', 'dumbbell, pair', 'dumbbell, single', 'EZ curl bar', 
                   'kettlebell', 'olympic bar', 'resistance band')





# ----------------- CALENDAR -----------------------
get_time <- function(k) {
  k %>%
    summarize(start=tail(time, 1),
              end=head(time, 1)) %>%
    summarize(mins=difftime(end, start, units='mins')) %>%
    as.numeric()
}

