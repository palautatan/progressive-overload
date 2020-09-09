

# --------------- SERVER ---------------------

server <- function(input, output) {
    
    
    # --------------- LOAD ALL FILES -------------------------------------
    workouts <- list.files(paste0('../profiles/', tolower(last_name)), full.names=TRUE)
    
    
    ## --------------- NUMBER SESSIONS ---------------------------------------
    output$number_of_workouts <- renderValueBox({valueBox(length(workouts),
                                                          subtitle='Sessions to Date',
                                                          icon = icon('check'),
                                                          color = 'green'
                                                          )})
    
    workout_info <- lapply(workouts, readr::read_csv)
    
    this_workout <- workout_info[[1]] # ---- MOST RECENT WORKOUT HERE -----
    
    
    
    
    ## --------------- SESSIONS PER WEEK ---------------------------------------
    workout_dates <- lapply(workout_info,
                            function(k) {
                                k %>% filter(exercise=='session start') %>% pull(date)
                            })
    
    per_week <- table(unlist(lapply(workout_dates, function(k) cut(k, 'week'))))
    output$per_week <- renderValueBox(valueBox(mean(per_week),
                                               'Average Sessions Per Week',
                                               icon = icon('check'),
                                               color = 'green'))
    
    
    
    
    ## PREVIOUS WORKOUT
    output$previous_exercises <- renderTable({this_workout %>% filter(exercise != 'session start') %>% select(exercise) %>% unique()})
    
    
    # ---------------------------- MENU ITEMS ------------------------------
    
    output$menu <- renderMenu({
        
        sidebarMenu(
            menuItem('Home',
                     tabName = 'home',
                     icon = icon('home')),
            
            menuItem('Log Workout',
                     tabName = 'log',
                     icon = icon('dumbbell')),
            
            menuItem('Achievements',
                     tabName = 'achievements',
                     icon = icon('medal'))
        )
        
    })
    
    
    
    
    
    
    
    df_data <- data.frame(date = NULL,
               time = NULL,
               exercise = NULL,
               weight = NULL,
               repetitions = NULL,
               notes = NULL)
    values <- reactiveValues(df_data = df_data)
    
    
    # START WORKOUT
    # Needs to be edited to make sure a user must click start workout before saving any exercises.
    # The reason? To make sure we have a starting for each session!
    
    observeEvent(input$start, {
        # SAVE NEW INPUT
        now_date <- as.character(anytime::anydate(lubridate::now()))
        now_time <- as.character(strftime(lubridate::now(), format='%H:%M:%S'))
        
        input_vals <- data.frame(date = now_date,
                                 time = now_time,
                                 exercise = 'session start',
                                 equipment = NA,
                                 weight = NA,
                                 repetitions = NA,
                                 notes = NA)
        
        
        values$df_data <- rbind(input_vals, values$df_data)
        
    })
    
    
    # CLEAR WORKOUT
    # Needs to be edited to make sure a user must click start workout before saving any exercises.
    # The reason? To make sure we have a starting for each session!
    
    observeEvent(input$clear, {
        # CLEAR INPUT
        
        df_data <- data.frame(date = NULL,
                              time = NULL,
                              exercise = NULL,
                              weight = NULL,
                              repetitions = NULL,
                              notes = NULL)
        
        values$df_data <- df_data
        
    })
    
    
    
    # SAVE EXERCISE
    observeEvent(input$save, {
        # SAVE NEW INPUT
        now_date <- as.character(anytime::anydate(lubridate::now()))
        now_time <- as.character(strftime(lubridate::now(), format='%H:%M:%S'))
        
        input_vals <- data.frame(date = now_date,
                                 time = now_time,
                                 exercise = input$exercise,
                                 equipment = input$equipment,
                                 weight = input$weight,
                                 repetitions = input$reps,
                                 notes = input$notes)
        

        values$df_data <- rbind(input_vals, values$df_data)
        
    })
    
    
    
    # UNDO SAVE
    observeEvent(input$undosave, {
        
        values$df_data <- values$df_data[-1,]
        
    })
    
    
    # DF DATA OBJECT
    output$df_data_out <- renderTable(values$df_data)
    
    
    
    # SAVE WORKOUT
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0('profiles/', tolower(last_name), '/',
                   as.character(anytime::anydate(lubridate::now())),
                   '.csv')
        },
        content = function(file) {
            write.csv(values$df_data, file, row.names = FALSE)
        }
    )
    
    
    
    
    
    
    # PLOTS
    
    output$recentsets <- renderPlot(this_workout %>%
                                        filter(exercise != 'session start') %>%
                                        ggplot(aes(x=exercise)) +
                                        geom_bar() +
                                        ylab('sets') +
                                        xlab('exercise') +
                                        ggtitle('Number of Sets Per Exercise') +
                                        theme_classic() +
                                        theme(axis.text.x = element_text(angle = 90)))
    
    output$recentreps <- renderPlot(this_workout %>%
                                       filter(exercise != 'session start') %>%
                                       group_by(exercise) %>%
                                       summarize(mean_rep=mean(repetitions)) %>%
                                       ggplot(aes(x=exercise, y=mean_rep)) +
                                       geom_bar(stat='identity') +
                                       ylab('sets') +
                                       xlab('exercise') +
                                       ggtitle('Average Reps Per Exercise') +
                                       theme_classic() +
                                       theme(axis.text.x = element_text(angle = 90)))
    
    output$recentweights <- renderPlot(this_workout %>%
                                           filter(exercise != 'session start') %>%
                                           ggplot(aes(x=exercise, y=weight)) +
                                           geom_count() +
                                           theme_classic() +
                                           theme(axis.text.x = element_text(angle = 90)) +
                                           labs(size='sets') +
                                           ggtitle('Total Weight per Exercise')
    )
    
    
    output$recentchart <- renderTable(this_workout %>%
                                          filter(exercise != 'session start') %>%
                                          group_by(exercise, equipment) %>%
                                          summarize(`max total weight (lbs)`=max(weight)))
    
}





