

# --------------- SERVER ---------------------

server <- function(input, output) {
    
    
    # --------------- LOAD ALL FILES -------------------------------------
    workouts <- list.files(paste0('../profiles/', tolower(last_name)), full.names=TRUE)
    
    
    
    ## --------------- NUMBER SESSIONS ---------------------------------------
    output$number_of_workouts <- renderValueBox({valueBox(length(workouts),
                                                          subtitle='Sessions to Date',
                                                          icon = icon('check'),
                                                          color = 'purple'
                                                          )})
    
    workout_info <- lapply(workouts, readr::read_csv)
    
    this_workout <- workout_info[[length(workout_info)]] # ---- MOST RECENT WORKOUT HERE -----
    
    
    
    
    ## --------------- SESSIONS PER WEEK ---------------------------------------
    workout_dates <- lapply(workout_info,
                            function(k) {
                                k %>% filter(exercise=='session start') %>% pull(date)
                            })
    
    per_week <- table(unlist(lapply(workout_dates, function(k) cut(k, 'week'))))
    output$per_week <- renderValueBox(valueBox(mean(per_week),
                                               'Average Sessions Per Week',
                                               icon = icon('check'),
                                               color = 'yellow'))
    
    
    
    
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
            
            menuItem('Statistics',
                     tabName = 'statistics',
                     icon = icon('chart-bar'))
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
    
    
    
    
    
    
    
    
    
    # -------------- RECENT WORKOUT -----------------------------------
    # -------------- EXERCISE LIST ------------------------------------
    
    ex_date <- this_workout %>%
        filter(exercise == 'session start') %>%
        pull(date) %>%
        as.character()
    
    dow <- weekdays(as.Date(ex_date))
    
    output$ex_date <- renderText({paste0('Your last session was on ', dow, ', ', ex_date, '.')})

    
    ex_list <- this_workout %>% filter(exercise != 'session start') %>%
        select(exercise) %>%
        unique() %>%
        pull() %>%
        rev()
    
    output$num_exercises <- renderValueBox({valueBox(length(ex_list),
                                                     subtitle='Total Exercises',
                                                     icon = icon('dumbbell'),
                                                     color = 'yellow')})
    
    
    ex_list <- c(ex_list[-length(ex_list)], paste0('and ', ex_list[length(ex_list)]))
    
    output$ex_list <- renderText(paste0('In your most recent session, you completed the following exercises: ', 
                                        ex_list %>% paste0(collapse='; '), '.'))
    
    
    
    
    
    total_time <- this_workout %>%
        summarize(start=tail(time, 1),
                  end=head(time, 1)) %>%
        summarize(mins=difftime(end, start, units='mins')) %>%
        summarize(m=as.numeric(round(mins)),
                  s=as.numeric(round(abs(mins-round(mins)))*60))
    
    if (nchar(total_time$s)==1) {
        total_time$s <- paste0(total_time$s, 0)
    }
    
    total_time <- total_time %>% paste0(collapse=':')
    
    
    
    
    output$total_time <- renderValueBox({valueBox(total_time,
                                                  subtitle='Duration',
                                                  icon = icon('clock'),
                                                  color = 'purple'
    )})
    
    
    # --------------------------------- RECENT -------------------------------------
    # ---------------------------------- PLOTS -------------------------------------
    
    output$recentsets <- renderPlot(this_workout %>%
                                        filter(exercise != 'session start') %>%
                                        ggplot(aes(x=exercise)) +
                                        geom_bar(aes(fill=..count..)) +
                                        ylab('sets') +
                                        xlab('exercise') +
                                        ggtitle('Number of Sets Per Exercise') +
                                        theme_classic() +
                                        theme(axis.text.x = element_text(angle = 90)) +
                                        scale_fill_gradientn(colours=c('goldenrod', '#EDCB62', '#483D8B', '#473C8B'), guide = FALSE))
    
    output$recentreps <- renderPlot(this_workout %>%
                                       filter(exercise != 'session start') %>%
                                       group_by(exercise) %>%
                                       summarize(mean_rep=mean(repetitions)) %>%
                                       ggplot(aes(x=exercise, y=mean_rep)) +
                                       geom_bar(aes(fill=robustHD::standardize(mean_rep)), stat='identity') +
                                       ylab('reps') +
                                       xlab('exercise') +
                                       ggtitle('Average Reps Per Exercise') +
                                       theme_classic() +
                                       theme(axis.text.x = element_text(angle = 90)) +
                                       scale_fill_gradientn(colours=c('goldenrod', '#EDCB62', '#483D8B', '#473C8B'), guide = FALSE)
                                    
                                    )
    
    output$recentweights <- renderPlot(this_workout %>%
                                           filter(exercise != 'session start') %>%
                                           ggplot(aes(x=exercise, y=weight)) +
                                           geom_count(aes(col=weight)) +
                                           theme_classic() +
                                           theme(axis.text.x = element_text(angle = 90)) +
                                           labs(size='sets') +
                                           ggtitle('Total Weight per Exercise') +
                                           expand_limits(y = 0)  +
                                           guides(size = guide_legend(override.aes = list(color='#363636'))) +
                                           scale_colour_gradientn(colours=c('orange', '#EDCB62', '#483D8B', '#473C8B'), guide = FALSE)
    )
    
    
    output$recentchart <- renderTable(this_workout %>%
                                          filter(exercise != 'session start') %>%
                                          group_by(exercise, equipment) %>%
                                          summarize(`max total weight (lbs)`=max(weight)))
    
    
    
    # ---------------------- CREATE BOXES --------------------------------
    # https://stackoverflow.com/questions/52162024/create-value-box-in-renderui-by-looping
    
    
    all_history <- do.call(rbind, workout_info)
    
    output$prboxes <- renderUI({
        
        prs <- all_history %>%
            arrange(-as.numeric(date), exercise) %>%
            filter(exercise != 'session start') %>%
            group_by(exercise) %>%
            slice(which.max(weight)) %>%
            select(date, exercise, equipment, weight, repetitions) %>%
            arrange(-weight)
        
        
        lapply(1:nrow(prs), function(i) { 
            
            
            valueBox(subtitle=HTML(paste0(prs$exercise[i],
                                          br(),
                                          paste0(prs$repetitions[i], ' reps'),
                                          br(),
                                          prs$equipment[i],
                                          br(),
                                          prs$date[i])),
                     value=prs$weight[i],     #here display number1 one by one like name 
                     color = 'purple',
                     width = 3
            )
        } )
        
    })
    
    
    completed_exercises <- all_history %>%
        filter(exercise != 'session start') %>%
        select(exercise) %>%
        unique()

    output$completed_exercises = renderUI({
        selectInput('completed_exercise', 'Exercises', completed_exercises)
    })
    
    
    output$time_series_plot <- renderPlot({
        
        a1 <- all_history %>%
            filter(exercise == input$completed_exercise) %>%
            group_by(date, exercise, weight) %>%
            summarize(`average reps`=mean(repetitions))
        
        a2 <- a1 %>%
            group_by(date, exercise) %>%
            summarize(minw=min(weight),
                      maxw=max(weight)) 
        
        merge(a1, a2) %>%
            ggplot(aes(x=date, y=weight)) +
            geom_segment(aes(x = date, xend = date, y = minw, yend = maxw)) +
            geom_point(aes(col=`average reps`, size=`average reps`)) +
            facet_wrap(~ exercise) +
            expand_limits(y = 0)  +
            guides(size = guide_legend(override.aes = list(color='#363636'))) +
            scale_colour_gradientn(colours=c('orange', '#EDCB62', '#483D8B', '#473C8B')) +
            guides(size=FALSE) +
            theme_classic()
        
    })

    
}





