

# --------------- SERVER ---------------------

server <- function(input, output) {
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
    
    
    ## COPIED FROM https://community.rstudio.com/t/shiny-directory-input/29160/2
    
    shinyDirChoose(
        input,
        'dir',
        roots = c(home = '~'),
        filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
    )
    
    global <- reactiveValues(datapath = paste0(getwd(), '/profiles/', tolower(last_name)))
    
    dir <- reactive(input$dir)
    
    output$dir <- renderText({
        global$datapath
    })
    
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                     input$dir
                 },
                 handlerExpr = {
                     if (!"path" %in% names(dir())) return()
                     home <- normalizePath("~")
                     global$datapath <-
                         file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                 })
    
    ##
    
    
    output$wd <- renderText({
        paste('<B>Workout Directory: </B>')
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
    
        
    }





