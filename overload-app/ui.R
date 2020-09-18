

# ---------- BODY -----------------
body <- dashboardBody(
    
    tabItems(
        
        # --------- HOME ------------
        tabItem(
            tabName = 'home',
            fluidRow(
                box(title = welcome,
                    width=6,
                    status = 'warning',
                    'Continue your program.'),
                
                valueBoxOutput('number_of_workouts',
                         width=3),
                
                valueBoxOutput('per_week',
                         width=3),
                         
                
            ),
            
            fluidRow(
                
                box(title = 'Previous Session',
                    width=6,
                    'You did the following exercises during your last session.',
                    
                    tableOutput('previous_exercises')),
                
                box(title = 'Calendar',
                    width=6,
                    plotOutput('workout_calendar'))
                
                
            )
            
        ),
        
        # --------- LIFT ------------
        tabItem(
            tabName = 'log',

            
            fluidRow(
                # START WORKOUT
                box(title='Start Workout',
                    status='warning',
                    width=12,
                    'Click "Start Workout" to begin.
                    Choose your exercise from the dropdown list,
            then submit your equipment weight and number of reps.',
                    br(),
                    br(),
                    actionButton('start', 'Start Workout'),
                    actionButton('clear', 'Clear Workout'),
                    ),
                
                
            ),
            
            fluidRow(
                # EXERCISE
                box(title='Exercise',
                    width=4,
                    selectInput(inputId = 'exercise',
                                label = 'Exercise',
                                all_exercises),
                    
                    selectInput(inputId = 'equipment',
                                label = 'Equipment',
                                all_equipment),
                    
                    numericInput(inputId = 'reps',
                                 label = 'Repetitions',
                                 min = 1,
                                 max = 100,
                                 value = 12,
                                 step=1),
                    
                    
                    numericInput(inputId = 'weight',
                                 label = 'Total Weight (lbs)',
                                 min = 1,
                                 max = 500,
                                 value = 20,
                                 step=1),
                    
                ),
                
                # EXERCISE
                box(title='Specifications',
                    width=4,
                    
                    
                    radioButtons('handedness', label='Handedness', 
                                       choices = list('left', 'right', 'both'),
                                       selected = 'both'),
                    
                    checkboxGroupInput('grip', label='Grips', 
                                       choices = list('wide'='wide', 'narrow'='narrow', 'overhand'='overhand', 'underhand'='underhand', 'alternate'='alternate'),
                                       selected = 0),
                    
                    checkboxGroupInput('support', label='Support', 
                                       choices = list('belt', 'sleeve', 'spotter'),
                                       selected = 0),
                ),
                
            
                
                # NOTES
                box(title = 'Quality and Notes',
                    width=4,
                    
                    
                    checkboxGroupInput('repquality', label='Quality', 
                                       choices = list('warm up', 'amrap / until failure', 'cheating', 'fast', 'slow', 'full range of motion', 'limited range of motion'),
                                       selected = 1),
                    
                    textInput(inputId = 'notes',
                              label = 'Notes'),
                    
                    actionButton('save', 'Save'),
                    actionButton('undosave', 'Undo'))
            ),
            
            # HISTORY
            fluidRow(
                box(title = 'History',
                    width=12,
                    tableOutput("df_data_out"),
                    
                    # SHINY SAVE
                    downloadButton('downloadData', 'Save Workout')
                )
                
            )),
        
        # --------- STATS ------------
        tabItem(tabName = 'statistics',
                
                
                fluidRow(
                    tabBox(title='Statistics',
                           id='recent_alltime_tab',
                           width=12,
                           
                           # --------------------- RECENT -----------------------
                           tabPanel(title='Most Recent',
                                    
                                    fluidRow(
                                    box(title='Your Last Session',
                                        status='warning',
                                        textOutput('ex_date'),
                                        
                                        br(),
                                        
                                        textOutput('ex_list'),
                                        width=6),
                                    
                                    valueBoxOutput('total_time',
                                                   width=3),
                                    
                                    valueBoxOutput('num_exercises',
                                                   width=3)
                                    ),
                                    
                                    br(),
                                    br(),
                                    
                                    fluidRow(
                                        
                                        tabBox(
                                            title = "Sets and Reps",
                                            tabPanel("Sets", plotOutput('recentsets')),
                                            tabPanel("Reps", plotOutput('recentreps'))
                                        ),
                                        
                                        
                                        tabBox(title='Weight',
                                            width=6,
                                            
                                            tabPanel('Weights', plotOutput('recentweights')),
                                            
                                            tabPanel('Max Weights', tableOutput('recentchart'))
                                        
                                        )
                                    )),
                           
                            # ----------------- ALL TIME ---------------------------
                            tabPanel(title = 'All Time',
                                     fluidRow(
                                         box(title='All Sessions',
                                             status='warning',
                                             width=12,
                                             'This tab displays information on your lifting progress over all sessions recorded through Progressive Overload.')
                                     ),
                                     
                                     fluidRow(
                                            
                                         box(title='Heaviest Lifts',
                                             width=12,

                                             uiOutput('prboxes')
                                     )
                                     ),

                                     fluidRow(
                                         box(title='Time Series',
                                             width=4,
                                             status='warning',

                                             uiOutput('completed_exercises')
                                     ),
                                     
                                     box(width=8,
                                         plotOutput('time_series_plot'))
                                     

                                     )
                )
        )))))
                    
                    
                


# --------------- USER INTERFACE ---------------------
ui <- dashboardPage(
    skin = 'yellow',
    header = dashboardHeader(title = app_name),
    sidebar = dashboardSidebar(
        sidebarMenuOutput('menu')
    ),
    body = body
)
