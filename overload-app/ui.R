

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
                    
                    tableOutput('previous_exercises'))
                
                
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
                    width=3,
                    selectInput(inputId = 'exercise',
                                label = 'Exercise',
                                all_exercises)
                ),
                
                # EXERCISE
                box(title='Equipment',
                    width=3,
                    selectInput(inputId = 'equipment',
                                label = 'Equipment',
                                all_equipment)
                ),
                
                # WEIGHT
                box(title = 'Weight',
                    width = 2,
                    numericInput(inputId = 'weight',
                                label = 'Total Weight (lbs)',
                                min = 1,
                                max = 500,
                                value = 20,
                                step=1)
                ),
                
                # REPS
                box(title = 'Reps',
                    width = 2,
                    numericInput(inputId = 'reps',
                                label = 'Repetitions',
                                min = 1,
                                max = 100,
                                value = 12,
                                step=1)
                ),
                
                
                # NOTES
                box(title = 'Notes',
                    width=2,
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
                           
                           tabPanel(title='Most Recent',
                                    
                                    fluidRow(
                                    box(status='warning',
                                        'Your last session was on [insert date here].',
                                        
                                        br(),
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
                    
                            tabPanel(title = 'All Time',
                                     'Under Construction.'
                                     )
                )
        ))))
                    
                    
                


# --------------- USER INTERFACE ---------------------
ui <- dashboardPage(
    skin = 'yellow',
    header = dashboardHeader(title = app_name),
    sidebar = dashboardSidebar(
        sidebarMenuOutput('menu')
    ),
    body = body
)
