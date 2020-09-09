library(shiny)
library(shinydashboard)
library(shinyFiles)











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
        
        # --------- ACHIEVEMENTS ------------
        tabItem(tabName = 'achievements',
                
                fluidRow(
                    box(title = 'Achievements',
                    status = 'warning',
                    width=12)),
                
                fluidRow(box(title='Most Recent Session',
                             width=12,
                             tableOutput('recentchart')), 
                    
                    ),
                
                fluidRow(
                    
                    tabBox(
                        title = "Sets and Reps",
                        # The id lets us use input$tabset1 on the server to find the current tab
                        id = "tabset1", height = "250px",
                        tabPanel("Recent Sets", plotOutput('recentsets')),
                        tabPanel("Recent Reps", plotOutput('recentreps'))
                    ),
                    
                    
                    box(title='Recent Weights',
                        width=6,
                        plotOutput('recentweights')),
                    
                    
                    )
                
                )
    )
)


# --------------- USER INTERFACE ---------------------
ui <- dashboardPage(
    skin = 'yellow',
    header = dashboardHeader(title = app_name),
    sidebar = dashboardSidebar(
        sidebarMenuOutput('menu')
    ),
    body = body
)
