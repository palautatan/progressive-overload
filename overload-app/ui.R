library(shiny)
library(shinydashboard)
library(shinyFiles)


# ---------- STATIC ---------------
app_name <- 'Progressive Overload'
full_name <- 'Edie Espejo'
number_of_workouts <- 10
average_sessions <- 3
all_exercises <- c('bicep curl', 'deadlift', 'deadlift, Romanian', 'deadlift, sumo',
                   'deadlift, single leg', 'hammer curl',
                   'kettlebell swings',
                   'shoulder press', 'rows, bent over', 
                   'squat', 'squat, narrow', 'squat, sumo')

all_equipment <- c('barbell', 'dumbbell, pair', 'dumbbell, single', 'EZ curl bar', 
                   'kettlebell', 'olympic bar', 'resistance band')

welcome <- paste0('Welcome back, ', full_name, '!')



last_name <- strsplit(full_name, ' ')[[1]][2]



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
                
                valueBox(number_of_workouts,
                         width=3,
                         'Sessions to Date',
                         icon = icon('check'),
                         color = 'green'),
                
                valueBox(average_sessions,
                         width=3,
                         'Average Sessions Per Week',
                         icon = icon('check'),
                         color = 'green'),
                
            ),
            
            fluidRow(
                box(title = 'Locate Workout Files',
                    width=4,
                    
                    shinyDirButton("dir", "Input directory", "Upload"),
                    br(),
                    br(),
                    htmlOutput("wd"),
                    textOutput('dir')),
                
                box(title = 'Previous Session',
                    width=6,
                    'You did the following exercises during your last session.')
                
                
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
                box(title = 'Achievements',
                    status = 'warning',
                    width=12))
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
