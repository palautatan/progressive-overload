# ---------- STATIC ---------------
app_name <- 'Progressive Overload'
full_name <- 'Edie Espejo'
last_name <- strsplit(full_name, ' ')[[1]][2]
# number_of_workouts <- 10
average_sessions <- 3
all_exercises <- c('bicep curl', 'deadlift', 'deadlift, Romanian', 'deadlift, sumo',
                   'deadlift, single leg', 'hammer curl',
                   'kettlebell swings',
                   'shoulder press', 'rows, bent over', 
                   'squat', 'squat, narrow', 'squat, sumo')

all_equipment <- c('barbell', 'dumbbell, pair', 'dumbbell, single', 'EZ curl bar', 
                   'kettlebell', 'olympic bar', 'resistance band')

welcome <- paste0('Welcome back, ', full_name, '!')