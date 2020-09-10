library(ggplot2)
library(dplyr)


# workouts <- list.files(paste0('profiles/', last_name), full.names=TRUE)
workouts <- list.files()
workouts <- sort(workouts, decreasing=TRUE)

number_of_workouts <- length(workouts)

workout_info <- lapply(workouts, readr::read_csv)

# FOR EACH WORKOUT
this_workout <- workout_info[[1]]


# ggplot(aes(x=reorder(exercise, exercise, function(x) -length(x)))) +

this_workout %>%
  filter(exercise != 'session start') %>%
  group_by(exercise, equipment) %>%
  summarize(`Max Total Weight (lbs)`=max(weight))

this_workout %>%
  filter(exercise != 'session start') %>%
    ggplot(aes(x=exercise)) +
    geom_bar() +
    ylab('sets') +
    xlab('exercise') +
    ggtitle('Number of Sets Per Exercise') +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90))

this_workout %>%
  filter(exercise != 'session start') %>%
  group_by(exercise) %>%
  summarize(mean_rep=mean(repetitions)) %>%
  ggplot(aes(x=exercise, y=mean_rep)) +
  geom_bar(stat='identity') +
  ylab('sets') +
  xlab('exercise') +
  ggtitle('Number of Reps Per Exercise') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))


this_workout %>%
  filter(exercise != 'session start') %>%
  group_by(exercise) %>%
  summarize(mean_rep=mean(repetitions)) %>%
  ggplot(aes(x=exercise, y=mean_rep)) +
  geom_bar(stat='identity') +
  ylab('sets') +
  xlab('exercise') +
  ggtitle('Number of Reps Per Exercise') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))

this_workout %>%
  filter(exercise != 'session start') %>%
  ggplot(aes(x=exercise, y=weight)) +
    geom_count() +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(size='sets') +
    ggtitle('Total Weight per Exercise')


workout_dates <- lapply(workout_info,
       function(k) {
         k %>% filter(exercise=='session start') %>% pull(date)
       })


per_week <- table(unlist(lapply(workout_dates, function(k) cut(k, 'week'))))



total_time <- workout_info[[1]] %>%
  summarize(start=tail(time, 1),
            end=head(time, 1)) %>%
  summarize(mins=difftime(end, start, units='mins')) %>%
  summarize(m=as.numeric(round(mins)),
            s=as.numeric(mins-round(mins))*60) %>%
  paste0(collapse=':')


