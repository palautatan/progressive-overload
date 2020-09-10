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
    geom_count(aes(col=weight)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(size='sets') +
    ggtitle('Total Weight per Exercise')  +
    expand_limits(y = 0) +
    guides(size = guide_legend(override.aes = list(color='#363636'))) +
    scale_colour_gradientn(colours=c('goldenrod', '#DB70DB', '#9F5F9F'), guide = FALSE)


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



a1 <- all_history %>%
  filter(exercise != 'session start') %>%
  group_by(date, exercise, weight) %>%
  summarize(`average reps`=mean(repetitions))

a2 <- a1 %>%
  group_by(date, exercise) %>%
  summarize(minw=min(weight),
            maxw=max(weight)) 




merge(a1, a2) %>%
  ggplot(aes(x=date, y=weight)) +
  geom_segment(aes(x = date, xend = date, y = minw, yend = maxw)) +
  geom_point(aes(col=`average reps`)) +
  facet_wrap(~ exercise) +
  expand_limits(y = 0)  +
  guides(size = guide_legend(override.aes = list(color='#363636'))) +
  scale_colour_gradientn(colours=c('orange', '#EDCB62', '#483D8B', '#473C8B')) +
  theme_classic()

