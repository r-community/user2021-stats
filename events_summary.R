event_df <- read.csv("events_data.csv")
tutorials_df <- read.csv("tutorial_agenda.csv")

# 1. Calendar summary
#library(dplyr)
smry_calendar <- event_df %>%
  group_by(Calendar.Name) %>%
  summarise(Count = n())
colnames(smry_calendar)[1] <- "Name" 

#library(stringr)
regular_talks <- smry_calendar %>%
  filter(str_detect(Name, "Regular talks"))
rt_count <- sum(regular_talks$Count)
smry_calendar <- smry_calendar %>% add_row(Name = "Regular talks", Count = rt_count)


# 2. Authors
events_speakers <- event_df$Who[1:nrow(event_df)]
speakerlist <- paste(events_speakers, collapse = ",")
speakerlist <- gsub(";", ",", speakerlist)
speakervector <- strsplit(speakerlist, ",")[[1]]
speakertable <- table(trimws(speakervector))
speakerdf <- as.data.frame(speakertable)
colnames(speakerdf) <- c("Speaker", "Freq")
speakerdf <- speakerdf[-1,]
number_of_speakers <- length(speakerdf$Speaker) - 1
speakerdf <- speakerdf[order(speakerdf$Freq, decreasing = TRUE), ]


# 3. Number of Sessions
session_df <- event_df %>%
  group_by(Session.name) %>%
  summarise(Count = n())
session_df<-session_df[!(session_df$Session.name=="Break"),]
no_of_sess <- sum(session_df$Count)


# 4. Tutorials Summary
no_of_tut <- nrow(tutorials_df)
avg_tut_duration <- floor(sum(tutorials_df$duration)/no_of_tut)
avg_tut_attendees <- floor(sum(tutorials_df$attendees)/no_of_tut)
lang_count <- length(unique(tutorials_df$language))
