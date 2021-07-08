event_df <- read.csv("events_data.csv")
tutorials_df <- read.csv("tutorial_agenda.csv")

# 1. Calendar summary
#library(dplyr)
#library(stringr)
smry_calendar <- event_df %>%
  group_by(Calendar.Name) %>%
  summarise(Count = n())
colnames(smry_calendar)[1] <- "Name" 

regular_talks <- smry_calendar %>%
  filter(str_detect(Name, "Regular talks"))
rt_count <- sum(regular_talks$Count)
smry_calendar <- smry_calendar %>% add_row(Name = "Regular talks", Count = rt_count)
smry_calendar <- smry_calendar[!(smry_calendar$Name=="Tutorials"),]

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

tut_speakers <- tutorials_df$instructors[1:nrow(tutorials_df)]
tut_speakerlist <- paste(tut_speakers, collapse = ",")
tut_speakerlist <- gsub("\n", " ", tut_speakerlist)
tut_speakerlist <- gsub(";", ",", tut_speakerlist)
tut_speakervector <- strsplit(tut_speakerlist, ",")[[1]]
tut_speakertable <- table(trimws(tut_speakervector))
tut_speakerdf <- as.data.frame(tut_speakertable)
colnames(tut_speakerdf) <- c("Instructors", "Frequency")
tut_speakerdf <- tut_speakerdf[order(tut_speakerdf$Frequency, decreasing = TRUE), ]
no_of_instr <- nrow(tut_speakerdf)


# 5. Popular events tags
events_tags <- event_df %>%
  group_by(Tags) %>%
  summarise(Count = n())
events_tags<-events_tags[!(events_tags$Tags=="Break"),]
events_tags <- events_tags[order(events_tags$Count, decreasing = TRUE), ]