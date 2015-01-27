## 9th Grade Data to reduce suspensions and retention, and to improve
## chances of graduating on time.

# set working directory
setwd("C:/Users/keithg.williams/Documents/9th Grade")

# load packages
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(ggplot2)
library(grid)

## Read in Data
file1 <- "full 9th grade export as of 1-23.csv"
file2 <- "referral data as of 1-23.csv"

master <- read.csv(file1)
refs <- read.csv(file2, stringsAsFactors = FALSE)

## Tidy Data
master <- tbl_df(master)

# select and rename useful columns
master <- master %>% select(sid = ï..StudentSISID2,
                            name = StudentFullNameLastFirst2,
                            gender = Gender2,
                            race = Race2,
                            hispanic = Ethnicity2,
                            lep = EnglishProficiencyCategory2,
                            ec = ExceptionalChildCategory2,
                            birthdate = Birthdate,
                            mcv = McKinneyVentoStatus,
                            DaysAbsentExcused: EOGScDate,
                            EOCE1Test: EOCE1Date,
                            EOCScTest: EOCScDate)

# fix ids and names
master$sid <- as.character(master$sid)
master$name <- sub("'", ",", as.character(master$name))

# fix factor labels in race
levels(master$race) <- c("Asian", "Black", 
                          "Native American", "Multi-Racial", 
                          "Hawaiian/Pacific Islander", "White")

# fix dates
master$birthdate <- mdy(master$birthdate)
master$EOGRdDate <- ymd(master$EOGRdDate)
master$EOGMaDate <- ymd(master$EOGMaDate)
master$EOGScDate <- ymd(master$EOGScDate)
master$EOCE1Date <- ymd(master$EOCE1Date)
master$EOCScDate <- ymd(master$EOCScDate)

# sum absences, sum suspensions
master <- master %>% 
    mutate(tot_absences = DaysAbsentExcused + DaysAbsentUnexcused,
           suspensions = NumDaysISS + NumDaysOSS)

# refs table
refs <- tbl_df(refs)

# extract incident ID, incident title, ActionType2
refs <- separate(refs, ï..SourceIncidentID, c("trash1", "incident_id"), 
                 sep="\n")
refs <- separate(refs, IncidentTitle, c("trash2", "incident_title"), 
                 sep="\n")
refs <- separate(refs, ActionType2, c("sid", "name_ref", "outcome"), 
                 sep=" - ")

# strip white space from sid
refs$sid <- str_trim(refs$sid)

# discard useless columns and rename
refs <- refs %>% select(incident_id,
                        incident_title,
                        incident_date = Textbox13,
                        location = Textbox21,
                        offense_type = OffenseType3,
                        sid:IncidentDetailDescription3)

# count number of incidents in referal data and merge with master
# this should only pull the 9th graders from the referalls data
sumrefs <- refs %>% group_by(sid) %>% summarise(num_referrals = n())
master <- left_join(master, sumrefs, by="sid")

# convert "NA" in num_referrals to "0"
master[is.na(master$num_referrals), "num_referrals"] <- 0

## investigate, race, gender, mcv vs num_referrals
ref_by_race_by_gender <- master %>% 
    group_by(race, gender, num_referrals) %>% 
    summarise(students = n())

# Open PNG device
png(width = 600, height = 600, filename="repeat.png")

# plot
ggplot(filter(ref_by_race_by_gender, num_referrals > 2), 
       aes(race, num_referrals, size=students)) + 
    scale_size_continuous(range=c(2,16)) +
    geom_point(col = "steelblue") + 
    facet_grid(.~gender) + 
    ggtitle("Repeat Referrals by Race and Gender") + 
    theme(axis.title.x = element_text(size = 16),
          axis.text.x = element_text(angle=30, hjust=1, size = 14),
          title = element_text(size = 20),
          axis.title.y = element_text(size = 16)) +
    scale_y_discrete("Number of Referrals", expand=c(.05,-1))

# close PNG device
# dev.off()

# race vs number of referrals, ISS, OSS, suspensions
by_race <- master %>% group_by(race) %>% 
    summarise(n = n(), 
              tot_ref = sum(num_referrals), 
              unique_ref = sum(num_referrals > 0), 
              tot_susp = sum(suspensions),
              unique_susp = sum(suspensions > 0),
              tot_ISS = sum(NumDaysISS),
              unique_ISS = sum(NumDaysISS > 0),
              tot_OSS = sum(NumDaysOSS),
              unique_OSS = sum(NumDaysOSS > 0)) %>%
    mutate(population = n / sum(n), 
           referrals = tot_ref / sum(tot_ref), 
           unique.referrals = unique_ref / sum(unique_ref),
           ISS = tot_ISS / sum(tot_ISS),
           unique.ISS = unique_ISS / sum(unique_ISS),
           OSS = tot_OSS / sum(tot_OSS),
           unique.OSS = unique_OSS / sum(unique_OSS),
           suspensions = tot_susp / sum(tot_susp),
           unique.suspensions = unique_susp / sum(unique_susp)) %>%
    arrange(population)
by_race$race <- factor(by_race$race, levels = by_race$race, ordered=TRUE)
by_race.percent <- by_race %>% select(race, population:unique.suspensions)

# race population vs referrals
percent_refs <- by_race.percent %>%
    select(race, population, referrals, unique.referrals) %>%
    gather("share", "percent", 2:4)
ref_file <- "share-refs.png"
ref_title <- "Share of Referrals by Race"
# race population vs ISS
percent_ISS <- by_race.percent %>%
    select(race, population, ISS, unique.ISS) %>%
    gather("share", "percent", 2:4)
iss_file <- "share-iss.png"
iss_title <- "Share of In School Suspensions by Race"
# race population vs OSS
percent_OSS <- by_race.percent %>%
    select(race, population, OSS, unique.OSS) %>%
    gather("share", "percent", 2:4)
oss_file <- "share-oss.png"
oss_title <- "Share of Out of School Suspensions by Race"
# race population vs suspensions
percent_suspensions <- by_race.percent %>%
    select(race, population, suspensions, unique.suspensions) %>%
    gather("share", "percent", 2:4)
sus_file <- "share-suspensions.png"
sus_title <- "Share of Total Suspensions by Race"

# function for creating plots
plt <- function(filename, df, title) {
    png(width=600, height=600, filename=filename)
    ggplot(data=df, aes(race, percent * 100, fill=share)) + 
        geom_bar(stat="identity", position="dodge") +
        scale_fill_brewer(palette = "Set2") +
        ggtitle(title) + 
        ylab("percent") +
        theme(axis.title.x = element_text(size = 16),
              axis.text.x = element_text(angle=30, hjust=1, size = 14),
              title = element_text(size = 20),
              axis.title.y = element_text(size = 16),
              plot.margin = unit(c(0.5,0.3,0.3,2), "cm"))
}

# create plots
plt(ref_file, percent_refs, ref_title)
plt(iss_file, percent_ISS, iss_title)
plt(oss_file, percent_OSS, oss_title)
plt(sus_file, percent_suspensions, sus_title)

# gender population vs suspensions
by_gender <- master %>% group_by(gender) %>% 
    summarise(n=n(), 
              tot_ref = sum(num_referrals), 
              tot_susp = sum(suspensions)) %>% 
    mutate(population = n / sum(n), 
           referrals = tot_ref / sum(tot_ref), 
           suspensions = tot_susp / sum(tot_susp))

pct_gender <- by_gender %>% 
    select(gender, population:suspensions) %>% 
    gather("share", "percent", 2:4)

png(width = 600, height = 600, filename="share-gender.png")
ggplot(pct_gender, aes(gender, percent * 100, fill=share)) + 
    geom_bar(stat="identity", position="dodge") + 
    scale_fill_brewer(palette = "Set2") +
    ggtitle("Share of Total Suspensions by Gender") +
    ylab("percent") +
    theme(axis.title.x = element_text(size = 16),
          axis.text.x = element_text(size = 20),
          title = element_text(size = 20))

# MCV status vs suspensions
by_mcv <- master %>% group_by(mcv) %>% 
    summarise(n=n(), 
              tot_ref = sum(num_referrals), 
              tot_susp = sum(suspensions)) %>% 
    mutate(population = n / sum(n), 
           referrals = tot_ref / sum(tot_ref), 
           suspensions = tot_susp / sum(tot_susp))

pct_mcv <- by_mcv %>% 
    select(mcv, population:suspensions) %>% 
    gather("share", "percent", 2:4)

png(width=600, height=600, filename="share-mcv.png")
ggplot(pct_mcv, aes(mcv, percent * 100, fill=share)) + 
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_brewer(palette = "Set2") +
    ggtitle("Share of Total Suspensions by MCV Status") +
    ylab("percent") +
    theme(axis.title.x = element_text(size = 16),
          axis.text.x = element_text(size = 20),
          title = element_text(size = 20))

# attendance shows no correlation
# ggplot(data = filter(master, tot_absences < 30), 
#        aes(tot_absences - suspensions, num_referrals)) + 
#     geom_point(col="steelblue", size=7, alpha = 0.2) + 
#     geom_smooth(method="lm", col="darkred", se=F, lwd=1) + 
#     labs(x = "Total Absences Not Due to Suspension", 
#          y = "Number of Referrals") + 
#     ggtitle("Absences vs Number of Referrals") + 
#     theme(axis.title.x = element_text(size = 14), 
#           axis.title.y = element_text(size = 14), 
#           title = element_text(size = 20)) + 
#     scale_y_discrete(expand=c(.05,0))

# EOG Reading Achievement Correlation
by_race_EOG <- master %>% 
    group_by(EOGRdAch, race, num_referrals) %>% 
    summarise(n = n())

# ggplot(by_race_EOG, aes(EOGRdAch, num_referrals, size = log10(n))) + 
#     geom_point(col="steelblue") + 
#     facet_grid(.~race) + 
#     geom_smooth(method="lm", se=F, col = "darkred", lwd = 1) + 
#     scale_y_discrete(expand=c(0.05, -1)) + 
#     labs(x = "EOG Reading Achievement Level", 
#          y = "Number of Referrals") + 
#     ggtitle("EOG Achievement Correlation to Referrals") + 
#     theme(axis.title.x = element_text(size=14), 
#           axis.title.y = element_text(size = 14), 
#           title = element_text(size=20))

# location vs race
ref.dem <- inner_join(refs, 
                      select(master, sid, race, gender, mcv), 
                      by="sid")

by_loc <- ref.dem %>% 
    group_by(location, race) %>% 
    summarise(n = n()) %>% 
    mutate(pct = n / sum(n))

# ggplot(by_loc, aes(race, pct * 100, fill=race)) + 
#     geom_bar(stat="identity") + 
#     scale_fill_brewer(palette = "Set2") + 
#     facet_grid(.~location) + 
#     labs(x="Race", y="Percent") + 
#     ggtitle("Percent of Referrals by Location and Race") + 
#     scale_x_discrete(labels=NULL) + 
#     theme(axis.title.y = element_text(size=16), 
#           axis.title.x = element_text(size=16), 
#           title = element_text(size=20))
# 
# ggplot(by_loc, aes(race, n, fill=race)) + 
#     geom_bar(stat="identity") + 
#     scale_fill_brewer(palette = "Set2") + 
#     facet_grid(.~location) + 
#     labs(x="Race", y="Number of Referrals") + 
#     ggtitle("Number of Referrals by Location and Race") + 
#     scale_x_discrete(labels=NULL) + 
#     theme(axis.title.y = element_text(size=16), 
#           axis.title.x = element_text(size=16), 
#           title = element_text(size=20))

# classroom and other referrals
class_other <- ref.dem %>% 
    group_by(sid, location, race) %>% 
    summarise(n=n()) %>% 
    filter(location == "Classroom" | 
               location == "Other location in building")

class_other_unique <- class_other %>% 
    group_by(race, location) %>% 
    summarise(n = n())

ggplot(class_other_unique, aes(race, n, fill=race)) + 
    geom_bar(stat="identity") + 
    scale_fill_brewer(palette = "Set2") + 
    facet_grid(.~location) + 
    labs(x="Race", y="Number of Students") + 
    ggtitle("Students with at least One Referral") + 
    scale_x_discrete(labels=NULL) + 
    theme(axis.title.y = element_text(size=16), 
          axis.title.x = element_text(size=16), 
          title = element_text(size=20))

# Combine Classroom/Gym, change "Other..." to "Lockout"
class_lo <- ref.dem %>% 
    group_by(sid, location, race, gender) %>% 
    summarise(n=n()) %>% 
    filter(location %in% c("Classroom", 
                           "Gym", 
                           "Other location in building"))

class_lo$location <- sub("Other location in building", "Lockout", 
                         class_lo$location)
class_lo$location <- sub("Gym", "Classroom", class_lo$location)

levels(class_lo$gender) <- c("Female", "Male")

class_lo_unique <- class_lo %>% 
    group_by(race, gender, location) %>% 
    summarise(n=n())

ggplot(class_lo_unique, aes(race, n, fill=race)) + 
    geom_bar(stat="identity") + 
    scale_fill_brewer(palette = "Set2") + 
    facet_grid(gender~location) + 
    labs(x="Race", y="Number of Students") + 
    ggtitle("Students with at least One Referral") + 
    scale_x_discrete(labels=NULL) + 
    theme(axis.title.y = element_text(size=16), 
          axis.title.x = element_text(size=16), 
          title = element_text(size=20))

# ggplot(filter(class_other, n > 2), aes(race, n)) + 
#     geom_point(alpha=0.2, size=10) + 
#     facet_grid(.~location) + 
#     scale_y_discrete(expand=c(.05, -1))

count_loc <- class_lo %>% 
    group_by(n, location, race, gender) %>% 
    summarise(count = n())

ggplot(count_loc, aes(race, n, size = count)) + 
    scale_size_continuous(range=c(3, 24)) + 
    geom_point(col = "steelblue") + 
    facet_grid(gender~location) + 
    ggtitle("Referrals by Race in Classroom and Building") + 
    scale_y_discrete() + labs(y="Number of Referrals") + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1), 
          axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16), 
          title = element_text(size = 20))

# close devices
dev.off()
dev.off()
dev.off()
dev.off()
dev.off()
dev.off()
dev.off()