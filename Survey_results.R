sdat <- read.csv("urbannature.csv", header = TRUE)
view(sdat)
view(thesis_survey)

library(tidyverse)

install.packages("knitr")
install.packages("lattice")
install.packages("tidyverse")
install.packages("likert")
install.packages("MASS")
install.packages("psych")
install.packages("viridis")
install.packages("ggplot2")
install.packages("here")
install.packages("flextable")
install.packages("devtools")
install.packages("GPArotation")

# devtools::install_github("matherion/userfriendlyscience", dependencies=T)
install.packages("ufs")
# install klippy for copy-to-clipboard button in code chunks
install.packages("remotes")
remotes::install_github("rlesur/klippy")

# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # suppress math annotation
# install packages
library(knitr)
library(lattice)
library(tidyverse)
library(likert)
library(vegan)
library(MASS)
library(psych)
library(viridis)
library(ggplot2)
library(here)
library(flextable)
library(devtools)
library(userfriendlyscience)
library(ufs)
# activate klippy for copy-to-clipboard button
klippy::klippy()

library(stringr)
## Clean column Names: 
library(stringr)

## URBAN NATURE ANALYSIS 

# clean column names
colnames(sdat)[3:ncol(sdat)] <- paste0("Q ", str_pad(seq_along(colnames(sdat)[3:ncol(sdat)]), 2, "left", "0"), ": ", colnames(sdat)[3:ncol(sdat)]) %>%
  stringr::str_replace_all("\\.", " ") %>%
  stringr::str_squish() %>%
  stringr::str_replace_all("\\s+", " ") %>%
  stringr::str_replace_all("(?<=[^\\s])$", "?")

# inspect column names
colnames(sdat)


lbs <- c("Strongly Agree", "Agree", "Neutral",  "Disagree", "Strongly Disagree")
survey <- sdat %>% dplyr::mutate_if(is.character, factor) %>% dplyr::mutate_if(is.numeric, factor, levels = 1:5, labels = lbs) %>% as.data.frame()



library(likert)
library(ggplot2)

# create the plot
likert_plot <- plot(likert(survey[,6:10]), ordered = F, wrap= 60)

survey_p1 <- likert_plot + theme(axis.text.y = element_text(size = 12))

survey_p1

# save plot
cowplot::save_plot(here("images", "stu_p1.png"), # where to save the plot
                   survey_p1,        # object to plot
                   base_asp = 1.5,  # ratio of space fro questions vs space for plot
                   base_height = 8)

## Separate based on age
plot(likert(survey[,6:9], grouping = survey[,1]))

## separate based on gender
plot(likert(survey[,6:9], grouping = survey[,2]))

## Evaluating the reliability of questions

surveydata <- sdat
str(sdat)
# calculate cronbach's alpha
Cronbach <- psych::alpha(surveydata[c("Q 04: We have an obligation to preserve urban nature for future generations?",   
                                      "Q 05: Preserving urban nature is essential to tackling climate change?",  
                                      "Q 06: Our health and wellbeing depend on nature?",  
                                      "Q 07: The green space in Dublin City is very important to me?")], check.keys=F)
# inspect results
Cronbach

library(psych)
my_data <- surveydata[, c("Q 04: We have an obligation to preserve urban nature for future generations?", "Q 05: Preserving urban nature is essential to tackling climate change?", "Q 06: Our health and wellbeing depend on nature?", "Q 07: The green space in Dublin City is very important to me?")]
omega(my_data)

## URBANISATION ANALYSIS

udat <- read.csv("urbanisation.csv", header = TRUE)
View(udat)


library(tidyverse)

# clean column names
colnames(udat)[3:ncol(udat)] <- paste0("Q ", str_pad(seq_along(colnames(udat)[3:ncol(sdat)]), 2, "left", "0"), ": ", colnames(udat)[3:ncol(udat)]) %>%
  stringr::str_replace_all("\\.", " ") %>%
  stringr::str_squish() %>%
  stringr::str_replace_all("\\s+", " ") %>%
  stringr::str_replace_all("(?<=[^\\s])$", "?")

# inspect column names
colnames(udat)


lbs <- c("Strongly Agree", "Agree", "Neutral",  "Disagree", "Strongly Disagree")
survey <- udat %>% dplyr::mutate_if(is.character, factor) %>% dplyr::mutate_if(is.numeric, factor, levels = 1:5, labels = lbs) %>% as.data.frame()



library(likert)
library(ggplot2)

# create the plot
likert_plot <- plot(likert(survey[,6:9]), ordered = F, wrap= 60)
likert_plot

survey_p2 <- likert_plot + theme(axis.text.y = element_text(size = 12))
survey_p2

# save plot
cowplot::save_plot(here("images", "stu_p1.png"), # where to save the plot
                   survey_p1,        # object to plot
                   base_asp = 1.5,  # ratio of space fro questions vs space for plot
                   base_height = 8)

## Separate based on age
plot(likert(survey[,6:9], grouping = survey[,1]))

## separate based on gender
plot(likert(survey[,6:9], grouping = survey[,2]))

## separate based on relationship to dublin
plot(likert(survey[,6:9], grouping = survey[,4]))


## Evaluating the reliability of questions

surveydata1 <- udat
str(udat)
# calculate cronbach's alpha
Cronbach <- psych::alpha(surveydata1[c("Q 04: X Available space in Dublin City should be returned to nature?",   
                                      "Q 05: X Available space in Dublin City should be used to build more houses apartments?",  
                                      "Q 06: X Available space in Dublin City should be used to increase green spaces for sport and recreation?",  
                                      "Q 07: X Available space in Dublin City should be used for urban agriculture or community gardens farms?")], check.keys=T)
# inspect results
Cronbach





## BENEFITS


bdat <- read.csv("benefits.csv", header = TRUE)
View(bdat)
str(bdat)

library(tidyverse)

# clean column names
colnames(bdat)[3:ncol(bdat)] <- paste0("Q ", str_pad(seq_along(colnames(bdat)[3:ncol(bdat)]), 2, "left", "0"), ": ", colnames(bdat)[3:ncol(bdat)]) %>%
  stringr::str_replace_all("\\.", " ") %>%
  stringr::str_squish() %>%
  stringr::str_replace_all("\\s+", " ") %>%
  stringr::str_replace_all("(?<=[^\\s])$", "?")

# inspect column names
colnames(bdat)


lbs <- c("Strongly Agree", "Agree", "Neutral",  "Disagree", "Strongly Disagree")
survey <- bdat %>% dplyr::mutate_if(is.character, factor) %>% dplyr::mutate_if(is.numeric, factor, levels = 1:5, labels = lbs) %>% as.data.frame()



library(likert)
library(ggplot2)

# create the plot
likert_plot <- plot(likert(survey[,3:11]), ordered = F, wrap= 60)
likert_plot

survey_p2 <- likert_plot + theme(axis.text.y = element_text(size = 12))
survey_p2

# save plot
cowplot::save_plot(here("images", "stu_p1.png"), # where to save the plot
                   survey_p1,        # object to plot
                   base_asp = 1.5,  # ratio of space fro questions vs space for plot
                   base_height = 8)

## Separate based on age
plot(likert(survey[,3:11], grouping = survey[,1]))

## separate based on gender
plot(likert(survey[,3:11], grouping = survey[,2]))


## Negatives


ndat <- read.csv("negatives.csv", header = TRUE)
View(ndat)
str(bdat)

library(tidyverse)

# clean column names
colnames(ndat)[3:ncol(ndat)] <- paste0("Q ", str_pad(seq_along(colnames(ndat)[3:ncol(ndat)]), 2, "left", "0"), ": ", colnames(ndat)[3:ncol(ndat)]) %>%
  stringr::str_replace_all("\\.", " ") %>%
  stringr::str_squish() %>%
  stringr::str_replace_all("\\s+", " ") %>%
  stringr::str_replace_all("(?<=[^\\s])$", "?")

# inspect column names
colnames(ndat)


lbs <- c("Strongly Agree", "Agree", "Neutral",  "Disagree", "Strongly Disagree")
survey <- ndat %>% dplyr::mutate_if(is.character, factor) %>% dplyr::mutate_if(is.numeric, factor, levels = 1:5, labels = lbs) %>% as.data.frame()



library(likert)
library(ggplot2)

# create the plot
likert_plot <- plot(likert(survey[,3:11]), ordered = F, wrap= 60)
likert_plot

survey_p2 <- likert_plot + theme(axis.text.y = element_text(size = 12))
survey_p2

# save plot
cowplot::save_plot(here("images", "stu_p1.png"), # where to save the plot
                   survey_p1,        # object to plot
                   base_asp = 1.5,  # ratio of space fro questions vs space for plot
                   base_height = 8)

## Separate based on age
plot(likert(survey[,3:11], grouping = survey[,1]))

## separate based on gender
plot(likert(survey[,3:11], grouping = survey[,2]))



## Benefits 

b <- read.csv("UWS_benefitz.csv")
view(mydata)

library(ggplot2)

library(ggplot2)


## BAR CHART OF UWS TYPES
types_UWS <- read.csv("types_UWS.csv")

ggplot(types_UWS, aes(x = UWS_type, y = frequency)) + 
  geom_bar(stat = "identity", fill = "palegreen4") +
  theme(panel.background = element_rect(fill = "white"))

ggplot(types_UWS, aes(x = UWS_type, y = frequency)) + 
  geom_bar(stat = "identity", fill = "palegreen4") +
  geom_text(aes(label=frequency, x=UWS_type, y=frequency), 
            hjust = -0.1, size=3) +
  coord_flip() +
  theme(panel.background = element_rect(fill = "white"))

ggplot(types_UWS, aes(x = UWS_type, y = frequency)) + 
  geom_bar(stat = "identity", fill = "gold1") +
  geom_text(aes(label=frequency, x=UWS_type, y=frequency), 
            hjust = -0.1, size=3) +
  coord_flip() +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.line.y = element_line(color = "black"), axis.text.y = element_text(face = "bold", size = 12), axis.text.x = element_text(face = "bold", size = 12))

UWS_mgmt <- read.csv("UWS_mgmt.csv")
# Load the dplyr package
library(dplyr)
str(UWS_mgmt)


library(ggplot2)

ggplot(UWS_mgmt, aes(x = reorder(Preferred.Use, -Frequency), y = Frequency)) + 
  geom_bar(stat = "identity", fill = "gold1", width = 0.5) +
  geom_text(aes(label = Frequency), vjust = -0.5) +
  labs(x = "Preferred use for a suitably large UWS in Dublin", y = "Frequency") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 1, color = "black"),
        axis.text = element_text(face = "bold"))




