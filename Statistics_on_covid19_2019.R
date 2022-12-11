COVID19_line_list_data <- read_excel("~/COVID19_line_list_data.xlsx",
                   col_types = c("numeric", "numeric", "text",
                                 "numeric", "text", "text", "text",
                                 "text", "numeric", "text", "text",
                                 "text", "text", "text", "numeric",
                                 "text", "text", "text", "text", "text",
                                 "text"))
View(COVID19_line_list_data)

data <- read_excel("~/COVID19_line_list_data.xlsx",
                                     col_types = c("numeric", "numeric", "text",
                                                   "numeric", "text", "text", "text",
                                                   "text", "numeric", "text", "text",
                                                   "text", "text", "text", "numeric",
                                                   "text", "text", "text", "text", "text",
                                                   "text"))
View(COVID19_line_list_data)
library(Hmisc)#import

describe(data) #hmisc command

#cleaned up death data
data$death_dummy = as.integer(data$death != 0)

#finding death rate
sum(data$death_dummy) / nrow(data) #rate 5.8%

#age of people who die
#people who die are older
dead = subset(data, death_dummy == 1) #63
alive = subset(data, death_dummy == 0) #1022
mean(dead$age, na.rm = TRUE) #68.6
mean(alive$age, na.rm = TRUE) #48.1
#is range of death significant
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)
#p-value < 0.05 so we reject null hypothesis, so this is
#statistically significant

#gender of people who die
#does gender have an effect
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) #8.4%
mean(women$death_dummy, na.rm = TRUE) #3.7%
#is gender significant
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.99)
# 99% confidence: men have from 0.8% to 8.8% higher chance of dying
#p-value = 0.002 < 0.05, so this is statistically significant
