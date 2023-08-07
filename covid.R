rm(list=ls()) # removes all variables stored previously
library(Hmisc)

data <- read_csv("COVID19_line_list_data.csv")
describe(data) #Hmisc command


#cleaned up death column
data$death_dummy <- as.integer(data$death != 0)
# death rate
sum(data$death_dummy) / nrow(data)

#Age
#Hypothesis: People who die from COVID-19 are older.

dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)

# because some entries in the age column say NA we need to clean up the Age column
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

# Is this statistically significant?
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.95)

# Interpretation of results:
# There's a 95% chance the difference between a dead and alive person is between 24 and 16 years old. Therefore, a person who is alive is very young.
# Looking at the p-value we can see that 2.2e-16 is actually equal to 0. This means that there is 0% chance that the ages in the two populations are actually equal in the population(sample).
# Normally when p-value < 0.5, we reject null hypothesis. 
# Since our p-value is = 0, then we can conclude that this is significantly significant and people who die from COVID-19 are in fact older.

# GENDER
# Hypothesis:
women = subset(data, gender == "female")
men = subset(data, gender == "male")

mean(women$death_dummy, na.rm = TRUE) #3.7%
mean(men$death_dummy, na.rm = TRUE) #8.5% 

#Is this statistically significant?
t.test(women$death_dummy, men$death_dummy, alternative="two.sided", conf.level = 0.99)

#Interpretation of results
# 99% confidence level - men have .8% to 8.8% higher chance of dying from COVID-19
# p-value = 0.002 < 0.05 so this is statistically significant
# Based on the p-value we can conclude that, Men have higher death rates than women in the sample and is representative of the population.