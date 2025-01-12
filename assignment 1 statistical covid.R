# load the library every time restart
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(skimr)
library(reshape2)

########## EDA ####################

#insert the csv file
covid <- read.csv("C:\Users\senhe\OneDrive\Documents\Year 2\Statistical\cases_state.csv")


library(readxl)
covid <- read_excel("C:/Users/8004528/Documents/sem 3/statistical inteference and modeling/assginment/dataset/cases_state.xlsx")


#show first 6 rows of data
head(covid)
#summary and visualize of data
skim(covid)
#check for duplicates
which(duplicated(covid))
#check for missing values
sum (is.na(covid))
#change "date"to date type
covid$date <- as.Date(covid$date)
covid
#remove unnecessary variables
columns_to_keep <- c("date", "state", "cases_active", "cases_new", "cases_pvax", "cases_unvax",
                     "cases_fvax", "cases_child", "cases_adolescent", "cases_adult",
                     "cases_elderly") 
covid <- subset(covid, select = columns_to_keep)
glimpse(covid)

# plot the bar charts using ggplot
agg_data <- aggregate(cases_adult ~ state, data = covid, sum)
ggplot(agg_data, aes(x = reorder(state, cases_adult), y = cases_adult, fill = state)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Histogram of Adult Cases by State", x = "State", y = "Frequency")

#create boxplot
covid_long <- melt(covid, id.vars = "cases_new", 
                   measure.vars = c( "cases_child", "cases_adolescent",
                                     "cases_adult", "cases_elderly"),
                   variable.name = "age_group", value.name = "cases_new")
boxplot(cases_new ~ age_group, data = covid_long, 
        main = "Boxplot of New Cases by Age Group",
        xlab = "Age Group",
        ylab = "New Cases",
        col = c("lightblue", "lightgreen", "lightcoral"),
        border = "black")

##############
#Plot bar chart for state analysis
agg_data <- aggregate(cases_adult ~ state, data = covid, sum)

agg_data$state <- factor(agg_data$state, levels = agg_data$state[order(-agg_data$cases_adult)])

ggplot(agg_data, aes(x = state, y = cases_adult, fill = state)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Number of Active Cases in Malaysia by State", x = "State", y = "Active Cases")

##############
# Plot multiple bar chart for age group analysis
age_group_data <- covid %>%
  select(date, state, cases_child, cases_adolescent, cases_adult, cases_elderly)

melted_data <- melt(age_group_data, id.vars = c("date", "state"), variable.name = "age_group", value.name = "cases")

ggplot(melted_data, aes(x = age_group, y = cases, fill = age_group)) +
  geom_bar(stat = "identity") +
  facet_wrap(~state, scales = "free_y") +
  labs(title = "Distribution of COVID-19 Across Age Groups in All States of Malaysia",
       x = "Age Group",
       y = "Number of Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        legend.position = "top")  

##############
# Plot multiple bar chart for vaccination status analysis
vaccination_status_data <- covid %>%
  select(date, state, cases_unvax, cases_pvax, cases_fvax)

# Melt the data frame to long format for easier plotting
melted_data_vaccination <- melt(vaccination_status_data, id.vars = c("date", "state"), variable.name = "vaccination_status", value.name = "cases")

# Create a bar plot with rotated x-axis labels
ggplot(melted_data_vaccination, aes(x = vaccination_status, y = cases, fill = vaccination_status)) +
  geom_bar(stat = "identity") +
  facet_wrap(~state, scales = "free_y") +
  labs(title = "Analyzing COVID-19 Impact: Vaccination Status Disparities Among Malaysian States",
       x = "Vaccination Status",
       y = "Number of Active Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "top")  # Adjust legend position

##############


############ statistic method ##################

#the correlation matrix
correlation_matrix <- cor(covid[, c("cases_new", "cases_active", "cases_unvax","cases_pvax",
                                    "cases_fvax", "cases_child", "cases_adolescent",
                                    "cases_adult", "cases_elderly")])
print(correlation_matrix)

#plot for the graph
plot(x=covid$cases_new, y=covid$cases_adult, 
     xlab = "New Cases", ylab = "Adult Cases",
     main = "Scatter Plot of New Cases vs. Adult Cases")
abline(lm(cases_adult ~ cases_new, covid), col = "red")
##############
plot(x=covid$cases_new, y=covid$cases_elderly,
     xlab = "New Cases", ylab = "Elderly Cases",
     main = "Scatter Plot of New Cases vs. Elderly Cases")
abline(lm(cases_elderly ~ cases_new, covid), col = "blue")
##############
plot(x=covid$cases_new, y=covid$cases_adolescent,
     xlab = "New Cases", ylab = "Adolescent Cases",
     main = "Scatter Plot of New Cases vs. Adolescent Cases")
abline(lm(cases_adolescent ~ cases_new, covid), col = "yellow")
##############
plot(x=covid$cases_new, y=covid$cases_child,
     xlab = "New Cases", ylab = "Child Cases",
     main = "Scatter Plot of New Cases vs. Child Cases")
abline(lm(cases_child ~ cases_new, covid), col = "green")

#multiple linear regression
model <- lm(cases_new ~ cases_unvax + cases_pvax + cases_fvax, covid)
summary(model)

