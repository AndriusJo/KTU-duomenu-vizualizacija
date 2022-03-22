library(tidyverse)

#1
duom = read.csv("data/lab_sodra.csv")

data = duom %>%
  filter(ecoActCode == 494100)


data = data[!is.na(data$avgWage),]

data = data %>% 
  group_by(avgWage) %>%
  arrange(desc(avgWage)) 

#sumar = summary(data$avgWage)
#Q1 = sumar[[2]]
#Q3 = sumar[[5]]
#IQR = Q3-Q1
#data = data[!(data$avgWage < Q1 - 3*IQR | data$avgWage > Q3+3/IQR),]
  
ggplot(data = data, aes(avgWage)) +
  geom_bar(stat = "bin", bins = 50, fill = "steelblue", color = "black")

#2
duom = read.csv("data/lab_sodra.csv")

data = duom %>%
  filter(ecoActCode == 494100) 
data = data[!is.na(data$avgWage),]



#max5 = data %>%
  #arrange(desc(avgWage)) %>%
  #group_by(name) %>%
  #slice(1) %>%
  #arrange(desc(avgWage)) %>%
  #head(5) %>%
  #select(name)

max5 = data %>%
  group_by(name) %>%
  summarise(wage = max(avgWage)) %>%
  arrange(desc(wage)) %>%
  head(5)

max5full = data %>% filter(name %in% max5$name)

max5full %>%
  ggplot(aes(x = month, y = avgWage, group = name)) +
  geom_line(aes(colour = name))+
  labs(title = "Top 5 Companies with Highest Average Wage", x = "Month", y = "Average Wage")
  
#3
duom = read.csv("data/lab_sodra.csv")

data = duom %>%
  filter(ecoActCode == 494100) 
data = data[!is.na(data$avgWage),]

max5 = data %>%
  group_by(name) %>%
  summarise(wage = max(avgWage)) %>%
  arrange(desc(wage)) %>%
  head(5)

max5full = data %>% filter(name %in% max5$name)

max5ins = max5full %>%
  group_by(name) %>%
  summarise(insured = max(numInsured)) %>%
  arrange(desc(insured))

max5ins %>%
  ggplot(aes(x = reorder(name, -insured), y = insured, group = name)) +
  geom_col(aes(fill = name)) +
  labs(title = "Top 5 companies highest number or insured employees",
       x = "Company name", y = "Ammount of insured employees")

