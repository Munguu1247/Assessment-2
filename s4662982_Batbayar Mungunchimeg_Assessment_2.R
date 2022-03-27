#Load the packages

library(tidyverse)
library(skimr)
library(tidymodels)
library(modeldata)
library(knitr)
library(ggplot2)
library(dplyr)



#Setup the data set

thanksgiving_meals <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-11-20/thanksgiving_meals.csv")


#Part 1
#1. Display the first 10 rows of the dataset using `kable()` function (1 marks).

kable(thanksgiving_meals[,1:10], "simple")


#2. Using `skim()` display the summary of variables.

skim(thanksgiving_meals)

kable(skim(thanksgiving_meals))

summary(thanksgiving_meals)


#Think about the task to predict a family income based on their menu: what variables may be useful? 
#Are all of them correct type? 
#Write 2-3 sentences with your explanation. (2 marks)


#The prediction of family income based on their menu means:
#family income is the output variable and other chosen variables are input variables.
#In this situation, the output variables should convert to factors.
#According to the result of the skim function, family income should convert to a factor from characteristic.
#In addition, input variables should be age, community type and US region. All characters variables should convert to a factor.


#Think about the task to predict a community type or US_region based on their menu: what variables may be useful? 
#Are all of them correct type? (2 marks)

#Community type and US region variables should be converted to factors from characteristics when predicting the community type or Us region based on the menu. 
#Because those variables are character which is the wrong type of variable for predicting.
#Input variables should be main dish and main prep.

#3. Use `fct_reorder` and `parse_number` functions to create a factor variable `family_income`(2 mark).

skim(thanksgiving_meals)

class(thanksgiving_meals$family_income)

factor <- thanksgiving_meals %>%
  mutate(family_income = fct_reorder(family_income, parse_number(family_income)))


skim(factor)
class(factor$family_income)


#4. What is the number of people who celebrate? (1 mark)

thanksgiving_meals %>%
  group_by(celebrate) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  filter(celebrate == "Yes") %>%
  kable()
#There were 980 people said that they celebrated Thanksgiving

#5. What are categories and insights for each main dish served and the method it is prepared? (2 marks)

thanksgiving_meals %>%
  group_by(main_dish) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  kable()
  
thanksgiving_meals %>%
  group_by(main_dish_other) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  kable()



thanksgiving_meals %>%
  group_by(main_prep) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  kable()

thanksgiving_meals %>%
  group_by(main_prep_other) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  kable()



#6. Create 3 different data viz showing insights for main dish served and the method. Provide your own legend and use themes.
#Write 2-3 sentences with your explanation of each insight. (4 marks)



##Data visualisation:1


thanksgiving_meals %>%
  group_by(main_dish) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(main_dish)) %>%
  ggplot(aes(main_dish, n, fill = main_dish)) +
  geom_col() +
  geom_text(aes(label = n), fontface = "bold", size = 3, vjust = 0, hjust = 0.7) +
  coord_flip() +
  labs(x = "", y = "", fill = "method",
       title = "TYPE OF MAIN DISH IN THANKSGIVING", 
       subtitle = "The graph shows the numbers of main dishes", caption = "Figure 1") +
  theme_grey() 

# Figure 1 shows the types of the main dish in Thanksgiving. 
# Most families eat Turkey which is the traditional main dish in the Thanksgiving holiday. 

##Data visualization:2

data_viz <- thanksgiving_meals%>%
  select(main_dish, main_prep) %>%
  filter(main_dish == "Turkey") %>%
  count(main_prep) %>%
  arrange(desc(n))


data_viz %>%
  mutate(main_prep = fct_reorder(main_prep, n)) %>%
  ggplot(aes(main_prep, n, fill = main_prep)) +
  geom_col() +
  geom_text(aes(label = n), fontface = "bold", size = 3, vjust = 0, hjust = 0.7) +
  coord_flip() +
  labs(x = "", y = "", fill = "method",
       title = "THE PREPARED METHOD OF TURKEY", 
       subtitle = "The graph shows the numbers of people how to cook the turkey", caption = "Figure 2") +
  theme_minimal() 

#Figure 2 was based on the result of Figure 1. The graph depicts the methods of how people cook their Turkey  
#Baking and Roasting were the main methods of cooking Turkey.


##Data visualisation:3

thanksgiving_meals %>%
  group_by(main_prep) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(main_prep)) %>%
  ggplot(aes(x = reorder(main_prep, - n), y = n, fill = main_prep)) +
  geom_col() +
  geom_text(aes(label = n), fontface = "bold", size = 3, vjust = 0, hjust = 0.7) +
  coord_flip() +
  labs(x = "", y = "", fill = "method",
       title = "PREPARING METHODS OF MAIN DISH IN THANKSGIVING", 
       subtitle = "The graph shows the numbers of preparing methods", caption = "Figure 3") +
  theme_classic() 


#Figure 3 illustrates the methods of preparing dishes for Thanksgiving. 
#According to the figure, the main cooking method is baking. 
#Then the roasting is the second popular way to cook the main dish




#7. How many use cranberry sauce? How many use gravy? 2marks
#As a result, 828 people use cranberry sauce and 892 people use gravy.

thanksgiving_meals %>%
  group_by(cranberry) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  kable()


  
thanksgiving_meals %>%
  group_by(gravy) %>%
  count( ) %>%
  ungroup() %>%
  filter(gravy == "Yes") %>%
  arrange(desc(n)) %>%
  kable()



#8-9. What is the distribution of those who celebrate across income ranges. Create a data viz.
#Write 2-3 sentences with your explanation of each insight. (4 marks)


income_range <- thanksgiving_meals %>%
  select(celebrate, family_income) %>%
  filter(celebrate == "Yes") %>%
  group_by(family_income) %>%
  count()%>%
  ungroup() %>%
  arrange(family_income)

income_range %>%
  mutate(family_income = fct_reorder(family_income, - n)) %>%
  ggplot(aes(x = family_income, y = n, fill = family_income)) +
  geom_col() +
  geom_text(aes(label = n), fontface = "bold", size = 3, vjust = 0, hjust = 1) +
  coord_flip() +
  labs(x = "", y = "", fill = "Income range",
       title = "DISTRIBUTION OF PEOPLE WHO CELEBRATE ACROSS INCOME RANGES", 
       subtitle = "The graph depicts the numbers of people who celebrate Thanksgiving across the income range ", caption = "Figure 4") +
  theme_bw() 
  
#Firstly, a new data set named income range was created for filtering the people who celebrated Thanksgiving across the income range
#Secondly, Figure 4 was built is to show the distribution of the people who celebrate across the income range
#Column graph was chosen based on one discrete and one continuous variable.
#According to the graph, most of the people who celebrate Thanksgiving have an income ranging from $25,000 to $124,999.


#10. Use the following code to create a new data set 2 mark
#Write 2-3 sentences with your explanation of what it does. (4 marks)

new_thankgiving_meals <- thanksgiving_meals %>%
  select(id, starts_with("side"),
       starts_with("pie"),
       starts_with("dessert")) %>%
  select(-side15, -pie13, -dessert12) %>%
  gather(type, value, -id) %>%
  filter(!is.na(value),
         !value %in% c("None", "Other (please specify)")) %>%
  mutate(type = str_remove(type, "\\d+"))

#According to the given function, the new data set shows 3 variables which are id, type and value. 
#The function selected all given variables from the main data set which means sorted and removed the "None" and 
#NA (missing) variables. This function is purpose is clearing the data set.



#11-12. Intsall package `widyr` and use `pairwise_cor()` function https://www.rdocumentation.org/packages/widyr/versions/0.1.3/topics/pairwise_cor
#Write 2-3 sentences with your explanation of what it does. (2 marks)

#Use this code for the new dataset

install.packages('widyr')
library(widyr)

new_thankgiving_meals %>%
  pairwise_cor(value, id, sort = TRUE) %>%
  kable()
  

#pairwise_cor of 'widyr' package is p-values for pairwise correlation tables. 
#Used for finding correlations of two values in one column that shows the relationship between them.


#Write 1 sentence with your explanation of what insights it shows. (2 marks)

#The table which was created by pairwise_cor function shows all possible correlations of two variables which are from the side, pie and dessert.  
#P-values of correlation are illustrated in the table.  


#13. Use `lm()` or randomForest() function to build a model that predict a family income based on data in the dataset. 8 marks

#Compare 3 models using different set of input variables. Use different number of variables.

#Explain your choice of variables (3 sentences) 

#Write 2 sentences explaining which model is best.

#There 2 methods are used in Classical Machine Leaning methods which are Supervised and Unsupervised.
#Regression (lm) and Classification (randomForest) are models of the Supervised modelling approach.
#Choosing the model is based on the type of output. If the output will be a character, Classification is used in the modelling and is made a decision tree.
#In this report, RandomForest function is used because family income is the output variable that is characteristic. 
#The report were chosen three input variables which were age, community type and celebrate.
#I predicted that people who celebrate Thanksgiving and the high age people maybe earn high salaries.
#In addition, people who live in urban maybe have high family incomes.
# As a result, an estimate of error value should be near to 0 or less than 5%. 
#Modelling 1 which is rf1 is the best of the three models because the estimated error rate is 82.05% is less than modelling 2 (82.47%) and modelling 3 (83.1%).


skim(thanksgiving_meals)

thanksgiving_meals$celebrate <- as_factor(thanksgiving_meals$celebrate)
thanksgiving_meals$age <- as_factor(thanksgiving_meals$age)
thanksgiving_meals$community_type <- as_factor(thanksgiving_meals$community_type)

install.packages("randomForest")
library(randomForest)

rf1 <- randomForest(family_income ~ celebrate , data = factor, na.action = na.omit)

print(rf1) 


rf2 <- randomForest(family_income ~  age + community_type , data = factor, na.action = na.omit)

print(rf2) 

rf3 <- randomForest(family_income ~  celebrate + community_type + age , data = factor, na.action = na.omit)

print(rf3) 
