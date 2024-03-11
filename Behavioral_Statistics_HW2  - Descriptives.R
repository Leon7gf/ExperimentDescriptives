#Setting up the data into a table
student_id = seq(1,61) #Creates sequence of ID numbers from 1-61

#In Python terms, im creating a loop (rep()) that will append values (NA,61) 
#and doing some conditional statements that will asign a certain letter for 
#a range of values
class_section = rep(NA,61) #Creates a vector to store the different class range
  class_section[student_id <= 16] = "A"
  class_section[student_id > 16 & student_id <= 31] = "B"
  class_section[student_id > 31 & student_id <= 47] = "C"
  class_section[student_id > 47] = "D"

test_score = c(74,63,67,69,61,69,58,80,79,
               75,54,74,63,75,64,68,63,98,
               67,89,82,82,70,73,70,88,81,
               62,85,77,75,73,83,70,74,66,
               85,69,73,83,67,75,62,75,77,
               82,76,79,72,75,72,76,79,81,
               73,75,73,87,84,75,81)

data = data.frame(ID=student_id, Class=class_section, Score=test_score)
print(data)

#Calculating descriptive statistics for "Score" variable
number_test_scores = length(data$Score)
mean_test_scores = mean(data$Score)
missing_test_scores = sum(is.na(data$Score)) #is.na() will identify which elements are missing.
median_test_scores = median(data$Score)
sd_test_scores = sd(data$Score)
minimum_test_scores = min(data$Score)
maximum_test_scores = max(data$Score)
iqr_test_scores = quantile(data$Score, probs = c(.25,.75))
  
summary_table_scores = data.frame(
  Measure = c("N", "Missing", "Mean", "Median", "S.D.", "Min", "Max",
              "25th percentile", "75th percentile"),
  Value = c(number_test_scores, missing_test_scores, mean_test_scores,
            median_test_scores, sd_test_scores, minimum_test_scores,
            maximum_test_scores, iqr_test_scores)
  )
print(summary_table_scores)

# Load required library
library(ggplot2)

# Plotting Histogram with Density Score by Class
ggplot(data, aes(x = Score, fill = Class)) +
  geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") +
  geom_density(aes(y = ..scaled..), color = "black", fill = NA, alpha = 0.5) +  # Specify x = ..scaled.. for the density line
  facet_wrap(~ Class, nrow = 1) +
  labs(title = "Histogram of Score by Class", x = "Score", y = "Density")

# Plotting Box Plot by Class
ggplot(data, aes(x = Class, y = Score, fill = Class)) +
  geom_boxplot() +
  labs(title = "Box Plot of Score by Class", x = "Class", y = "Score")

# Plotting Bar Plot by Class
ggplot(data, aes(x = Class, fill = Class)) +
  geom_bar() +
  labs(title = "Bar Plot of Score by Class", x = "Class", y = "Score")

#Calculating Descriptive Statistics with "Class" variable


