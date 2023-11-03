# Load the necessary libraries
library(caTools)
library(ggplot2)

# Read the data
my_data <- read.csv("./literacy.csv")

# Filter the data
filtered_data <- subset(my_data, Rural_Urban == ' Total' & Age_Group == 'All ages')

# Select relevant columns
data_subset <- filtered_data[c('State_code', 'Male_literates', 'Female_literates', 'Male_Illiterates', 'Female_Illiterates', 'Main_Male_workers', 'Main_Female_workers')]

# Calculate required columns
data_subset$sum1 <- data_subset$Male_literates + data_subset$Male_Illiterates
data_subset$MaleEmpPct <- (data_subset$Main_Male_workers / data_subset$sum1) * 100
data_subset$sum2 <- data_subset$Female_literates + data_subset$Female_Illiterates
data_subset$FemaleEmpPct <- (data_subset$Main_Female_workers / data_subset$sum2) * 100

# Check the summary statistics of the data
summary(data_subset)

# Check for normality
columns_to_test <- c('FemaleEmpPct', 'MaleEmpPct')
alpha <- 0.05

for (column in columns_to_test) {
  shapiro_test_result <- shapiro.test(data_subset[[column]])
  
  p_value <- shapiro_test_result$p.value
  if (p_value < alpha) {
    print(paste("H0 rejected for", column, ":", "Data is not normally distributed (p =", p_value, ")"))
  } else {
    print(paste("H0 not rejected for", column, ":", "Data is normally distributed (p =", p_value, ")"))
  }
}

# Calculate the correlation between MaleEmpPct and FemaleEmpPct
correlation_test <- cor.test(data_subset$MaleEmpPct, data_subset$FemaleEmpPct)
print(correlation_test)
p_value <- correlation_test$p.value
if (p_value < alpha) {
  print("H0 not rejected. They are strongly correlated.")
} else {
  print("H0 rejected. They are not strongly correlated.")
}

# Create a scatter plot for Male and Female Employment Percentages by State_code
ggplot(data_subset, aes(x = factor(State_code)))+
  geom_point(aes(y = MaleEmpPct, color = "Male")) +
  geom_point(aes(y = FemaleEmpPct, color = "Female")) +
  labs(x = "State_code", y = "Percentage", color = "Gender") +
  ggtitle("Male and Female Employment Percentages by State_code") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme(legend.title = element_blank())

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
split <- sample.split(data_subset, SplitRatio = 0.7)
train <- subset(data_subset, split == TRUE)
test <- subset(data_subset, split == FALSE)

# Fit a linear regression model
linear_model <- lm(MaleEmpPct ~ State_code, data = train)
summary(linear_model)

# Make predictions on the test set
y_pred <- predict(linear_model, newdata = test)

# Plot the data and regression line
plot(data_subset$State_code, data_subset$MaleEmpPct)
abline(linear_model, col = "blue")
# Fit a linear regression model
linear_model <- lm(MaleEmpPct ~ State_code, data = train)
summary(linear_model)




employment_data <- data_subset[c('State_code', 'MaleEmpPct', 'FemaleEmpPct')]

# Create a bar plot
ggplot(employment_data, aes(x = State_code)) +
  geom_bar(aes(y = MaleEmpPct, fill = "Male"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = FemaleEmpPct, fill = "Female"), stat = "identity", position = "dodge") +
  labs(x = "State Code", y = "Employment Percentage", fill = "Gender") +
  ggtitle("Male and Female Employment Percentages by State") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme(legend.title = element_blank())