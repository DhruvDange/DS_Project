# install.packages("nortest")
# install.packages("readr")
library(nortest)
library(readr)

csv_file_path <- "./ed_data.csv"

data_skip <- 6

raw_data <- read_csv(csv_file_path, skip = data_skip)
# print(spec(raw_data))

View(raw_data)

columns_to_select <- c("STATE", "AGE", "PERSONS", "MALE", "FEMALE")

data <- raw_data[, columns_to_select]
print(dim(data))
View(data)

# # Normality maybe
india_data <- data[data$STATE == "INDIA", ]
india_data <- na.omit(india_data)
india_data <- india_data[2:22,]
print(dim(india_data))
View(india_data)

result <- ad.test(india_data[["PERSONS"]])
print(result)
hist(india_data[["PERSONS"]], main = "Persons")

barplot(india_data[["PERSONS"]], names.arg = india_data[["AGE"]], main = "Bar Plot", xlab = "Ages", ylab = "Values", col = "blue", las=2)

# A = 2.633, p-value = 6.727e-07  ->>>> Not normally distributed



target_age <- "All ages"
all_age_data <- data[data$AGE == target_age, ]
all_age_data <- na.omit(all_age_data)



box_data <- all_age_data[2:22, ]
total_ed_col <- box_data$PERSONS
min_value <- min(total_ed_col)
max_value <- max(total_ed_col)

normalized_education <- (total_ed_col - min_value) / (max_value - min_value)

# Replace the original column with the normalized values
box_data$PERSONS <- normalized_education
boxplot(box_data$PERSONS)



View(all_age_data)

print("Data Head: ")
print(head(all_age_data))
print("Data Summary: ")
print(summary(all_age_data))
print("Data Structure: ")
print(str(all_age_data))