#install.packages("readxl")
#install.packages("ggplot2")

library(ggplot2)
library(mgcv)

death <- read.csv("./japan_deaths2.csv")
birth <- read.csv("./japan_birth.csv")
divorce <- read.csv("./divorce.csv")[-1,]


# Birth data
col2drop <- c("Country.or.Area", "Area", "Record.Type", "Reliability.Code", "Source.Year", "Value.Footnotes")
bdf <- birth[, !colnames(birth) %in% col2drop]

bdf <- bdf[!bdf$Month %in% c("Total", "Unknown", ""), ]
bdf$Month <- match(tolower(bdf$Month), tolower(month.name))
bdf$Year <- as.numeric(as.character(bdf$Year))

bdf20 <- bdf[bdf$Year >= 2019, ]
bdf20

bdf20$Year <- as.factor(bdf20$Year)
bdf20$Month <- as.factor(bdf20$Month)
bdf20 <- bdf20[complete.cases(bdf20), ]
bdf20 <- bdf20[order(bdf20$Year, bdf20$Month), ]



# Death data
death$Age <- sapply(strsplit(death$Age, "-"), function(x) mean(as.numeric(x)))
death$Male <- as.numeric(gsub(",", "", death$Male)) 
death$Female <- as.numeric(gsub(",", "", death$Female))

ddf <- data.frame(
  Age = death$Age,
  Sum = death$Male + death$Female
)

# Raw Visualizations
barplot(divorce$Number, names.arg = divorce$Age, xlab = "Age Group", ylab = "Divorces", las=2, cex.names = 0.7, main = "Number of Divorces by Age", ylim = c(0, max(divorce$Number) * 1.2))

barplot(ddf$Sum, names.arg = death$Age, xlab = "Age Group", ylab = "Deaths", las=2, main = "Number of Covid Deaths by Age", ylim = c(0, max(ddf$Sum) * 1.2))

plot(1:nrow(bdf20), bdf20$Value, type = "l", xlab = "Index", ylab = "Births", main = "Births per Month", xaxt = "n")
axis(1, at = 1:nrow(bdf20), labels = paste(bdf20$Year, bdf20$Month, sep = "-"), las = 2, cex.axis = 0.7)

bdf20 <- bdf[bdf$Year >= 2012, ]
bdf20

bdf20$Year <- as.factor(bdf20$Year)
bdf20$Month <- as.factor(bdf20$Month)
bdf20 <- bdf20[complete.cases(bdf20), ]
bdf20 <- bdf20[order(bdf20$Year, bdf20$Month), ]

# Sum the values by year
sum_by_year <- aggregate(Value ~ Year, data = bdf20, sum)
sum_by_year

# Create a new dataframe with year and sum of values
new_df <- data.frame(
  Year = sum_by_year$Year,
  Value = sum_by_year$Value
)

new_df <- new_df[complete.cases(new_df), ]

ggplot(new_df, aes(x = Year, y = Value, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  labs(x = "Year", y = "Births", title = "Births per Year") +
  theme_minimal()


# Model
total_pop <- 123294513
ddf$Sum <- (ddf$Sum / total_pop) * 100.0

model_death <- gam(Sum ~ s(Age), data = ddf)

new_age <- seq(min(1), max(90), length.out = 100)
predictions_gam <- predict(model_death, newdata = data.frame(Age = new_age))

plot(ddf$Age, ddf$Sum, type = "l", col = "blue", lwd = 2, xlab = "Age", ylab = "% of deaths per population", main = "Number of Deaths by Age")
lines(new_age, predictions_gam, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Data", "Predictions"), col = c("blue", "red"), lty = 1:2, cex = 0.8)


divorce$Age <- sapply(strsplit(divorce$Age, "-"), function(x) mean(as.numeric(x)))

divorce
str(divorce)

divdf <- data.frame(
  Age = divorce$Age,
  Sum = (divorce$Number/total_pop) * 100
)

model_div <- gam(Sum ~ s(Age), data = divdf)

predictions_gam_div <- predict(model_div, newdata = data.frame(Age = new_age))

plot(divdf$Age, divdf$Sum, type = "l", col = "blue", lwd = 2, xlab = "Age", ylab = "% of divorce per population", main = "Number of Divorces by Age")
lines(new_age, predictions_gam_div, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Data", "Predictions"), col = c("blue", "red"), lty = 1:2, cex = 0.8)

predict_outcome <- function(age) {
  
  div_p <- predict(model_div, newdata = data.frame(Age = c(age)))
  dea_p <- predict(model_death, newdata = data.frame(Age = c(age)))
  if(div_p > dea_p)
  {
    return (paste("Person more likely to get a divorce: ", div_p, " than die: ", dea_p))
  }else{
    return (paste("Person more likely to get die: ", dea_p, " than get a divorce: ", div_p))
  }
}

age <- 85

predict_outcome(age)
