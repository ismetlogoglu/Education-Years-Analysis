data1 <- read.csv("country_comparison_large_dataset.csv")
head(data1)
str(data1)
library(ggplot2)
library(dplyr)

# Distribution of unemployment rate
ggplot(data1, aes(x = Unemployment.Rate....)) +
  geom_histogram(binwidth = 0.5, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Unemployment Rates", x = "Unemployment Rate", y = "Count")

# Boxplot of Literacy Rate by Country
ggplot(data1, aes(x = reorder(Country, Literacy.Rate...., FUN = median), y = Literacy.Rate....)) +
  geom_boxplot(fill = "mediumpurple3", color = "black") +
  labs(title = "Graduation Rate by Country", 
       x = "Country", 
       y = "Graduation Rate (%)") +
  geom_hline(yintercept = median(data1$Literacy.Rate....), linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Correlation between literacy rate and unemployment rate
cor(data1$Literacy.Rate...., data1$Unemployment.Rate...., use = "complete.obs")



# Time series of unemployment and education expenditure for a specific country
ggplot(data1[data1$Country == "USA", ], aes(x = Year)) +
  geom_line(aes(y = Unemployment.Rate...., color = "Unemployment Rate")) +
  geom_line(aes(y = Education.Expenditure.as...of.GDP, color = "Qualified Student")) +
  labs(title = "Unemployment vs Qualified Students in the USA", x = "Year", y = "Value")



library(ggplot2)

# Filter the data for the USA
usa_data <- data1[data1$Country == "USA", ]

# Create the plot
ggplot(usa_data, aes(x = Year)) +
  geom_line(aes(y = Unemployment.Rate...., color = "Unemployment Rate"), size = 1) +
  geom_line(aes(y = Education.Expenditure.as...of.GDP, color = "Qualified Students"), size = 1, linetype = "dashed") +
  labs(
    title = "Unemployment vs Qualified Students in the USA", 
    x = "Year", 
    y = "Value",
    color = "Metric"
  ) +
  scale_color_manual(values = c("Unemployment Rate" = "blue", "Qualified Students" = "green")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )


library(ggplot2)

# Filter the data for the USA
usa_data <- data1[data1$Country == "USA", ]

# Create the plot
ggplot(usa_data, aes(x = Year)) +
  geom_line(aes(y = Unemployment.Rate...., color = "Unemployment Rate"), size = 1) +
  geom_line(aes(y = Education.Expenditure.as...of.GDP, color = "Education Expenditure (% of GDP)"), size = 1, linetype = "dashed") +
  labs(
    title = "Unemployment vs Education Expenditure in the USA", 
    x = "Year", 
    y = "Value",
    color = "Metric"
  ) +
  scale_color_manual(values = c("Unemployment Rate" = "blue", "Education Expenditure (% of GDP)" = "green")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )






















ggplot(data1, aes(x = Literacy.Rate...., y = Unemployment.Rate....)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Literacy Rate vs Unemployment Rate", x = "Literacy Rate (%)", y = "Unemployment Rate (%)")


data1$Education.Group <- cut(data1$Education.Expenditure.as...of.GDP, breaks = 3, labels = c("Low", "Medium", "High"))

ggplot(data1, aes(x = Education.Group, y = Unemployment.Rate....)) +
  geom_boxplot(aes(fill = Education.Group)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(title = "Unemployment Rate by Student Experience Levels", 
       x = "Student Experience Level", 
       y = "Unemployment Rate (%)") +
  scale_y_continuous(limits = c(3, 8)) +
  theme_minimal()














