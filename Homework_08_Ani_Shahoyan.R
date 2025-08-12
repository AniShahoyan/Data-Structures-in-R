# Data preparation and generation

# Setting a seed to make the random numbers reproducible
set.seed(123)

cities <- c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix")
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

generate_weather_data <- function(n, min_val, max_val) {
  round(runif(n, min = min_val, max = max_val), 2)
}

temperature_list <- list()
precipitation_list <- list()
wind_speed_list <- list()

# Generate weather data for cities using random numbers

for (city in cities) {
  temperature_list[[city]] <- generate_weather_data(length(days), -5, 35)
  precipitation_list[[city]] <- generate_weather_data(length(days), 0, 20)
  wind_speed_list[[city]] <- generate_weather_data(length(days), 0, 100)
}
# Combine temperature data into a matrix
temperature_matrix <- do.call(rbind, temperature_list)
colnames(temperature_matrix) <- days
rownames(temperature_matrix) <- cities
# Combine precipitation data into a matrix
precipitation_matrix <- do.call(rbind, precipitation_list)
colnames(precipitation_matrix) <- days
rownames(precipitation_matrix) <- cities
# Combine wind speed data into a matrix
wind_speed_matrix <- do.call(rbind, wind_speed_list)
colnames(wind_speed_matrix) <- days
rownames(wind_speed_matrix) <- cities

# 
# Combine all weather parameters into a 3D array (cities, days, parameters)
weather_array <- array(
  data = c(temperature_matrix, precipitation_matrix, wind_speed_matrix),
  dim = c(length(cities), length(days), 3),
  dimnames = list(cities, days, c("Temperature", "Precipitation", "Wind_Speed"))
)

print(weather_array)


# DATA ANALYSIS

# Temperature Analysis

# Calculate the average temperature for each city over the week
avg_temperature <- rowMeans(temperature_matrix)  # Average temperature for each city
cat("Average temperatures for each city:\n")
print(avg_temperature)

# Identify the city with the highest average temperature
highest_avg_temp_city <- names(avg_temperature)[which.max(avg_temperature)]
cat("\nCity with the highest average temperature:", highest_avg_temp_city, "\n")

# Precipitation Analysis

# Calculate the total precipitation for each city
total_precipitation <- rowSums(precipitation_matrix)  # Total precipitation for each city
cat("\nTotal precipitation for each city (in mm):\n")
print(total_precipitation)

# Identify the city with the most rain
most_rainy_city <- names(total_precipitation)[which.max(total_precipitation)]
cat("\nCity with the most rain:", most_rainy_city, "\n")

# Wind Speed Analysis
# Calculate the average wind speed across all cities for each day
avg_wind_speed_per_day <- colMeans(wind_speed_matrix)  # Average wind speed per day
cat("\nAverage wind speed per day:\n")
print(avg_wind_speed_per_day)

# Identify the day with the highest average wind speed
day_with_highest_wind_speed <- names(avg_wind_speed_per_day)[which.max(avg_wind_speed_per_day)]
cat("\nDay with the highest average wind speed:", day_with_highest_wind_speed, "\n")


# WORKING WITH LISTS

# Create a list to store the weather data matrices (temperature, precipitation, wind speed)
weather_list <- list(
  Temperature = temperature_matrix,
  Precipitation = precipitation_matrix,
  WindSpeed = wind_speed_matrix
)
# List Operations
# Overall average temperature for the week (all cities and days)
overall_avg_temperature <- mean(weather_list$Temperature)  # Mean of all temperature values
cat("\nOverall average temperature for the week (all cities):", overall_avg_temperature, "°C\n")

# Total precipitation for the week (for each city)
total_precipitation_week <- rowSums(weather_list$Precipitation)  # Sum of precipitation for each city
cat("\nTotal precipitation for the week (each city in mm):\n")
print(total_precipitation_week)

# Average wind speed for the week (all cities and days)
overall_avg_wind_speed <- mean(weather_list$WindSpeed)  # Mean of all wind speed values
cat("\nOverall average wind speed for the week (all cities):", overall_avg_wind_speed, "km/h\n")

# Find days where the temperature was above 30°C in more than one city
# We will identify days where the temperature exceeds 30°C in more than one city.
above_30_temp_days <- apply(weather_list$Temperature, 2, function(day_temps) sum(day_temps > 30))  # Count cities per day where temp > 30°C
days_above_30 <- names(above_30_temp_days[above_30_temp_days > 1])  # Days with more than one city > 30°C

cat("\nDays where the temperature was above 30°C in more than one city:", paste(days_above_30, collapse = ", "), "\n")

# Advanced data manipulation

# Select a city for adjustment (e.g., New York)
selected_city <- "New York"

# Identify the days where precipitation was 0 mm for the selected city
days_with_zero_precip <- weather_list$Precipitation[selected_city, ] == 0

# Store the temperature data before the adjustment for comparison
temperature_before_adjustment <- weather_list$Temperature[selected_city, ]

# Adjust the temperature for those days (increase by 10%)
weather_list$Temperature[selected_city, days_with_zero_precip] <- 
  weather_list$Temperature[selected_city, days_with_zero_precip] * 1.10

# Reflect the changes in the matrix and array
temperature_matrix[selected_city, ] <- weather_list$Temperature[selected_city, ]
weather_array[which(rownames(weather_array) == selected_city), , 1] <- weather_list$Temperature[selected_city, ]

# Compare the average temperature before and after the adjustment
avg_temp_before <- mean(temperature_before_adjustment)
avg_temp_after <- mean(weather_list$Temperature[selected_city, ])

cat("\nAverage temperature of", selected_city, "before adjustment:", round(avg_temp_before, 2), "°C")
cat("\nAverage temperature of", selected_city, "after adjustment:", round(avg_temp_after, 2), "°C")


