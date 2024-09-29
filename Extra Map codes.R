data <- read.csv("Global_Education.csv")


head(data)
str(data)
summary(data)
library(ggplot2)
library(leaflet)
library(dplyr)
library(RColorBrewer)

# Scatter plot comparing Unemployment Rate with various education completion rates globally
ggplot(data, aes(x=Completion_Rate_Primary_Male + Completion_Rate_Primary_Female, y=Unemployment_Rate)) +
  geom_point(color='blue') +
  geom_smooth(method='lm', se=FALSE, color='red') +
  labs(title="Global: Unemployment vs. Primary Education Completion", 
       x="Primary Education Completion Rate (Male + Female)", 
       y="Unemployment Rate (%)")

ggplot(data, aes(x=Completion_Rate_Lower_Secondary_Male + Completion_Rate_Lower_Secondary_Female, y=Unemployment_Rate)) +
  geom_point(color='green') +
  geom_smooth(method='lm', se=FALSE, color='red') +
  labs(title="Global: Unemployment vs. Lower Secondary Education Completion", 
       x="Lower Secondary Education Completion Rate (Male + Female)", 
       y="Unemployment Rate (%)")

ggplot(data, aes(x=Completion_Rate_Upper_Secondary_Male + Completion_Rate_Upper_Secondary_Female, y=Unemployment_Rate)) +
  geom_point(color='purple') +
  geom_smooth(method='lm', se=FALSE, color='red') +
  labs(title="Global: Unemployment vs. Upper Secondary Education Completion", 
       x="Upper Secondary Education Completion Rate (Male + Female)", 
       y="Unemployment Rate (%)")


# dataset for Turkey
turkey_data <- data[data$Countries.and.areas == "Turkey", ]

# Scatter plot comparing unemployment rate with education levels for Turkey
ggplot(turkey_data, aes(x=Completion_Rate_Primary_Male + Completion_Rate_Primary_Female, y=Unemployment_Rate)) +
  geom_point(color='blue') +
  geom_smooth(method='lm', se=FALSE, color='red') +
  labs(title="Turkey: Unemployment vs. Primary Education Completion", 
       x="Primary Education Completion Rate (Male + Female)", 
       y="Unemployment Rate (%)")

ggplot(turkey_data, aes(x=Completion_Rate_Lower_Secondary_Male + Completion_Rate_Lower_Secondary_Female, y=Unemployment_Rate)) +
  geom_point(color='green') +
  geom_smooth(method='lm', se=FALSE, color='red') +
  labs(title="Turkey: Unemployment vs. Lower Secondary Education Completion", 
       x="Lower Secondary Education Completion Rate (Male + Female)", 
       y="Unemployment Rate (%)")

ggplot(turkey_data, aes(x=Completion_Rate_Upper_Secondary_Male + Completion_Rate_Upper_Secondary_Female, y=Unemployment_Rate)) +
  geom_point(color='purple') +
  geom_smooth(method='lm', se=FALSE, color='red') +
  labs(title="Turkey: Unemployment vs. Upper Secondary Education Completion", 
       x="Upper Secondary Education Completion Rate (Male + Female)", 
       y="Unemployment Rate (%)")


ggplot(data, aes(x=Completion_Rate_Primary_Male + Completion_Rate_Primary_Female, y=Unemployment_Rate)) +
  geom_point() +
  facet_wrap(~ Countries.and.areas == "Turkey") +
  geom_smooth(method='lm', se=FALSE, color='red') +
  labs(title="Unemployment vs. Primary Education Completion: Global vs. Turkey",
       x="Primary Education Completion Rate (Male + Female)",
       y="Unemployment Rate (%)")

# Filter data for countries with unemployment data
data_filtered <- data %>% 
  filter(!is.na(Unemployment_Rate))
# Convert the dataset to UTF-8
data$Countries.and.areas <- iconv(data$Countries.and.areas, from = "latin1", to = "UTF-8")


# Create a color palette for unemployment rates
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = data_filtered$Unemployment_Rate
)

# Create the leaflet map
leaflet(data_filtered) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~Longitude, 
    lat = ~Latitude, 
    color = ~pal(Unemployment_Rate), 
    stroke = FALSE, 
    fillOpacity = 0.7,
    radius = ~Unemployment_Rate / 2, # Radius based on unemployment rate
    popup = ~paste0("<strong>Country: </strong>", Countries.and.areas,
                    "<br><strong>Unemployment Rate: </strong>", Unemployment_Rate, "%")
  ) %>%
  addLegend(
    pal = pal, 
    values = ~Unemployment_Rate, 
    title = "Unemployment Rate (%)",
    position = "bottomright"
  )
# Remove or replace non-ASCII characters in country names
data$Countries.and.areas <- gsub("[^[:print:]]", "", data$Countries.and.areas)
# Ensure popup text is encoded in UTF-8
popup_content <- function(country, rate) {
  paste0("<strong>Country: </strong>", 
         iconv(country, from = "latin1", to = "UTF-8"),
         "<br><strong>Unemployment Rate: </strong>", rate, "%")
}

library(leaflet)

pal <- colorNumeric(palette = "YlOrRd", domain = data_filtered$Unemployment_Rate)

popup_content <- function(country, rate) {
  paste("<strong>Country: </strong>", country, 
        "<br><strong>Unemployment Rate: </strong>", round(rate, 2), "%")
}

leaflet(data_filtered) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  # Add a clean base map
  addCircleMarkers(
    lng = ~Longitude, 
    lat = ~Latitude, 
    color = ~pal(Unemployment_Rate), 
    stroke = FALSE, 
    fillOpacity = 0.7,
    radius = ~sqrt(Unemployment_Rate) * 5,  # Adjust size to be more proportional
    popup = ~popup_content(Countries.and.areas, Unemployment_Rate)
  ) %>%
  addLegend(
    pal = pal, 
    values = ~Unemployment_Rate, 
    title = "Unemployment Rate (%)", 
    position = "bottomright",
    labFormat = labelFormat(suffix = "%")  # Add a percentage sign
  )

# Set the encoding to UTF-8 in R session
options(encoding = "UTF-8")

library(leaflet)
library(stringi)

# Ensure all character columns are in UTF-8
data_filtered <- data_filtered %>%
  mutate_if(is.character, ~iconv(., to = "UTF-8"))

# Define a popup content function ensuring UTF-8 encoding
popup_content <- function(country, rate) {
  content <- paste("<strong>Country: </strong>", country, 
                   "<br><strong>Unemployment Rate: </strong>", round(rate, 2), "%")
  iconv(content, to = "UTF-8")
}

# Generate the map
pal <- colorNumeric(palette = "YlOrRd", domain = data_filtered$Unemployment_Rate)

leaflet(data_filtered) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~Longitude, 
    lat = ~Latitude, 
    color = ~pal(Unemployment_Rate), 
    stroke = FALSE, 
    fillOpacity = 0.7,
    radius = ~sqrt(Unemployment_Rate) * 5, 
    popup = ~popup_content(Countries.and.areas, Unemployment_Rate)  # Ensure UTF-8 encoding
  ) %>%
  addLegend(
    pal = pal, 
    values = ~Unemployment_Rate, 
    title = "Unemployment Rate (%)", 
    position = "bottomright",
    labFormat = labelFormat(suffix = "%")
  )


# Install necessary packages
install.packages("geojsonio")
install.packages("rgdal")

# Load the libraries
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(geojsonio)

# Load GeoJSON data for country borders
geojson <- geojson_read("countries.geo.json", what = "sp")
# Merge the unemployment data with the GeoJSON data
geojson@data <- merge(geojson@data, data, by.x = "name", by.y = "Countries.and.areas", all.x = TRUE)
# Create a color palette for unemployment rates
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = geojson@data$Unemployment_Rate
)

# Create the leaflet map
leaflet(geojson) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(Unemployment_Rate),
    fillOpacity = 0.7,
    color = "#BDBDC3", # Country border color
    weight = 1, # Border width
    popup = ~paste0("<strong>Country: </strong>", name,
                    "<br><strong>Unemployment Rate: </strong>", Unemployment_Rate, "%")
  ) %>%
  addLegend(
    pal = pal, 
    values = ~Unemployment_Rate, 
    title = "Unemployment Rate (%)",
    position = "bottomright"
  )



