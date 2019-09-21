library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)


# How many times larger were carbon emissions in the last year relative to the
#  first year?
# Last year
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()
# First year
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  min()
# Print carbon emissions
temp_carbon %>%
  filter(year==2014 | year==1751) %>%
  pull(carbon_emissions)
# Ratio
9855/3


# How many degrees Celsius has temperature increased over the date range?
# Compare the temperatures in the most recent year versus the oldest year.
# Last year
temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  pull(year) %>%
  max()
# First year
temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  pull(year) %>%
  min()
# Print temp_anomalies
temp_carbon %>%
  filter(year==2018 | year==1880) %>%
  pull(temp_anomaly)
# Comparison
0.82+0.11


p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(x=year, y=temp_anomaly)) +
  geom_line()

p <- p + geom_hline(aes(yintercept = 0), col = "blue")
p +
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x=2000, y=0.05, label="20th century mean"), col="blue")

p + geom_line(aes(x=year, y=ocean_anomaly, color="ocean")) +
  geom_line(aes(x=year, y=land_anomaly, color="land"))


greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(rows=vars(gas), scales = "free") +
  geom_vline(aes(xintercept=1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")


# Make a time series line plot of carbon emissions (carbon_emissions)
#  from the temp_carbon dataset. The y-axis is metric tons of carbon
#  emitted per year.
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  ggplot(aes(x=year, y=carbon_emissions)) +
  geom_line()

# Make a line plot of co2 concentration over time (year), coloring by
#  the measurement source (source). Save this plot as co2_time for later use.
co2_time <- historic_co2 %>%
  ggplot(aes(x=year, y=co2, color=source)) +
  geom_line()

# Use the co2_time plot saved above. Change the limits as directed to
#  investigate the rate of change in co2 over various periods with spikes
#  in co2 concentration.
# Change the x-axis limits to -3000 and 2018 to investigate modern changes
#  in co2. About how many years did it take for co2 to rise from its stable
#  level around 275 ppmv to the current level of over 400 ppmv?
co2_time + xlim(-3000,2018)






