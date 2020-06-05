##########################################################################
# This script:
# 1. Counts and plots gun crimes by city
#
# Exports: 
#
# To-do:
# 
##########################################################################

## 1. ----
crime_count <- gun_crimes_clean %>% 
  group_by(city) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

crimesByCityPlot <- ggplot(crime_count,
                           aes(x = reorder(city, count), y = count)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  coord_flip() +
  plotTheme()