#### From this link: https://github.com/RamiKrispin/coronavirus 

install.packages("coronavirus")

library(coronavirus)
update_dataset()

covid19_df <- refresh_coronavirus_jhu()
head(covid19_df)

data("coronavirus")
head(coronavirus)

library(dplyr)
library(scales)

summary_df <- coronavirus %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)

summary_df %>% head(20) 

unique(coronavirus$country)

coronavirus_UK <- coronavirus %>% 
  filter(country == "United Kingdom") 

coronavirus_PORT <- coronavirus %>% 
  filter(country == "Portugal") 

unique(covid19_df$location_type)

covid19_df_UK <- covid19_df %>% 
  filter(location == "United Kingdom") 

covid19_df_PORT <- covid19_df %>% 
  filter(location == "Portugal") 
unique(covid19_df_PORT$location_type)

dt2_COVID <- read.csv("owid-covid-data.csv")
UK_df2 <- dt2_COVID %>%
  filter(location == "United Kingdom") 

UK_df2_B <- UK_df2[-(1:32),]
UK_df2_B <- UK_df2_B[-(246:247),]
UK_df2_B$date2 <- as.Date(UK_df2_B$date)
UK_df2_B$month <- format(as.Date(UK_df2_B$date), "%Y-%m")

UK_df2BB = UK_df2_B[,c("date2","total_cases", "new_cases", "total_deaths", "new_deaths", "total_tests", "new_tests")]
UK_df2BB <- UK_df2BB %>% 
  mutate(total_cases = comma(total_cases),
  new_cases = comma(new_cases),
  total_deaths = comma(total_deaths),
  new_deaths = comma(new_deaths),
  total_tests = comma(total_tests),
  new_tests = comma(new_tests))

UK_df2_Bagreg <-  UK_df2_B %>%
  group_by(month) %>%
  summarise_at(vars(new_cases, new_deaths, new_tests), sum)

UK_df2BBB = UK_df2_B[,c('month', 'new_cases', 'new_deaths')]
UK_df2BBB %>%
  group_by(month) %>%
  summarise(Tot_New_Cases = round(sum(new_cases),0),
            Avg_New_Cases = round(mean(new_cases),0),
            Max_New_Cases = round(max(new_cases),0),
            Min_New_Cases = round(min(new_cases),0),
            St.Dev_New_Cases = round(sd(new_cases),0),
            Tot_Deaths = round(sum(new_deaths),0),
            Avg_Deaths = round(mean(new_deaths),0),
            Max_Deaths = round(max(new_deaths),0),
            Min_Deaths = round(min(new_deaths),0),
            St.Dev_Deaths = round(sd(new_deaths),0),
            .groups = 'drop')



cols <- c('month', 'new_cases', 'new_deaths')

UK_df3 <- UK_df2_B[cols]

UK_df3 %>% 
  group_by(month) %>% 
  summarise(Tot_New_Cases = round(sum(new_cases),0),
            Avg_New_Cases = round(mean(new_cases),0),
            Max_New_Cases = round(max(new_cases),0),
            Min_New_Cases = round(min(new_cases),0),
            St.Dev_New_Cases = round(sd(new_cases),0),
            Tot_Deaths = round(sum(new_deaths),0),
            Avg_Deaths = round(mean(new_deaths),0),
            Max_Deaths = round(max(new_deaths),0),
            Min_Deaths = round(min(new_deaths),0),
            St.Dev_Deaths = round(sd(new_deaths),0),
            .groups = 'drop') %>%
  kable(caption = "Table 3: Summary Daily Statistics") %>%
  kable_styling(full_width = F)
    