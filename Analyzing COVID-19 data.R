update.packages(ask = 'graphics',
                checkBuilt = TRUE)

installr::updateR(fast = FALSE,
                  browse_news = TRUE,
                  copy_Rprofile.site = FALSE,
                  start_new_R = TRUE,
                  print_R_versions = FALSE,
                  GUI = TRUE,
                  to_checkMD5sums = TRUE,
                  keep_install_file = FALSE)

# https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv



# 1 - Data Retrieval ----

# 1.1 - Packages ----

Allocated_Memory <- paste(memory.size(),"Mb")

cls <- function() cat("\f")

library(data.table)
library(zoo)
library(tidyverse)
library(lubridate)
library(dtplyr)

Allocated_Memory <- paste(memory.size(),"Mb")


# 2 - Dataset Creation ----


# 2.1 - Useful Functions ----

covid_rename_columns <- function(input_data){
   input_data %>%
      select('country' = 'Country/Region',
             'subregion' = 'Province/State',
             'lat' = 'Lat',
             'long' = 'Long',
             everything()
      ) ->
      output_data
   return(output_data)
}

covid_pivot_data <- function(input_data, value_var_name){
   input_data %>%
      pivot_longer(cols = -one_of('country',
                                  'subregion',
                                  'lat',
                                  'long'),
                   names_to = 'date',
                   values_to = value_var_name
      ) ->
      output_data
   return(output_data)
}

covid_convert_dates <- function(input_data){
   input_data %>%
      mutate(date = mdy(date)) ->
      output_data
   return(output_data)
}

covid_rearrange_data <- function(input_data){
   input_data %>%
      select(country, subregion, date, lat, long, everything()) %>%
      arrange(country, subregion, date) ->
      output_data
   return(output_data)
}

covid_get_data <- function(input_url, value_var_name){
   covid_data_inprocess <- fread(input_url)
   covid_data_inprocess <- covid_rename_columns(covid_data_inprocess)
   covid_data_inprocess <- covid_pivot_data(covid_data_inprocess, value_var_name)
   covid_data_inprocess <- covid_convert_dates(covid_data_inprocess)
   covid_data_inprocess <- covid_rearrange_data(covid_data_inprocess)
   return(covid_data_inprocess)
}


# 2.2 - Defining URLs of Data ----

url_confirmed = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
url_deaths = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
url_recovered = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv'


# 2.3 - Importing Data ----

cls()
covid_confirmed = covid_get_data(url_confirmed,'confirmed')
covid_deaths = covid_get_data(url_deaths,'dead')
covid_recovered = covid_get_data(url_recovered,'recovered')

rm(list = ls(pattern = 'url_'))
Allocated_Memory <- paste(memory.size(),"Mb")
cls()


# 2.4 - Merging Data ----

covid_deaths <- covid_deaths %>% select(-long, -lat)
covid_recovered <- covid_recovered %>% select(-long, -lat)

covid_confirmed %>%
   left_join(covid_deaths, on = c(country, subregion, date)) %>%
   left_join(covid_recovered, on = c(country, subregion, date)) %>%
   as.data.table() ->
   covid_data

covid_data %>% class()
rm(covid_confirmed, covid_deaths, covid_recovered)
Allocated_Memory <- paste(memory.size(),"Mb")


# 2.5 - Computing Newer Cases ----

covid_data %>%
   rename(deaths = dead) %>%
   arrange(country, subregion, date) %>%
   group_by(country, subregion) %>%
   mutate(new_confirmed = confirmed - lag(confirmed)) %>%
   mutate(new_deaths = deaths - lag(deaths)) %>%
   mutate(new_recovered = recovered - lag(recovered)) %>%
   mutate(confirmed_inc = 100 * (confirmed / lag(confirmed) - 1)) %>%
   mutate(deaths_inc = 100 * (deaths / lag(deaths) - 1)) %>%
   mutate(recovered_inc = 100 * (recovered / lag(recovered) - 1)) %>%
   mutate(death_pct = ifelse(test = confirmed > 0,
                             yes = 100 * deaths / confirmed,
                             no = 0)) %>%
   mutate(recovery_pct = ifelse(test = confirmed > 0,
                                yes = 100 * recovered / confirmed,
                                no = 0)) %>%
   ungroup() ->
   covid_data

cls()
covid_data %>% head()
covid_data %>% tail()
covid_data %>% glimpse()

covid_data %>%
   select(country) %>%
   unique() #%>% print(n = 200)



# 3 - Examining Data ----

cls()
covid_data %>% names()


# 3.0 - Brazilian Preliminary Data ----

cls()
covid_data %>%
   filter(country == 'Brazil') %>%
   # filter(date <= '2020-06-06') %>%
   select(-subregion, -lat, -long) %>%
   group_by(country, date) %>%
   summarise(confirmed = sum(confirmed),
             deaths = sum(deaths),
             recovered = sum(recovered),
             new_confirmed = sum(new_confirmed),
             new_deaths = sum(new_deaths),
             new_recovered = sum(new_recovered)) %>%
   mutate(confirmed_inc = 100 * (confirmed / lag(confirmed) - 1),
          deaths_inc = 100 * (deaths / lag(deaths) - 1),
          recovered_inc = 100 * (recovered / lag(recovered) - 1),
          death_pct = ifelse(test = confirmed > 0,
                             yes = 100 * deaths / confirmed,
                             no = 0),
          recovery_pct = ifelse(test = confirmed > 0,
                                yes = 100 * recovered / confirmed,
                                no = 0)) %>%
   ungroup() %>%
   tail(n = 15)

# A tibble: 15 x 13
# country date       confirmed deaths recovered new_confirmed new_deaths new_recovered confirmed_inc deaths_inc recovered_inc death_pct recovery_pct
# <chr>   <date>         <int>  <int>     <int>         <int>      <int>         <int>         <dbl>      <dbl>         <dbl>     <dbl>        <dbl>
#    1 Brazil  2021-01-10   8105790 203100   7231336         29792        469         25926         0.369      0.231         0.360      2.51         89.2
# 2 Brazil  2021-01-11   8131612 203580   7273237         25822        480         41901         0.319      0.236         0.579      2.50         89.4
# 3 Brazil  2021-01-12   8195637 204690   7284945         64025       1110         11708         0.787      0.545         0.161      2.50         88.9
# 4 Brazil  2021-01-13   8256536 205964   7347080         60899       1274         62135         0.743      0.622         0.853      2.49         89.0
# 5 Brazil  2021-01-14   8324294 207095   7394739         67758       1131         47659         0.821      0.549         0.649      2.49         88.8
# 6 Brazil  2021-01-15   8393492 208246   7438075         69198       1151         43336         0.831      0.556         0.586      2.48         88.6
# 7 Brazil  2021-01-16   8455059 209296   7485823         61567       1050         47748         0.734      0.504         0.642      2.48         88.5
# 8 Brazil  2021-01-17   8488099 209847   7509009         33040        551         23186         0.391      0.263         0.310      2.47         88.5
# 9 Brazil  2021-01-18   8511770 210299   7560460         23671        452         51451         0.279      0.215         0.685      2.47         88.8
# 10 Brazil  2021-01-19   8573864 211491   7618080         62094       1192         57620         0.730      0.567         0.762      2.47         88.9
# 11 Brazil  2021-01-20   8638249 212831   7618080         64385       1340             0         0.751      0.634         0          2.46         88.2
# 12 Brazil  2021-01-21   8697368 214147   7673092         59119       1316         55012         0.684      0.618         0.722      2.46         88.2
# 13 Brazil  2021-01-22   8753920 215243   7746910         56552       1096         73818         0.650      0.512         0.962      2.46         88.5
# 14 Brazil  2021-01-23   8816254 216445   7775928         62334       1202         29018         0.712      0.558         0.375      2.46         88.2
# 15 Brazil  2021-01-24   8844577 217037   7821376         28323        592         45448         0.321      0.274         0.584      2.45         88.4


# 3.1 - Country ----

covid_data %>%
   distinct(country) %>%
   print(n = 200)

covid_data %>%
   distinct(country) %>%
   count()


# 3.2 - Country / Subregion ----

covid_data %>%
   distinct(country, subregion) %>%
   print(n = 300)

covid_data %>%
   distinct(country, subregion) %>%
   count()


# 3.3 - Date ----

covid_data %>%
   distinct(date) %>%
   as.data.table()

covid_data %>%
   distinct(date) %>%
   count()


# 3.4 - Country / Subregion / Date ----

covid_data %>%
   distinct(country, subregion, date) %>%
   count()


# 3.5 - Lat / Long ----

cls()
covid_data %>%
   select(long) %>%
   summarise('minimum' = min(long, na.rm = TRUE),
             'maximum' = max(long, na.rm = TRUE),
             'mean' = mean(long, na.rm = TRUE),
             'median' = median(long, na.rm = TRUE),
             'sd' = sd(long, na.rm = TRUE),
             'IQR' = IQR(long, na.rm = TRUE),
             'n_distinct' = n_distinct(long, na.rm = TRUE))

covid_data %>%
   select(lat) %>%
   summarise('minimum' = min(lat, na.rm = TRUE),
             'maximum' = max(lat, na.rm = TRUE),
             'mean' = mean(lat, na.rm = TRUE),
             'median' = median(lat, na.rm = TRUE),
             'sd' = sd(lat, na.rm = TRUE),
             'IQR' = IQR(lat, na.rm = TRUE),
             'n_distinct' = n_distinct(lat, na.rm = TRUE))

covid_data %>%
   select(long, lat) %>%
   gather() %>%
   group_by(key) %>%
   summarise('minimum' = min(value, na.rm = TRUE),
             'maximum' = max(value, na.rm = TRUE),
             'mean' = mean(value, na.rm = TRUE),
             'median' = median(value, na.rm = TRUE),
             'sd' = sd(value, na.rm = TRUE),
             'IQR' = IQR(value, na.rm = TRUE),
             'n_distinct' = n_distinct(value, na.rm = TRUE))


# 3.6 - Confirmed / Deaths / Recovered ----

cls()
covid_data %>%
   select(confirmed, deaths, recovered, new_confirmed, new_deaths, new_recovered) %>%
   gather() %>%
   group_by(key) %>%
   summarise('minimum' = min(value, na.rm = T),
             'maximum' = max(value, na.rm = T),
             'mean' = mean(value, na.rm = T),
             'median' = median(value, na.rm = T),
             'sd' = sd(value, na.rm = T),
             'IQR' = IQR(value, na.rm = T)
   )

covid_data %>%
   filter(confirmed < 0 | deaths < 0 | recovered < 0) %>%
   select(country, subregion, date, confirmed, deaths, recovered) %>%
   arrange(desc(deaths), desc(confirmed))

covid_data %>%
   filter(new_confirmed < 0 | new_deaths < 0 | new_recovered < 0) %>%
   select(country, subregion, date, confirmed, deaths, recovered, new_confirmed, new_deaths, new_recovered) %>%
   arrange(country, subregion, date, desc(new_deaths), desc(new_confirmed))



# 3.7 - The worst country cases ----
covid_data %>%
   filter(date == max(date)) %>%
   select(country, date, confirmed, deaths, recovered) %>%
   group_by(country, date) %>%
   summarise(confirmed = sum(confirmed),
             deaths = sum(deaths),
             recovered = sum(recovered),
             pct_deaths = 100 * deaths / confirmed) %>%
   ungroup() %>%
   arrange(desc(confirmed), desc(deaths)) %>%
   print(n = 25)

covid_data %>%
   filter(date == max(date)) %>%
   select(country, date, confirmed, deaths, recovered) %>%
   group_by(country, date) %>%
   summarise(confirmed = sum(confirmed),
             deaths = sum(deaths),
             recovered = sum(recovered),
             pct_deaths = 100 * deaths / confirmed) %>%
   ungroup() %>%
   arrange(desc(deaths), desc(confirmed)) %>%
   print(n = 25)

covid_data %>%
   select(country, date, new_confirmed, new_deaths, new_recovered) %>%
   group_by(country, date) %>%
   summarise(new_confirmed = sum(new_confirmed),
             new_deaths = sum(new_deaths),
             new_recovered = sum(new_recovered)) %>%
   ungroup() %>%
   arrange(desc(new_deaths), desc(new_confirmed)) %>%
   print(n = 25)

covid_data %>%
   filter(country != 'US') %>%
   select(country, date, new_confirmed, new_deaths, new_recovered) %>%
   group_by(country, date) %>%
   summarise(new_confirmed = sum(new_confirmed),
             new_deaths = sum(new_deaths),
             new_recovered = sum(new_recovered)) %>%
   ungroup() %>%
   arrange(desc(new_deaths), desc(new_confirmed)) %>%
   print(n = 25)


# 3.8 - Notable Countries ----
covid_data %>%
   filter(country %in% c('US',
                         'United Kingdom',
                         'Italy',
                         'Spain',
                         'France',
                         'Germany',
                         'Sweden',
                         'Brazil',
                         'Russia',
                         'India',
                         'South Africa',
                         'China',
                         'Peru')) %>%
   filter(date == max(date)) %>%
   select(country, date, confirmed, deaths, recovered) %>%
   group_by(country) %>%
   summarise(confirmed = sum(confirmed),
             deaths = sum(deaths),
             recovered = sum(recovered),
             pct_deaths = 100 * deaths / confirmed,
             pct_recovered = 100 * recovered / confirmed) %>%
   ungroup() %>%
   arrange(desc(confirmed), desc(deaths))



# 4 - Data Visualization ----

cls()


# 4.1 - Scatter Plot ----

covid_data %>%
   filter(date == max(date)) %>%
   select(-subregion, -lat, -long) %>%
   group_by(country) %>%
   summarise(confirmed = sum(confirmed),
             deaths = sum(deaths)) %>%
   ggplot(aes(x = confirmed, y = deaths)) +
   geom_point()


# 4.2 - Bar Charts ----

graphics.off()

covid_data %>%
   filter(date == max(date)) %>%
   select(-subregion, -lat, -long) %>%
   group_by(country) %>%
   summarise(confirmed = sum(confirmed),
             deaths = sum(deaths),
             recovered = sum(recovered)) %>%
   arrange(desc(confirmed), desc(deaths)) %>%
   slice_head(n = 15) %>%
   ggplot(aes(x = fct_reorder(country, confirmed), y = confirmed)) +
   geom_bar(stat = 'identity', fill = 'darkred') +
   labs(x = '')

covid_data %>%
   filter(date == max(date)) %>%
   select(-subregion, -lat, -long) %>%
   group_by(country) %>%
   summarise(confirmed = sum(confirmed),
             deaths = sum(deaths),
             recovered = sum(recovered)) %>%
   arrange(desc(deaths), desc(confirmed)) %>%
   slice_head(n = 15) %>%
   ggplot(aes(x = deaths, y = fct_reorder(country, deaths))) +
   geom_bar(stat = 'identity', fill = 'darkred') +
   labs(y = '')


# 4.3 - Line Charts ----

graphics.off()

covid_data %>%
   filter(country != 'China') %>%
   select(-subregion, -lat, -long) %>%
   group_by(date) %>%
   summarise(confirmed = sum(confirmed),
             deaths = sum(deaths),
             recovered = sum(recovered)) %>%
   ggplot(aes(x = date, y = confirmed)) +
   geom_line(color = 'red')

covid_data %>%
   mutate(us_ind = if_else(condition = country == 'US',
                           true = 'US',
                           false = 'Not US')) %>%
   select(-subregion, -lat, -long) %>%
   group_by(us_ind, date) %>%
   summarise(confirmed = sum(confirmed),
             deaths = sum(deaths),
             recovered = sum(recovered)) %>%
   ggplot(aes(x = date, y = confirmed)) +
   geom_line(aes(color = us_ind)) +
   scale_color_manual(values = c('red', 'navy'))

covid_data %>%
   filter(country %in% c("US", "China", "Italy", "Spain", "France", "Brazil", "India"),
          date < max(date)) %>%
   group_by(country, date) %>%
   summarise(confirmed = sum(confirmed),
             deaths = sum(deaths),
             recovered = sum(recovered)) %>%
   ggplot(aes(x = date, y = confirmed)) +
   geom_line(aes(color = country)) +
   scale_color_manual(values = c('darkgreen', 'red', 'violet', 'pink', 'yellow', 'orange', 'navy'))

covid_data %>%
   filter(country %in% c("US", "China", "Italy", "Spain", "France", "Brazil", "India"),
          date < max(date)) %>%
   group_by(country, date) %>%
   summarise(confirmed = sum(confirmed),
             deaths = sum(deaths),
             recovered = sum(recovered)) %>%
   ggplot(aes(x = date, y = deaths)) +
   geom_line(aes(color = country)) +
   scale_color_manual(values = c('darkgreen', 'red', 'violet', 'pink', 'yellow', 'orange', 'navy'))

covid_data %>%
   filter(country == 'Brazil',
          date < max(date)) %>%
   select(-subregion, -lat, -long) %>%
   group_by(date) %>%
   summarise(new_confirmed = sum(new_confirmed),
             new_deaths = sum(new_deaths),
             new_recovered = sum(new_recovered)) %>%
   plot_time_series(date,
                    new_confirmed,
                    .color_var = week(date),
                    .interactive = FALSE,
                    .color_lab = 'Week'
   )

covid_data %>%
   filter(country == 'Brazil',
          date < max(date)) %>%
   select(-subregion, -lat, -long) %>%
   group_by(date) %>%
   summarise(new_confirmed = sum(new_confirmed),
             new_deaths = sum(new_deaths),
             new_recovered = sum(new_recovered)) %>%
   plot_time_series(date,
                    new_deaths,
                    .color_var = week(date),
                    .interactive = FALSE,
                    .color_lab = 'Week'
   )

covid_data %>%
   filter(country == 'Brazil',
          date < max(date)) %>%
   select(-subregion, -lat, -long) %>%
   group_by(date) %>%
   summarise(new_confirmed = sum(new_confirmed),
             new_deaths = sum(new_deaths),
             new_recovered = sum(new_recovered)) %>%
   plot_time_series(date,
                    new_deaths,
                    .color_var = week(date),
                    .interactive = TRUE,
                    .plotly_slider = TRUE,
                    .color_lab = 'Week'
   )



# 5 - Some Data Issues ----

graphics.off()
cls()


# 5.1 - Isolating 12 Worst Country Cases ----

covid_data %>%
   filter(date == max(date)) %>%
   group_by(country) %>%
   summarise(confirmed = sum(confirmed)) %>%
   arrange(desc(confirmed)) %>%
   top_n(n = 12, wt = confirmed) ->
   covid_worst_12


# 5.2 - Plotting Small Multiple Charts ----

covid_data %>%
   filter(country %in% covid_worst_12$country) %>%
   group_by(country, date) %>%
   summarise(new_confirmed = sum(new_confirmed)) %>%
   ggplot(aes(x = date, y = new_confirmed)) +
   geom_line() +
   facet_wrap(~ country, ncol = 4, scales = "free")


# 5.3 - Checking Spanish Data for April ----

covid_data %>%
   filter(country == 'Spain') %>%
   filter(month(date) == '4') %>%
   select(country, date, confirmed, deaths, recovered, new_confirmed, new_deaths, new_recovered) %>%
   print(n = 30)



# 6 - Deepening Country Comparison ----

graphics.off()
cls()


# 6.1 - Isolating 20 Worst Country Cases ----

covid_data %>%
   filter(date == max(date)) %>%
   group_by(country) %>%
   summarise(confirmed = sum(confirmed)) %>%
   arrange(desc(confirmed)) %>%
   top_n(n = 20, wt = confirmed) ->
   covid_worst_20


# 6.2 - Plotting Small Multiple Charts ----

covid_data %>%
   filter(country %in% covid_worst_20$country) %>%
   group_by(country, date) %>%
   summarise(new_confirmed = sum(new_confirmed)) %>%
   ggplot(aes(x = date, y = new_confirmed)) +
   geom_line() +
   facet_wrap(~ country, ncol = 5, scales = 'free')


6# 6.3 - Doing it the dumb way... ----

covid_success <- function(input_value) {

   result = case_when(
      input_value == 'Bangladesh' ~ 'Needs work',
      input_value == 'Brazil' ~ 'Needs work',
      input_value == 'Canada' ~ 'Almost there',
      input_value == 'Chile' ~ 'Needs work',
      input_value == 'China' ~ 'Winning',
      input_value == 'France' ~ 'Almost there',
      input_value == 'Germany' ~ 'Almost there',
      input_value == 'India' ~ 'Needs work',
      input_value == 'Iran' ~ 'Needs work',
      input_value == 'Italy' ~ 'Almost there',
      input_value == 'Mexico' ~ 'Needs work',
      input_value == 'Pakistan' ~ 'Needs work',
      input_value == 'Peru' ~ 'Needs work',
      input_value == 'Qatar' ~ 'Needs work',
      input_value == 'Russia' ~ 'Needs work',
      input_value == 'Saudi Arabia' ~ 'Needs work',
      input_value == 'Spain' ~ 'Almost there',
      input_value == 'Turkey' ~ 'Almost there',
      input_value == 'United Kingdom' ~ 'Almost there',
      input_value == 'US' ~ 'Needs work',
      TRUE ~ 'other'
   )
   return(result)
}

covid_success('Brazil')
covid_success('Canada')
covid_success('China')


# 6.4 - Sophisticating... ----

covid_data %>%
   filter(country %in% covid_worst_20$country) %>%
   group_by(country, date) %>%
   summarise(new_confirmed = sum(new_confirmed)) %>%
   mutate(new_confirmed_rollmean_7 = rollmean(x = new_confirmed,
                                              k = 7,
                                              na.pad = TRUE,
                                              align = 'right')) %>%
   mutate(covid_success = covid_success(country)) %>%
   select(country, date, covid_success, new_confirmed, new_confirmed_rollmean_7) %>%
   ggplot(aes(x = date, y = new_confirmed_rollmean_7)) +
   geom_line(aes(color = covid_success)) +
   labs(color = 'Covid\nSuccess',
        title = 'Covid19 New Cases\n7 day rolling average') +
   facet_wrap(~ country, scales = 'fixed') +
   scale_color_manual(values = c('Winning' = '#107F12',
                                 'Almost there' = '#FDA520',
                                 'Needs work' = '#FC101D')) +
   theme(axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         panel.background = element_blank(),
         text = element_text(family = 'Avenir'),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         axis.line.x = element_line(color = '#333333'),
         axis.title.y = element_blank(),
         axis.title.x = element_blank(),
         plot.title = element_text(size = 22, hjust = .5),
         legend.key = element_rect(fill = 'white')
   )



# 9 - Clean Temporary Files ----

graphics.off()
rm(list = ls(pattern = 'covid_'))
cls()
rm(list = ls())
