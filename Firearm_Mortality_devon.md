Firearm Mortality\_devon
================
Devon Morgan
11/13/2018

Import data
-----------

Imported firearm mortality dataset from CDC Wonder database.

``` r
firearm_mortality = read_csv("./data/cdc_firearm_mortality_data.csv", na = "Unreliable") %>% 
    janitor::clean_names() %>% 
    select(-ten_year_age_groups_code, -injury_mechanism_all_other_leading_causes_code, -race_code, death_cause= injury_mechanism_all_other_leading_causes) %>% 
    mutate(ten_year_age_groups = factor(ten_year_age_groups, levels = c("1-4 years", "5-14 years", "15-24 years", "25-34 years", "35-44 years", "45-54 years", "55-64 years", "65-74 years", "75-84 years", "85+ years")))

firearm_mortality_summary = read_excel("./data/cdc_firearm_all_ages.xlsx") %>% 
    janitor::clean_names() %>% 
    select(-year_code, -injury_mechanism_all_other_leading_causes_code, death_cause= injury_mechanism_all_other_leading_causes)
```

Added state abbreviations to dataset:

``` r
st_crosswalk = tibble(state = state.name) %>%
   bind_cols(tibble(abb = state.abb)) %>% 
     bind_rows(tibble(state = "District of Columbia", abb = "DC"))
```

Map of Crude Death Rates in 1999, 2007, 2012, 2016
--------------------------------------------------

![](Firearm_Mortality_devon_files/figure-markdown_github/unnamed-chunk-3-1.png)![](Firearm_Mortality_devon_files/figure-markdown_github/unnamed-chunk-3-2.png)![](Firearm_Mortality_devon_files/figure-markdown_github/unnamed-chunk-3-3.png)![](Firearm_Mortality_devon_files/figure-markdown_github/unnamed-chunk-3-4.png)![](Firearm_Mortality_devon_files/figure-markdown_github/unnamed-chunk-3-5.png)

Spaghetti Plot of Crude Firearm Death Rate, by State, 1999-2016
---------------------------------------------------------------

``` r
left_join(firearm_mortality_summary, st_crosswalk, by = "state") %>% 
  ggplot(aes(x = year, y = crude_rate, color = abb)) + 
  geom_line(alpha = 0.7, size = 0.5) +
  labs(
    title = "Crude Firearm Death Rate in Each State, 1999 to 2016",
    x = "Year",
    y = "Crude Death Rate",
    caption = "Data from the CDC Wonder Database"
  ) + 
  theme_bw() + 
    viridis::scale_color_viridis(discrete = TRUE,
                               name = "State")
```

![](Firearm_Mortality_devon_files/figure-markdown_github/unnamed-chunk-4-1.png)

Crude Firearm Death Rate by Age Group and Race, 1999, 2016
----------------------------------------------------------

``` r
firearm_mortality %>% 
    filter(year == 1999) %>% 
    group_by(ten_year_age_groups, race) %>%
    summarize(n_deaths = sum(deaths),
                        n_population = sum(population)) %>% 
    mutate(crude_rate = (n_deaths/n_population)*100000) %>% 
    ggplot(aes(fill= race, x = ten_year_age_groups, y = crude_rate, color = race)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle=60, hjust=1)) + 
    labs(
    title = "Crude Firearm Death Rate Across All States by Age Group and Race, 1999",
    x = "Age Group",
    y = "Crude Firearm Death Rate",
    caption = "Data from the CDC Wonder Dataset")
```

![](Firearm_Mortality_devon_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
firearm_mortality %>% 
    filter(year == 2016) %>% 
    group_by(ten_year_age_groups, race) %>%
    summarize(n_deaths = sum(deaths),
                        n_population = sum(population)) %>% 
    mutate(crude_rate = (n_deaths/n_population)*100000) %>% 
    ggplot(aes(fill= race, x = ten_year_age_groups, y = crude_rate, color = race)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle=60, hjust=1)) + 
    labs(
    title = "Crude Firearm Death Rate Across All States by Age Group and Race, 2016",
    x = "Age Group",
    y = "Crude Firearm Death Rate",
    caption = "Data from the CDC Wonder Dataset")
```

![](Firearm_Mortality_devon_files/figure-markdown_github/unnamed-chunk-5-2.png)
