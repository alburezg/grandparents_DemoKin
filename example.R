Now we want to match the dfs of grandchildren with that of the grandparents. Let's go back to our example and consider the number of grandparents for a Focal aged 10:

```{r}

gp10 <- 
  gp %>% 
  filter(Location == "China", year == 2000, age_focal %in% c(10))

gd10 <- 
  gd %>% 
  filter(Location == "China", year == 2000, age_gd %in% c(10))

plot(gp10$gp)
# This is the age distribution of grandparents who have a grandchild aged 10
lines(gd10$share_gd)
lines(gd10$gd)

gpd10 <- 
  left_join(
    gp10, gd10
    , by = c("Location", "year", "age_gp" = "age_focal", "age_focal" = "age_gd")
    ) %>% 
  mutate(iso3 = countrycode(Location, origin = "country.name", destination = "iso3c"))

gpd10 %>% 
  mutate(age_focal = as.character(age_focal)) %>%
  pivot_longer(gp:share_gd) %>% 
  ggplot(aes(x = age_gp, y = value, colour = name)) +
  geom_line() +
  theme_bw()

```

```{r}
# Now we get the total number of grandparents

# gp_num_age10 <- 
  gpd10 %>% 
  group_by(iso3, year, age_focal) %>% 
  summarise(gp_sum = sum(gp*share_gd)) %>% 
  ungroup() %>% 
  # gp_sum is the expected number of 'unique' grandparents for a Focal aged x
  # accounting for the fact some grandparents are shared by multiple grandchildren
  left_join(
    pop %>% select(iso3, year, age, pop_un)
    , by = c("iso3", "year", "age_focal" = "age")
  ) %>%
  mutate(gp_num = pop_un * gp_sum) %>% 
  kable()

```