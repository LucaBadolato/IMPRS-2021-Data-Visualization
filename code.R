#===============================================================================
# 2021-06-18 -- MPIDR dataviz course
# Final Assignment: Maps challange
# Author: Luca Badolato 
#===============================================================================

# clear the working environment
rm(list=ls())

# load and (in case) install required packages 
listofpackages = c("tidyverse",
                   "janitor",
                   "sf",
                   "ggthemes", 
                   "eurostat",
                   "rmapshaper")
              
for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}

# download the data (dataset name found at http://ec.europa.eu/eurostat/web/regions/data/database)
df <- get_eurostat("demo_r_pjanaggr3")

# explore available years and geographical locations
time <- df %>%
  select(time) %>% 
  unique() 

geo <- df %>% filter(
  geo  %>% str_sub(1,2) == "IT",  # only Italy
  geo %>% paste %>% nchar == 5, # only NUTS3 level
) %>%
select(geo) %>% unique()


# filter data of interest 
df_it <- df %>%
  mutate(
    time = time %>% lubridate::year()
  ) %>% 
   filter(
    sex == "T" , # total population
    geo  %>% str_sub(1,2) == "IT",  # only Italy
    geo %>% paste %>% nchar == 5, # only NUTS3 level
    time %in% c("2020", "2001"), 
    age == "TOTAL"
    ) %>%
  transmute(
    id = geo,
    year = time, 
    pop = values 
  ) 
  
# reshape to wide format 
df_it <- df_it %>% 
  pivot_wider(names_from = year, values_from = pop)

# define variable of interest: population change 
df_it <- df_it %>% 
  mutate(
    pop_change = (`2020` - `2001`)/`2001`
  )

# the built-in dataset of EU boundaries
gd <- eurostat_geodata_60_2016 %>% 
  clean_names()
# transform the projection for the one suitable for Europe
gdtr <- gd %>% 
  st_transform(crs = 3035)

# filter Italy
gd_it <- gdtr %>% 
  filter(cntr_code == "IT", levl_code == 3) 

# join Eurostat and spatial datasets
dj_it <- left_join(gd_it, df_it, "id")

# get the NUTS2 (regions) bords to be plotted in the map
bord <- gdtr %>% 
  filter(cntr_code == "IT", levl_code == 2) %>% 
  ms_innerlines()

# visualization 
 dj_it %>% 
  ggplot()+
  geom_sf()+
  coord_sf(datum = NA)+
  geom_sf(aes(fill = pop_change), color=NA)+
  geom_sf(data = bord, color = "black", size=0.1)+
  theme_map()+
  labs(fill = "", 
       title = "POPULATION GROWTH RATE (2001 - 2020)",
       subtitle = "Population growth rate from 2001 to 2020 in Italy at NUTS3 level",
       caption = paste0("Source: Eurostat\n",
                        "@BadolatoLuca")) +
  scale_fill_distiller(palette = "RdBu",
                       direction=1,
                       limits = c(-0.177985, 0.177985),
                       breaks = c(-0.1, 0, 0.1),
                       labels = c("-10%", "0", "+10%")
                       ) +
  theme(legend.position = c(.0, .06),
        legend.direction = "horizontal",
        #legend.text = element_text("0,1,2"),
        plot.title = element_text(hjust = 0.5, 
                                  vjust = 0.1,
                                  size = 16, 
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,
                                     vjust = 0.1,
                                     size = 10),
        plot.caption = element_text(size = 7,
                                    hjust = 1))
  
ggsave("BADOLATO-challenge.png", width = 6, height = 7,)

# end
