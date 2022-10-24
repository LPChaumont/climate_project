library(tidyverse)
library(R.utils)
library(ncdf4)
library(data.table)
library(ggtext)

url <- "https://data.giss.nasa.gov/pub/gistemp/gistemp250_GHCNv4.nc.gz"
download.file(url, destfile = "gistemp250_GHCNv4.nc.gz")
gunzip("gistemp250_GHCNv4.nc.gz")

nc_data <- nc_open("gistemp250_GHCNv4.nc")
# Save the print(nc) dump to a text file
{
  sink("gistemp250_GHCNv4.txt")
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

t_anamoly.array <- ncvar_get(nc_data, "tempanomaly") # store the data in a 3-dimensional array
dim(t_anamoly.array) 

fillvalue <- ncatt_get(nc_data, "tempanomaly", "_FillValue")
t_anamoly.array[t_anamoly.array == fillvalue$value] <- NA

t_data <- as.data.table(t_anamoly.array) %>% 
  as_tibble() %>% 
  select(longitude = V1, latitude = V2, time = V3, t_diff = value) %>% 
  mutate(longitude = lon[longitude],
         latitude = lat[latitude],
         time = t[time] + as.Date("1800-01-01"),
         year = year(time)) %>% 
  group_by(year, longitude, latitude) %>%
  summarize(t_diff = mean(t_diff), .groups = "drop") %>% 
  filter(between(year, 1950, 2021),
         between(latitude, 42, 83),
         between(longitude, -141, -53)) %>% 
  mutate(decade = floor(year/10)*10,
         single = year %% 10)



t_data %>% 
  mutate(t_diff = case_when(t_diff < -4 ~ -4,
                            t_diff > 4 ~ 4,
                            TRUE ~ t_diff)) %>% 
  # filter(year == 2000) %>% 
  ggplot(aes(x = longitude, y = latitude, fill = t_diff)) +
  geom_raster() +
  scale_fill_gradient2(name = "Temperature<br>anomaly (°C)",
                       low = "darkblue", mid = "white", high = "darkred",
                       midpoint = 0,
                       limits = c(-4.2, 4.2),
                       breaks = c(-4, -2, 0, 2, 4)) +
  coord_fixed(expand = FALSE) +
  facet_grid(single ~ decade, switch = "y") +
  labs(title = "Canada annual temperature anomalies between 1950 and 2021",
       subtitle = "Deviations from the corresponding 1951-1980 means",
       caption = "Data: https://data.giss.nasa.gov/gistemp/",
       x = NULL,
       y = NULL) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(color = "white", face = "bold"),
        plot.caption = element_text(color = "white"),
        plot.subtitle = element_text(color = "white"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        panel.grid = element_blank(),
        strip.text.y = element_blank(),
        strip.text.x = element_text(angle = 0, colour = "white"),
        strip.background = element_blank(),
        legend.position = c(0.95, 0.45),
        legend.direction = "vertical",
        legend.text = element_text(colour = "white", size = 5),
        legend.title = element_markdown(colour = "white", size = 6),
        legend.background = element_blank()) +
  guides(fill = guide_colorbar(title.position = "top",
                               barwidth = unit(0.8, "lines")))
         
ggsave("figure/canada_temp_anomaly.png", width = 8, height = 6)

nc_close(nc_data)
unlink("gistemp250_GHCNv4.nc")
unlink("gistemp250_GHCNv4.txt")
