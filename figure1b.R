library(tidyverse)
library(readxl)
library(eurostat)
library(scales)

load("colors.RData")
load("geo.RData")

## Load Eurostat datasets if not already loaded ------

datasets_eurostat <- c("prc_hicp_midx")

if (!(datasets_eurostat %in% ls())){
  for (dataset in datasets_eurostat){
    assign(dataset, 
           get_eurostat(dataset, stringsAsFactors = F, cache = F) |>
             rename(date = TIME_PERIOD)
    )
  }
}

# Figure 1 ------

figure1 <- prc_hicp_midx |>
  filter(unit == "I15",
         coicop %in% c("CP0711"),
         geo %in% c("DE", "FR", "IT", "NL", "ES")) |>
  left_join(geo, by = "geo") |>
  select(geo, Geo, coicop, date, values) |>
  filter(date >= as.Date("2019-01-01")) |>
  arrange(date) |>
  group_by(Geo) |>
  mutate(values = 100*values/values[date == as.Date("2019-01-01")]) |>
  left_join(colors, by = c("Geo" = "country"))

ggplot(data = figure1) + geom_line(aes(x = date, y = values, color = color)) + 
  theme_minimal() + xlab("") + ylab("Prix des véhicules automobiles - CP0711\nIndice Janv. 2019 = 100") +
  scale_x_date(breaks = as.Date(paste0(seq(2019, 2100, 1), "-01-01")),
               labels = date_format("%Y")) +
  scale_y_log10(breaks = seq(0, 200, 5)) +
  scale_color_identity() +
  ggimage::geom_image(data = figure1 |>
                        group_by(date) |>
                        filter(n() == 5) |>
                        arrange(values) |>
                        mutate(dist = min(values[2]-values[1],values[3]-values[2],
                                          values[4]-values[3],values[5]-values[4])) |>
                        arrange(-dist, date) |>
                        head(5) |>
                        mutate(image = paste0("flags/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
                      aes(x = date, y = values, image = image), asp = 1.5) +
  theme(legend.position = "none") +
  labs(caption = "Source: Eurostat, calculs de l'auteur")


ggsave("figure1b.png", width = 1.25*6, height = 1.25*3.375, bg = "white", dpi = 150)
ggsave("figure1b.pdf", width = 1.25*6, height = 1.25*3.375, dpi = 150)


