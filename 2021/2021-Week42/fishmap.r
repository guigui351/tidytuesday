library(tidyverse)
library(sf)
library(maptools)
library(cartogram)
library(patchwork)
library(showtext)
library(janitor)
library(colorspace)
library(CoordinateCleaner)
library(gggibbous)
library(ggthemes)
library(ggtext)
library(here)

captured_vs_farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv') %>% 
  clean_names() %>% 
  filter(year == 2018)

consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv') %>%  
  clean_names() %>% 
  filter(year == 2017) %>% 
  select(code, fish_seafood_food_supply_quantity_kg_capita_yr_fao_2020) %>% 
  rename(fish_seafood_capita = fish_seafood_food_supply_quantity_kg_capita_yr_fao_2020)

font_add_google("Eczar", "Eczar")
font_add_google("Playfair Display", "Playfair Display")
font_add_google("Bitter", "Bitter")
font_add_google("Roboto Mono", "Roboto Mono")

showtext_auto()

theme_set(theme_map(base_family = "Playfair Display"))
theme_update(
  plot.title = element_markdown(family = "Bitter", size = 32, hjust = 0.5),
  plot.subtitle = element_markdown(family = "Montserrat", color = "grey80", size = 18, face = "bold", hjust = 0.5, margin = margin(b = 6)),
  plot.caption = element_markdown(family = "Bitter", color = "grey60", size = 9, hjust = 1, lineheight = 1.2),
  legend.position = "bottom",
  legend.justification = c(0.5, 0),
  legend.title = element_text(family = "Bitter", color = "grey60", face = "bold", size = 14),
  legend.text = element_text(family = "Roboto Mono", color = "grey60", size = 10)
)

options(scipen=10000)

data("wrld_simpl")
data("countryref")

country <- countryref %>% 
  filter(is.na(source) & type == "country") %>% 
  group_by(name) %>% 
  slice(1) %>%         
  ungroup()

df <- captured_vs_farmed %>% 
  mutate(aquaculture_production_metric_tons = if_else(is.na(aquaculture_production_metric_tons), 0, aquaculture_production_metric_tons),
         capture_fisheries_production_metric_tons = if_else(is.na(capture_fisheries_production_metric_tons), 0, capture_fisheries_production_metric_tons)) %>% 
  left_join(consumption, by = c("code"="code")) %>% 
  mutate(total_production = aquaculture_production_metric_tons + capture_fisheries_production_metric_tons,
         prop_aquaculture = aquaculture_production_metric_tons / total_production,
         prop_fisheries = capture_fisheries_production_metric_tons / total_production) %>% 
  drop_na() %>% 
  mutate(code = if_else(code == "UK", "GB", code)) %>% 
  mutate(max = max(fish_seafood_capita, na.rm = T),
         label_leg = if_else(entity == "Iceland", "With 90.7 kg per person \n per year,  Icelandic people \n consume far more seafood than any other country", NA_character_))

df_final <-
  wrld_simpl %>%
  st_as_sf() %>%
  st_transform(crs = "+proj=robin") %>% 
  mutate(code = as.character(ISO3)) %>%
  left_join(df, by ="code") %>% 
  filter(NAME != "Antarctica")

country_df <- country %>% 
  mutate(code = as.character(iso3)) %>%
  left_join(df, by ="code") %>% 
  filter(!is.na(total_production))

centroid_country <- st_as_sf(country_df, coords = c("centroid.lon", "centroid.lat"), 
                  crs = 4326, agr = "constant")

options(scipen=1000000)

map <- ggplot(df_final) +
  geom_sf(aes(geometry = geometry,
              fill = fish_seafood_capita),
          color = "grey80", size = 0.1) +
  geom_point(data = centroid_country, 
             aes(geometry = geometry, size = total_production),
             stat = StatSfCoordinates,
             fun.geometry = sf::st_centroid,
             color = lighten("white", .65, space = "combined"),
             fill = "transparent",
             stroke = .3) +
  geom_moon(data = centroid_country, 
            aes(geometry = geometry, ratio = prop_fisheries, size = total_production), 
            stat = StatSfCoordinates,
            fun.geometry = sf::st_centroid,
            fill = "white", 
            color = "#855464",
            stroke = .3,
            right = FALSE) +
  geom_moon(data = centroid_country, 
            aes(geometry = geometry, ratio = prop_aquaculture, size = total_production), 
            stat = StatSfCoordinates,
            fun.geometry = sf::st_centroid,
            fill = "#855464", 
            color = "#855464",
            stroke = .3) +
  coord_sf(clip = "on", expand = TRUE) +
  rcartocolor::scale_fill_carto_c(palette = "Darkmint",
                                  direction = 1,
                                  limits = c(0, 100),  ## max percent overall
                                  breaks = seq(0, 100, by = 25),
                                  labels = glue::glue("{seq(0, 100, by = 25)}kg")) +
  scale_size(range = c(1, 20), guide = F) + 
  guides(fill = guide_colorbar(barheight = unit(2.3, units = "mm"),  
                               barwidth = unit(230, units = "mm"),
                               direction = "horizontal",
                               title = "Fish and seafood consumption per capita (kg)",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "top",
                               title.hjust = 0.5)) +
  theme(legend.direction = "horizontal")

seafood_supply <- df %>% 
  mutate(total_seafood_capita = sum(fish_seafood_capita, na.rm = TRUE)) %>% 
  arrange(desc(fish_seafood_capita)) %>% 
  mutate(fivefirst = if_else(row_number() <= 5, 1, 0)) %>% 
  group_by(fivefirst) %>% 
  summarize(prop_five = sum(fish_seafood_capita, na.rm = TRUE)/total_seafood_capita*100) %>% 
  slice(n())

df_leg <- 
  tribble(
    ~x, ~y, ~label, ~size, ~prop_aquaculture, ~prop_fisheries,
     .3,  .18, "40,000,000 tons", 15, 0.8168214, 0.1831786,
    .35,  .12, "20,000,000 tons",  8, 0.8168214, 0.1831786,
    .40, .075, "10,000,000 tons",  4, 0.8168214, 0.1831786,
    .43, .045, "1.000,000 tons",   .4, 0.8168214, 0.1831786
  )

df_labs <-
  tribble(
    ~x, ~y, ~label, ~color,
    .06, .6, "<span style='font-size:10pt'>**China as a reference**<br>Production of more than<br>80 Millions of tons over 2018</span>", "A",
    .05, .3, "**Total production of seafood**<br>per country", "A",
    .32, .65, "**Proportion of Aquaculture production**<br><span style='font-size:9pt'>(farming of aquatic organisms)</span>", "A",
    .5, .27, "**Proportion of Capture fisheries production**<br><span style='font-size:9pt'>(fishing and catching wild fish and shellfish)</span>", "grey88"
  )

df_lines <-
  tribble(
    ~x, ~y, ~xend, ~yend, ~curv, ~color,
    .08, .57, .2, .43, .42, "A",  ## *China as a reference
    .07, .33, .162, .39, .20, "A",  ##  production of seafood
    .34, .61, .21, .4, .38, "A",  ## AquacultureCapture fisheries
    .32, .26, .170, .385, -.42, "grey88"  ## Capture fisheries
  )

legend <-
  df_final %>%
  mutate(max = max(
    total_production,
    na.rm = T
  )) %>%
  filter(entity == "China") %>%
  ggplot(aes(x = .2,
             y = .4)) +
  # legend moon facet
  geom_point(
    size = 20,
    color = lighten("#855464", .65, space = "combined"),
    shape = 21,
    fill = "transparent",
    stroke = 1.1
  ) +
  geom_moon(
    aes(ratio = prop_aquaculture), 
    size = 20,
    fill = "#855464", 
    color = "#855464",
    stroke = .3
  ) +
  geom_moon(
    aes(ratio = prop_fisheries), 
    size = 20,
    fill = "white", 
    color = "#855464",
    stroke = .3,
    right = FALSE
  ) +
  geom_richtext(
    data = df_labs,
    aes(x = x, y = y, label = label, color = color),
    family = "sans",
    size = 4,
    lineheight = .9,
    fill = NA,
    label.color = NA
  ) +
  geom_curve(
    data = df_lines,
    aes(
      x = x, xend = xend,
      y = y, yend = yend,
      color = color
    ),
    curvature = -.43
  ) +
  # legend moon facet (smallest ones)
  geom_point(data = df_leg, aes(x = x,
                 y = y, size = size),
    #size = 15,
    color = lighten("#855464", .65, space = "combined"),
    shape = 21,
    fill = "transparent",
    stroke = 1.1
  ) +
  geom_moon(data = df_leg, 
    aes(x = x, y = y, size = size, ratio = prop_aquaculture),
    fill = "#855464", 
    color = "#855464",
    stroke = .3
  ) +
  geom_moon(data = df_leg, 
    aes(x = x, y = y, size = size, ratio = prop_fisheries), 
    fill = "white", 
    color = "#855464",
    stroke = .3,
    right = FALSE
  ) +
  geom_text(data = df_leg, 
    aes(x = x-.035, y = y, label = label),
    size = 2.5,
    color = "grey20",
    family = "Playfair Display",
    hjust  = 1) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(-0.5, 1.5)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(
    values = c("#855464", "grey20"),
    guide = F
  ) +
  scale_size_area(max_size = 20/1.3, guide = F) +
  theme(plot.margin = margin(0, 0, 0, 0))

  
# Insert legend inside the map
map + annotation_custom(
    grob = ggplotGrob(legend),
    xmin = -23000000,
    xmax =   2300000,
    ymin = -6000000,
    ymax =  7000000
) + 
labs(title ="<b style='font-size:24pt'>The future of food from the sea in the World (Marine Capture Vs Aquaculture)</b>",
     subtitle = "<br>While <span style='color:#855464 font-size:18pt'>**China**</span> is by far the major fish producer with more than 80,000,000 of tons in 2018, <span style='color:#025B40 font-size:18pt'>**Iceland**</span> has the largest seafood consumption in the World in terms of kg/per capita.<br>The top 5 countries (others are <span style='color:#025B40'> Maldives, Kiribati, Hong Kong, and Malaysia</span>) account for 11.89% of it.",
     caption = "<br>*Visualization by Guillaume Abgrall • Data by OurWorldinData.org*</span>")
  
path <- here::here("global_fishing")
ggsave(glue::glue("{path}.pdf"), width = 24, height = 16, device = cairo_pdf)
pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"), 
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 450)