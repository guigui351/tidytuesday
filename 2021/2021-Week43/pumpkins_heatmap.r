# Load packages
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(patchwork)
library(ggtext)
library(ggflags)
library(countrycode)

# add fonts
font_add_google("Poppins", "Poppins")
font_add_google("Bitter", "Bitter")
font_add_google("Merienda", "Merienda")
showtext_auto()

# import tidytuesday data
tuesdata <- tidytuesdayR::tt_load(2021, week = 43)
pumpkins <- tuesdata$pumpkins

# clean data 
pumpkins_clean <- pumpkins %>% 
  separate(id, c("Year", "Type"), remove = FALSE) %>% 
  filter(Type == "P" & place != "EXH" & weight_lbs != "") %>% 
  filter(!country %in% c("United States", "Canada", "New Zealand", "Australia", "Japan")) %>%
  mutate(weight_lbs = as.numeric(gsub(",", "",  weight_lbs)),
         est_weight = as.numeric(gsub(",", "",  est_weight)),
         weight_kg = as.numeric(weight_lbs)*0.453592,
         est_weight_kg = as.numeric(est_weight)*0.453592,
         Year = as.numeric(Year),
         place = if_else(!is.na(place), as.numeric(place), as.double(NA)),
         type_decode = "Giant Pumpkin") %>% 
  arrange(desc(place)) %>% 
  #sample_n(nrow(.)) %>%
  rowid_to_column()

# Maximum Pumpkins Weight per year and GPC site each year
pumpkins_highest_persite <- pumpkins_clean %>% 
  group_by(gpc_site, Year) %>% 
  summarise(max_insite = max(weight_kg), na.rm = TRUE) %>% 
  filter(!is.na(max_insite))

# Average Pumpkins Weight per GPC site from 2013 to 2021
pumpkins_average_persite <- pumpkins_clean %>%
  group_by(gpc_site) %>% 
  summarise(mean_insite = mean(weight_kg, na.rm = TRUE))

# Number of particpations, keep only GPC sites with at least 6 participation over the last 9 years
nb_participation <- pumpkins_highest_persite %>% 
  arrange(gpc_site, Year) %>% 
  summarise(n_year = n()) %>% 
  filter(n_year >= 6)

# Combine all df  
pumpkins_highest_persite <-  pumpkins_highest_persite %>% 
  inner_join(nb_participation, by =  "gpc_site") %>% 
  inner_join(pumpkins_average_persite, by =  "gpc_site") %>% 
  arrange(desc(mean_insite)) %>% 
  filter(gpc_site != "Central Market Giant Pumpkin Weigh-off")

# Label each GPC site on Y axis
labs <- pumpkins_highest_persite %>% 
  distinct(gpc_site, mean_insite) %>% 
  rowid_to_column()

pumpkins_highest_persite <- pumpkins_highest_persite %>% 
  inner_join(labs, by = "gpc_site")

# Add some meaningful text
pumpkins.top <- pumpkins_highest_persite %>%
  ungroup() %>% 
  top_n(., 1, max_insite) %>%
  mutate(weight_kg = round(max_insite, 2),
         rowid = fct_reorder(as.character(rowid), mean_insite.x, max),
         label = glue::glue("<b><span style='font-size:12pt'><span style='color:#363636;'>Pumpkin from the Guinness Book of Records<br>in Italy: weighs {weight_kg} kg</span></span></b> <br><span style='font-size:9pt'>it was presented at the Festa dello Zuccone in the province of Pisa.</span>"))

df_labs <- 
  tibble(
    gpc = fct_reorder(labs$gpc_site, labs$mean_insite, max),
    rowid =  fct_reorder(as.character(labs$rowid), labs$mean_insite, max),
    country = case_when(gpc == "Europameisterschaft im Kurbiswiegen" ~ "Germany",
                        gpc == "Dutch Giant Vegetable Championship" ~ "The Netherlands",
                        gpc == "Early Weigh-off Ludwigsburg" ~ "Germany",
                        gpc == "Gartenbauschule Langenlois" ~ "Austria",
                        gpc == "Campionato dello Zuccone" ~ "Italy",
                        gpc == "XXL-Kuerbis-Wiegemeisterschaft auf dem Krewelshof" ~ "Germany",
                        gpc == "Le Potager Extraordinaire" ~ "France",
                        gpc == "Festa della zucca di sale" ~ "Italy",
                        gpc == "Schweizer Meisterschaft im Kurbiswagen" ~ "	Switzerland",
                        gpc == "Tharinger Meisterschaft" ~ "Germany",
                        gpc == "Concurso Calabazas Gigantes de Valtierra" ~ "Spain",
                        gpc == "Slovenian Weigh-off" ~ "Slovenia",
                        gpc == "Austrian Weigh-off" ~ "Austria",
                        gpc == "Royal Victoria Country Park" ~ "United Kingdom",
                        gpc == "Wiegemeisterschaft Berlin/Brandenburg" ~ "Germany",
                        gpc == "Pesa la Zucca! Foiano" ~ "Italy",
                        gpc == "Saechsische Meisterschaft im Kurbiswiegen" ~ "Germany",
                        gpc == "Finnish Weigh-off" ~ "Finland")
  )

# Country flags
flags <- df_labs %>% 
  distinct(country) %>% 
  pull(country) 

flags_code <- countrycode(flags, 'country.name', 'iso2c') %>% 
  as_tibble() %>% 
  rename(flag = value) %>% 
  mutate(flag = str_to_lower(flag))

as_tibble(flags) %>% 
  bind_cols(flags_code) -> names

df_labs <- df_labs %>% 
  left_join(names, by = c("country" = "value"))

# Create heatmap 
ggplot(data= pumpkins_highest_persite, aes(Year, fct_reorder(as.character(rowid), mean_insite.x, max))) + 
  # heatmap
  geom_tile(aes(fill= max_insite)) +
  # fill color, orange
  scale_fill_gradient(low="white", high="#f8cd24") +
  # add flag of each country where festival happen
  geom_flag(
    data = df_labs, 
    aes(x = 2012, y = rowid, country = flag), size = 10
  ) +
  # Customize axis text on Y axis
  geom_text(
    data = df_labs, 
    aes(x = 2021.8, y = rowid, label = gpc, 
        color = gpc, color = after_scale(darken(sequential_hcl(n = 18, h = c(0, 90), c = c(80, NA, 30), l = c(30, 90), power = c(0.2, 2), register = ),.2))), 
    hjust = 0, size = 4.9, family = "Merienda", fontface = "italic", lineheight = 1.50
  ) +
  # Add annotations and curves
  geom_richtext(
    data = pumpkins.top,
    aes(x=Year-1, y=11.5, label = label, hjust = 0.5, family = "Bitter"), 
    color = "#b8321a",
    fill = NA, 
    label.color = NA
  ) +
  geom_curve(
    data = pumpkins.top,
    aes(
      x = Year-1.6, xend = Year,
      y = 12.3, yend = 14
    ),
    color = "black", 
    size = 1.5,
    curvature = -.35
  ) +
  geom_richtext(
    data = pumpkins.top,
    aes(x=2016, y=17, label = "<span style='font-size:10pt'><span style='color:#363636;'>Previous record was hold by **Mathias Willemijns** (Belgium)<br>who made grown a giant pumpkin of 1,190.49 kg</span></span>", 
        hjust = 1, family = "Bitter"), 
    color = "#b8321a",
    fill = NA, 
    label.color = NA
  ) +
  geom_curve(
    data = pumpkins.top,
    aes(
      x = 2014.5, xend = 2016.15,
      y = 16.5, yend = 18
    ),
    color = "black",
    size = 1.5,
    curvature = .75
  ) +
  geom_richtext(
    data = pumpkins.top,
    aes(x=2021.8, y=19, label = "<span style='font-size:12pt'><span style='color:#b8321a;'><b>Europe GPC Festivals</b><br>ranked by Pumpkins avg weights presented from 2013 to 2021</span></span>", 
        hjust = 0, family = "Bitter"), 
    color = "#b8321a",
    fill = NA,
    label.color = NA
  ) +
  # Customize axis scales
  scale_x_continuous(limits =c(NA, 2026.5), breaks = seq(2013, 2021, by = 1), position = "top") +
  scale_color_discrete(guide = "none") +
  # Guide for colorbar
  guides(fill = guide_colorbar(barheight = unit(2.6, units = "mm"),  
                               barwidth = unit(160, units = "mm"),
                               direction = "horizontal",
                               title = "Maximum Pumpkins Weight (kg) presented per GPC festival over each year",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "top",
                               title.hjust = 0.5)) +
  # black theme
  theme_void() +
  coord_cartesian(clip = "off", expand = FALSE) +
  # add title/subtitles and caption
  labs(title = "<b style='font-size:36pt'>Where to go in Europe to see Giant Pumpkin Festivals ? </b><br>",
       subtitle = "<b style='font-size:18pt'><span style='color:#e68d00 font-size:25pt'>The Great Pumpkin Commonwealth's (GPC)</span> mission cultivates the hobby of growing giant pumpkins throughout the world !<br>They organize yearly festivals across Europe, find the <span style='color:#e68d00 font-size:25pt'>most amazing to visit near you!</span></b><br><br><br>",
       caption = "Visualization by Guillaume Abgrall • Data from BigPumpkins.com") +
  # Customize theme further
  theme(
    plot.title = element_markdown(family = "Bitter"),
    plot.subtitle = element_markdown(family = "Bitter"),
    plot.caption = element_text(family = "Bitter", size = "8", hjust=0.98),
    axis.text.x = element_text(family = "Poppins", size = "15pt", vjust=1.5, face = "italic"),
    axis.text.y = element_blank(),
    plot.margin = margin(10, 25, 10, 25),
    legend.margin = margin(10, 200, 25, -150),
    legend.position = "bottom",
    legend.title = element_text(family = "Bitter")
  )  

# Save plot
path <- here::here("pumpkins_heatmap")
ggsave(glue::glue("{path}.pdf"), width = 16.5, height = 12, device = cairo_pdf)
pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"), 
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 300)
