library(afrihealthsites)
library(afrilearndata)
library(tidyverse)
library(sf)
library(showtext)
library(janitor)
library(wbstats)
library(ggrepel)
library(ggtext)

# afrilearndata
remotes::install_github("afrimapr/afrilearndata")
# afrihealthsites`
remotes::install_github("afrimapr/afrihealthsites")

# Add google font
font_add_google("Roboto Condensed", "roboto condensed")
font_add_google("Bitter", "Bitter")
font_add_google("Playfair Display", "Playfair Display")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# Import africa countries
data(africountries) 

# Distinct list of all countries
country <- africountries %>% 
  pull(name)

## Query WHO database to calculate number of health facilities in each country
list_health <- c()
for (i in 1:length(country)){
  list_health[[i]] <- afrihealthsites(country, datasource='who', plot=FALSE)
}

n_health_facilities <- bind_rows(list_health) %>% 
  group_by(Country, iso3c) %>% 
  mutate(n = n()) %>% 
  select(iso3c, Country, n) %>% 
  slice(n())

saveRDS(n_health_facilities, "n_health_facilities")
n_health_facilities <- readRDS("n_health_facilities")

## Query Open Street Map database to calculate number of health facilities in each country
list_healthsites <- c()
for (i in 1:length(country)){
  list_healthsites[[i]] <- afrihealthsites(country, datasource='healthsites', plot=FALSE)
}

n_healthsites_facilities <- bind_rows(list_healthsites)  %>% 
  group_by(iso3c, country) %>% 
  mutate(n_osm = n()) %>% 
  select(iso3c, country, n_osm) %>% 
  slice(n()) %>% 
  as.data.frame()

saveRDS(n_healthsites_facilities_, "n_healthsites_facilities")
n_healthsites_facilities_ <- readRDS("n_healthsites_facilities")

# Combine both
all <- n_healthsites_facilities_ %>% 
  left_join(n_health_facilities, by = "iso3c")

# Gini Index (income inequality) from World Databank
poverty_data <- wb_data("SI.POV.GINI")

gini_data_clean <- poverty_data %>% 
  clean_names() %>% 
  filter(!is.na(si_pov_gini)) %>% 
  group_by(iso3c) %>% 
  arrange(desc(date)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rename(date_gini = date)

# GDP per capita from World Databank
gdp_data <- wb_data("NY.GDP.PCAP.CD")
gdp_data_clean <- gdp_data %>% 
  clean_names() %>% 
  filter(!is.na(ny_gdp_pcap_cd)) %>% 
  group_by(iso3c) %>% 
  arrange(desc(date)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rename(date_gdp = date)

# Life expectancy from World Databank
lifeexp_data <- wb_data("SP.DYN.LE00.IN")
lifeexp_data_clean <- lifeexp_data %>% 
  clean_names() %>% 
  filter(!is.na(sp_dyn_le00_in)) %>% 
  group_by(iso3c) %>% 
  arrange(desc(date)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rename(date_lifexp = date)

# Combine all dataframes together
afric_data <- africountries %>% 
  left_join(gdp_data_clean %>% select(iso3c, ny_gdp_pcap_cd, date_gdp), by = c("iso_a3" = "iso3c")) %>% 
  left_join(lifeexp_data_clean %>% select(iso3c, sp_dyn_le00_in, date_lifexp), by = c("iso_a3" = "iso3c")) %>% 
  left_join(all %>% select(iso3c, n_osm, n, geometry.x), by = c("iso_a3" = "iso3c")) %>% 
  mutate(health_fac_byhab = (n_osm / pop_est) *10000,
         health_fac_byhab2 = (n / pop_est) *10000,
         diff_osm = (n - n_osm) / n_osm *100)

# create 3 buckets for GDP per capita
quantiles_gdp <- afric_data %>%
  filter(!is.na(ny_gdp_pcap_cd)) %>% 
  pull(ny_gdp_pcap_cd) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for life expectancy
quantiles_lifeexp <- afric_data %>%
  filter(!is.na(sp_dyn_le00_in)) %>% 
  pull(sp_dyn_le00_in) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# Create bivariate color palette
bivariate_color_scale <- tibble(
  "3 - 3" = "#e86c00", # high gini index, high life expectancy
  "2 - 3" = "#dca677", # medium  gini index,  high life expectancy
  "1 - 3" = "#dddeba", # low gini index, high life expectancy
  "3 - 2" = "#be5300", # high gini index, medium life expectancy
  "2 - 2" = "#b47f3c", # medium gini index, medium life expectancy
  "1 - 2" = "#ada16a", # low gini index, medium life expectancy
  "3 - 1" = "#943800", # high gini index, low life expectancy
  "2 - 1" = "#8c5700", # middle gini index, low life expectancy
  "1 - 1" = "#876e00"  # low gini index, low life expectancy
) %>%
  gather("group", "fill_col")


# cut into groups defined above and join fill
afric_data %<>%
  mutate(
    gdp_quantiles = cut(
      ny_gdp_pcap_cd,
      breaks = quantiles_gdp,
      include.lowest = TRUE
    ),
    lifeexp_quantiles = cut(
      sp_dyn_le00_in,
      breaks = quantiles_lifeexp,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      4-as.numeric(gdp_quantiles), "-",
      as.numeric(lifeexp_quantiles)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each country knows its hex value based on the his GDP per capita and life expectancy value
  left_join(bivariate_color_scale, by = "group") %>% 
  mutate(fill_col = if_else(is.na(fill_col), "grey88", fill_col))

# Create a nudge_x/nudge_y value for countries overlapping each other on the west coast
afric_data_ggrepel <-
  st_as_sf(afric_data) %>%
  mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2)
  ) %>%
  as_tibble() %>%
  st_as_sf()

afric_data_ggrepel$nudge_x <- 0
afric_data_ggrepel$nudge_y <- 0

x_range <- abs(Reduce("-", range(afric_data_ggrepel$COORDS_X)))
y_range <- abs(Reduce("-", range(afric_data_ggrepel$COORDS_Y)))


is <- afric_data_ggrepel$name %in% c(
  "Cote d'Ivoire", "Liberia", "Togo", "Benin", "Ghana"
)
afric_data_ggrepel$nudge_x[is] <- -.1 * 0.2 * x_range
afric_data_ggrepel$nudge_y[is] <- -.8 * 0.15 * y_range

ix <- afric_data_ggrepel$name %in% c(
  "Senegal", "Gambia", "Guinea-Bissau", "Guinea", "Sierra Leone"
)
afric_data_ggrepel$nudge_x[ix] <- -3.5 * 0.2 * x_range
afric_data_ggrepel$nudge_y[ix] <- -.1 * 0.15 * y_range

# Get centroid data of each country to plot number of health facilities
library(CoordinateCleaner)
data("countryref")
country_df <- countryref %>% 
  filter(is.na(source) & type == "country") %>% 
  group_by(name) %>% 
  slice(1) %>%         
  ungroup() %>% 
  mutate(code = as.character(iso3)) %>%
  inner_join(afric_data, by = c("code"="iso_a3")) %>% 
  select(-geometry, -geometry.x)

centroid_country <- st_as_sf(country_df, coords = c("centroid.lon", "centroid.lat"), 
                             crs = 4326, agr = "constant")

theme_map <- function(base_size = 12, base_family = "roboto condensed", ...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Playfair Display",
                          color = "grey10"),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_markdown(size = 9,
                                      color = "grey20"),
      axis.title.y = element_markdown(size = 9, 
                                      color = "grey20"),
      # add a subtle grid
      panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "grey92",
                                     color = NA),
      panel.background = element_rect(fill = "grey92",
                                      color = NA),
      legend.background = element_rect(fill = "grey92",
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(1.5, 1, 1, .5), "cm"),
      panel.border = element_blank(),
      #legend
      legend.position =  c(.9, .88),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0, 
                                 color = "grey20"),
      legend.title.align = 0.5,
      legend.text.align = 0.1,
      plot.title = element_markdown(size = 24, hjust = 0.5,
                                    color = "grey20"),
      plot.subtitle = element_markdown(size = 16, hjust = 0.5, 
                                       color = "grey20",
                                       margin = margin(b = -0.1,
                                                       t = -0.1,
                                                       l = 2,
                                                       unit = "cm"),
                                       debug = F),
      # captions
      plot.caption = element_markdown(size = 9,
                                      family = "roboto condensed",
                                      hjust = .98,
                                      margin = margin(t = 0.2,
                                                      b = 0,
                                                      unit = "cm"),
                                      color = "grey30"),
      ...
    )
}


theme_set(theme_map(base_family = "Playfair Display"))

map <- ggplot(
  data = afric_data) +
  # color countries according to their GDP per capita / Life expectancy combination
  geom_sf(
    aes(
      fill = fill_col
    ),
    # use thin white stroke for countries
    color = "white",
    size = 0.1
  ) +
  scale_fill_identity() +
  # use thicker white stroke for cantons
  geom_sf(
    data = afrihighway,
    color = "red",
    size = 0.2,
    alpha = 0.4,
  ) +
  # Point of number of health facilities per 10000habs
  geom_sf(data = centroid_country,
          aes(size = health_fac_byhab),
          color = "black", 
          alpha = 0.7
  ) + 
  # Add country name as text
  geom_text_repel(data = afric_data_ggrepel, 
                  mapping = aes(
                    x = COORDS_X,
                    y = COORDS_Y,
                    label = name,
                  ),
                  nudge_x = afric_data_ggrepel$nudge_x,
                  nudge_y = afric_data_ggrepel$nudge_y,
                  size = 3.1,
                  family = "roboto condensed",
                  min.segment.length = 1,
                  point.padding = NA,
                  segment.color = "grey50"
  ) +
  # Customize size legend
  scale_size(breaks = c(10,20,40,60,80,100,120), 
             labels = c("<10", "10-20", "20-40", "40-60", "60-80", "80-100", ">100"),
             range = c(0.1,10),
             guide = guide_legend(
               title = "Number of health facilities \n (per 10k habitants)",
               title.theme = element_text(face = "bold", size = 9, margin = margin(b = 0.2,
                                                                                   unit = "cm")),
               label.theme = element_text(face = "italic", size = 8, margin = margin(l = 1.5,
                                                                                       unit = "cm")),
               direction = "vertical",
               title.position = "top",
               label.position = "left",
               label.hjust = 0.5,
               label.vjust = 0.5,
               keywidth = unit(1, "cm"),
               keyheight = unit(0.8, "cm"),
               nrow = 7, ncol = 1
             )) +
  coord_sf(crs = st_crs(afric_data_ggrepel), datum = NA) +
  theme_map() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  xlim(min(afric_data_ggrepel$COORDS_X) * 1.6, max(afric_data_ggrepel$COORDS_X) * 1.15)


# separate the groups
bivariate_color_scale_n <- bivariate_color_scale %>% 
  separate(group, into = c("gdp", "lifeexp"), sep = " - ") %>%
  mutate(gdp = as.integer(gdp),
         lifeexp = abs(4-as.integer(lifeexp)))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale_n,
    mapping = aes(
      x = gdp,
      y = lifeexp,
      fill = fill_col)
  ) +
  scale_fill_identity() +
  labs(x = "<span style = 'font-size:7.3pt'>**Lower  GDP  per  capita** --> </span>",
       y = "<span style = 'font-size:7.3pt'>**Lower  Life  Expectancy** --> </span>") +
  theme_void()+
  theme(
    legend.position="none",
    axis.title.x = element_textbox_simple(size = 8, 
                                          width = unit(3.8, "cm"),
                                          halign = 0.3,
                                          padding = margin(4, 4, 4, 4),
                                          margin = margin(4, 0, 0, 0),
                                          linetype = 1,
                                          r = grid::unit(8, "pt"),
                                          fill = "#dca677",
                                          color = "black", 
                                          family = "Bitter"
    ),
    axis.title.y = element_textbox_simple(size = 8, 
                                          width = unit(3.8, "cm"),
                                          hjust = .3,
                                          halign = 0.3,
                                          valign = 0.8,
                                          linetype = 1,
                                          r = grid::unit(8, "pt"),
                                          orientation = "left-rotated",
                                          padding = margin(4, 4, 4, 4),
                                          margin = margin(4, 0, 2, 0),
                                          fill = "#ada16a",
                                          color = "black", 
                                          family = "Bitter"
    )
  ) +
  # quadratic tiles
  coord_fixed()



# Organize plot elements
library(patchwork)
final <- map + inset_element(legend,
                    right = 0.4,
                    bottom = 0.1, 
                    left = 0.01,
                    top = 0.35) +
  plot_annotation(
    caption = "Visualization: Guillaume Abgrall • Data: {afrilearndata} & World Bank Databank",
    title = "Africa: <span style='color::#e86c00'>**Wealth**</span> versus <span style='color:#876e00'>**Health**</span>",
    subtitle = "<br>Increased wealth not necessarily lead to improved health although<br>higher incomes facilitate access to goods & services which in turn contributes to <br> <span style='color:#2D5F47'>**improved health and longevity**</span>")
   
# Save data
ragg::agg_png(here::here(paste0("afrihealthsites", ".png")), res = 320, width = 14, height = 11, units = "in")
final
dev.off()