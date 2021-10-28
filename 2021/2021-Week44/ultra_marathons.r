
# Load packages
library(tidyverse)
library(gggibbous)
library(ggridges)
library(showtext)
library(ggimage)
library(magick)
library(patchwork)
library(ggtext)

# Import tidytuesday data - Week 44
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

# Add goofle fonts for the plots
font_add_google("Roboto Condensed", "roboto condensed")
font_add_google("Bitter", "Bitter")
font_add_google("Playfair Display", "Playfair Display")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# Clean Ultra Marathons races
# Calculate average elevation_gain per event and country (can vary a little bit from year to year)
race_clean <- race %>% 
  mutate(year = format(date, format="%Y"),
         event = str_to_upper(event)) %>% # everything to upper case
  filter(distance > 100) %>% 
  group_by(event, country) %>% 
  summarise(avg_elevgain = mean(elevation_gain, na.rm = TRUE),
            avg_elevloss= mean(elevation_loss, na.rm = TRUE),
            avg_distance = mean(distance, na.rm = TRUE)) %>% 
  ungroup()


# Take only countries with at least 10 Ultra Marathons referenced in the data
race_clean_country <- race_clean %>% 
  group_by(country) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 10) %>% 
  mutate(country = fct_reorder(country, n)) %>% 
  arrange(country, n, desc(avg_elevgain)) %>% 
  mutate(country_num = as.numeric(fct_rev(country))) 

# Pull country order for display
country_order <- race_clean_country %>%
  group_by(country) %>% 
  filter(avg_elevgain == max(avg_elevgain)) %>%
  ungroup() %>% 
  arrange(n) %>%
  pull(country)

n <- race_clean_country %>%
  group_by(country) %>%
  filter(avg_elevgain == max(avg_elevgain)) %>%
  ungroup() %>% 
  arrange(n) %>%
  pull(n)

# Color countries
color_country <- c()
color_country[country_order] <- colorRampPalette(c("#fafaf4", "#00429d", "#2d5f47"))(8)

# Label country
label_country <- tibble(label = rev(country_order),
                        x = c(4000,4700,8350,5900,5970,4300,6080,6500),
                        y = c(9,25,13,32,36.2,44.5,39.7,42.3),
                        idy = seq(1:8),
                        n = rev(n),
                        nred = n /100)

# Axis ticks of 1st plot
axis_ticks <- tibble(x = -500,
                     xend = c(850, 1600, 2250, 3100),
                     y = seq(10,40,10),
                     yend = seq(10,40,10))

# Area plot (cumulative by country) of number of events per D+
area_plot <- ggplot(race_clean_country, 
            aes(x = avg_elevgain)) + 
  geom_area(aes(fill = country), 
            stat = "bin", bins = 10, alpha=0.7) +
  scale_fill_manual(values = color_country) +
  scale_color_manual(values = color_country) +
  scale_x_continuous(breaks = seq(0,12000,2500), 
                     labels = glue::glue("{seq(0,12000,2500)} m"),
                     limits = c(-500,12000)) +
  geom_segment(data = label_country, aes(x = 8000, xend = 12500, y = 32, yend = 32), color = "black", size = 0.5) +
  geom_segment(data = axis_ticks, aes(x= x, xend = xend, y =y, yend = yend), linetype = "13", color = "white") +
  geom_text(data = axis_ticks, aes(x = 700, y = y, label = glue::glue("{y} Ultra Marathons")), hjust = 1, nudge_y = 1.2, color = "white", family = "roboto condensed", face = "italic", size = 2.5) +
  geom_text(data = label_country,
            aes(x = x, y = y, label = label), hjust = 0.5, color = "grey90", size = 4, family = "Playfair Display") +
  guides(fill = "none") +
  labs(x = "Elevation gain (D+) in Ultra Marathons") +
  theme_light() +
  theme(plot.background = element_rect(fill = "grey20", color = NA),
        panel.border = element_rect(fill = NA, 
                                    colour = NA),
        panel.background = element_rect(fill = "grey20", 
                                        colour = NA),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_line(color = "grey88", size = 3),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color = "grey88", vjust=-1, size = 12, family = "roboto condensed"),
        axis.title.x = element_text(color = "grey88",  vjust=-2, size = 14, family = "Playfair Display"),
        plot.margin = margin(10,5,15,40))

# Customize legend 
legend <- ggplot(label_country,
                 aes(x = 1,y = idy)) +
  geom_moon(aes(ratio = 0.7, size = n), 
            stat = "identity",
            fill = colorspace::lighten(rev(color_country)), 
            color = rev(color_country),
            stroke = .3,
            right = FALSE) +
  geom_moon(aes(ratio = 0.3, size = n), 
            stat = "identity",
            fill = rev(color_country), 
            color = rev(color_country),
            stroke = .3) +
  geom_text(aes(label = n), hjust = 0.9, color = colorspace::darken(rev(color_country),0.8), family = "Bitter", fontface = "italic", size = 4) +
  geom_text(aes(label = label), color = colorspace::lighten("#c6c6a9",0.4), hjust = .5, nudge_y = 0.5, family = "Playfair Display", fontface = "bold", size = 5) +
  scale_size_continuous(range = c(9, 24)) +
  scale_y_continuous(limits = c(.4,9)) +
  guides(size = "none") +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey20", color = NA))

# density ridges by country of races per D+ 
density_ridges <- ggplot(race_clean_country, 
         aes(x = avg_elevgain, y = country_num, fill = country, color = country)) +
  geom_density_ridges(
    jittered_points = FALSE, quantile_lines = FALSE, scale = 1.1, alpha = 0.7,
    vline_size = 0.5, vline_color = "grey88",
    point_size = 0.4, point_alpha = 1,
    position = position_raincloud(adjust_vlines = FALSE)
  ) +
  scale_fill_cyclical(values = colorspace::lighten(color_country,0.2)) +
  scale_color_cyclical(values = color_country) +
  geom_boxplot(
    aes(y = country_num - .15), 
    width = .15, 
    outlier.shape = NA
  ) +
  geom_point(
    aes(y = country_num - .3, group = country, fill = after_scale(colorspace::darken(color, .2))), 
    shape = "|",
    size = 2.5,
    alpha = .33
  ) +
  scale_x_continuous(breaks = seq(0,12000, 2500),
                     labels = glue::glue("{seq(0,12000,2500)} m"),
                     limits = c(0,12000)) +
  labs(x = "Elevation gain (D+) in Ultra Marathons") +
  coord_cartesian(expand = TRUE, clip = "on") +
  theme_light() +
  theme(plot.background = element_rect(fill = "grey20", color = NA),
        panel.border = element_rect(fill = NA, 
                                    colour = NA),
        panel.background = element_rect(fill = "grey20", 
                                        colour = NA),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_line(color = "grey88", size = 3),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color = "grey88", vjust=-1, size = 12, family = "roboto condensed"),
        axis.title.x = element_text(color = "grey88",  vjust=-2, size = 14, family = "Playfair Display"),
        plot.margin = margin(10,5,15,5)
        )

# Import image and transform it with transparent background 
image <- image_read(here::here("running-man-color3.jpg")) 
image <- image_fill(image, "transparent", fuzz = 40) %>% 
  image_scale("400") 
image_write(image, path = "running-man-color3.png", format = "png")
image_runner <- here::here("running-man-color3.png")

# Create image plot
image_runner_p <- ggplot() +
  geom_image(aes(x = 0, y = 0, image = image_runner), size = 1) +
  scale_x_continuous(limits = c(-10,10)) +
  scale_y_continuous(limits = c(-10,10)) +
  coord_fixed(ratio = 1) +
  theme_void()+
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Assembling elements to create final plot
final <- 
  area_plot + inset_element(image_runner_p,0.001, 0.835, 0.9, 1, align_to = 'full') +
  legend + density_ridges + 
  plot_layout(widths = c(1,0.25,1)) + 
  plot_annotation(
    caption = "Visualization: Guillaume Abgrall • Data: Benjamin Nowak from International Trail Running Association (ITRA)",
    title = "Ultra Marathons : Where elevation gain (D+) make runs harder",
    subtitle = "<br><span style='font-size:12pt color:grey88'> The distance isn’t the only problem, as competitors are asked to wrestle with mountains where <span style='color:#f9de57 font-size:12pt'>elevation gain (D+)</span> make the runs even harder !</span><br>
                <span style='font-size:12pt color:grey88'> Since they’re events that allow you to experience the world in a unique way, I've chosen to show countries where you can find races with the best D+ <span style='font-size:9pt color:grey88'>*(at least 10 ultra marathons in each country)*</span></span><br><br>
                <span style='font-size:10pt color:grey88'> While <span style='font-size:14pt'><span style='color:#2D5F47'>**United Stated**</span></span>  has the most ultra marathon races referenced <span style='color:#2D5F47'>**(92)**</span>, <span style='font-size:14pt'><span style='color:#235CA9'>**France**</span></span>  is by far the first grantor of highest D+ in their Ultra Marathons !</span>",
    theme=theme(
      plot.title =  element_text(color = "grey88", size = 21, family = "Playfair Display", hjust = .5),
      plot.subtitle =  element_markdown(color = "grey88", size = 10, family = "Bitter", hjust = .5),
      plot.caption =  element_markdown(color = "grey88", size = 6, family = "Playfair Display", hjust = .985),
      plot.background = element_rect(fill = "grey20", color = NA),
      plot.margin = margin(10,25,20,25))) 

# Save data
ragg::agg_png(here::here(paste0("ultra_marathons", ".png")), res = 320, width = 16, height = 10, units = "in")
final
dev.off()


