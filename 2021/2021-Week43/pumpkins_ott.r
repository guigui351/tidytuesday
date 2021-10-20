
# Load packages
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(patchwork)
library(png)
library(ggpubr)
library(ggtext)
library(ggimage)
library(magick)

# add fonts
font_add_google("Poppins", "Poppins")
font_add_google("Permanent Marker", "Permanent Marker")
showtext_auto()

# import tidytuesday data
tuesdata <- tidytuesdayR::tt_load(2021, week = 43)
pumpkins <- tuesdata$pumpkins

# clean data 
pumpkins_clean <- pumpkins %>% 
  separate(id, c("Year", "Type"), remove = FALSE) %>% 
  mutate(type_decode = case_when(Type == "F" ~ "Field Pumpkin",
                                 Type == "P" ~ "Giant Pumpkin",
                                 Type == "S" ~ "Giant Squash",
                                 Type == "W" ~ "Giant Watermelon",
                                 Type == "L" ~ "Long Gourd",
                                 Type == "T" ~ "Tomato"),
         type_decode = factor(type_decode, 
                              levels = c("Field Pumpkin", 
                                         "Giant Watermelon", 
                                         "Giant Squash", 
                                         "Giant Pumpkin", 
                                         "Long Gourd",
                                         "Tomato"),
                              labels = c("Field Pumpkin<span style='font-size:8pt'>  (only top 100 ranking Pumpkins are shown for each category)</span>", 
                                         "Giant Watermelon", 
                                         "Giant Squash", 
                                         "Giant Pumpkin", 
                                         "Long Gourd",
                                         "Tomato"))) %>% 
  mutate_at(vars("place", "weight_lbs", "Year", "pct_chart", "est_weight"), ~if_else(!is.na(.), as.double(.), NA_real_)) %>% 
  filter(!is.na(place) & !is.na(weight_lbs) & !is.na(est_weight) & est_weight !=0) %>% 
  filter(place < 100) %>% 
  filter(!(type_decode == "Field Pumpkin" & est_weight > 800))

# Create plot object
plot <- ggplot(pumpkins_clean, 
               aes(x=est_weight, y=weight_lbs)) + 
  geom_abline(intercept=0, 
              slope=1, 
              lty = 2, 
              color = "gray88", 
              size = 1.5, 
              guide = F
              ) +
  geom_smooth(method="loess", 
              span = 0.8, 
              se = TRUE, 
              fill="#ffb171", 
              color = "#b8321a") +
  geom_point(aes(size = weight_lbs, 
                 color = as.factor(Year)), 
             alpha = 0.4) +
  facet_wrap(vars(type_decode), 
             ncol=1, 
             nrow=3, 
             scales = "free") +
  scale_x_continuous(n.breaks = 7) +
  scale_color_viridis_d(option = "inferno",
                        guide = guide_legend(override.aes = list(size = 3,
                                                                 alpha = 1))) +
  coord_cartesian(xlim = c(0,NA), 
                  ylim = c(0,NA)) +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black",
                                   color = "black"),
    strip.background = element_rect(fill = "black",
                                    color = "transparent"),
    strip.switch.pad.wrap = unit(2.5, "lines"),
    strip.text = element_markdown(hjust=0.05, 
                              color = "white", 
                              family = "Poppins", 
                              face = "italic", 
                              size = 16),
    panel.grid.major.x = element_line(size = .1, color="#b5b5b5"),
    panel.grid.major.y = element_line(size = .1, color="#b5b5b5"),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11, 
                               family = "Poppins", 
                               face = "italic"),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    axis.ticks.length = unit(0, "lines"),
    axis.text = element_text(color = "#b8321a", face = "bold.italic", family = "Poppins"),
    axis.title = element_text(color = "white", family = "Poppins"),
    legend.position = "none", 
    plot.title.position = 'plot',
    plot.margin = margin(10,5,10,5)
  ) +
  labs(x='Final Pumpkins Weight (pounds) ', y='Estimated Weight of Pumpkin (pounds) using OTT method')


# Fill background pumpkins images with black color and save as png
image <- image_read(here::here("OTTCircumference.jpg")) 
image <- image_fill(image, "black", fuzz = 45) %>% 
  image_scale("200") 
image_write(image, path = "OTTCircumference_bk.png", format = "png") 

image <- image_read(here::here("OTTEnd-to-End.jpg")) 
image <- image_fill(image, "black", fuzz = 45) %>% 
  image_scale("200") 
image_write(image, path = "OTTEnd-to-End_bk.png", format = "png") 

image <- image_read(here::here("OTTPerpendicular.jpg")) 
image <- image_fill(image, "black", fuzz = 45) %>% 
  image_scale("200") 
image_write(image, path = "OTTPerpendicular_bk.png", format = "png") 

# Load pumpkins images 
ott_circ <- here::here("OTTCircumference_bk.png")
ott_endtoend <- here::here("OTTEnd-to-End_bk.png")
ott_perp <- here::here("OTTPerpendicular_bk.png")

# Convert to ggplot object and add meaningful text
# Circumference pumpkins size
gg_ott_circ <- ggplot()+
  geom_image(aes(x = 0, y = 0, image = ott_circ), size = 0.5) +
  scale_x_continuous(limits = c(-28,28)) +
  scale_y_continuous(limits = c(-20,20)) +
  coord_fixed(ratio = 1) +
  geom_richtext(aes(x=0, 
                    y=10, 
                    label = "<span style='color:#f7ca18 font-size:18pt'>**Circumference**</span>    – Measure the circumference<br>of the fruit parallel<br>to the ground at stem height.",
                    hjust = 0.5, 
                    vjust = 0.2,
                    family = "Poppins"), 
                color = "#b8321a",
                fill = NA, 
                label.color = NA) +
  geom_curve(
    aes(
      x = -2, xend = 0.5,
      y = 9, yend = 6,
      color = "#b8321a"
    ),
    curvature = -.35
  ) +
  guides(colour = "none") +
  theme_void()+
  theme(plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black"))

# End to End pumpkins size
gg_ott_endtoend <- ggplot()+
  geom_image(aes(x = 0, y = 0, image = ott_endtoend), size = 0.5) +
  scale_x_continuous(limits = c(-28,30)) +
  scale_y_continuous(limits = c(-20,20)) +
  coord_fixed(ratio = 1) +
  theme_void()+
  theme(plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black")) +
  geom_richtext(aes(x=8, 
                    y=-20, 
                    label = "<span style='color:#f7ca18 font-size:18pt'>**End to end**</span>    - Place your tape measure<br>on the groundat the stem end,<br> and at the blossom end.",
                    hjust = 0.5, 
                    vjust = 0.35,
                    family = "Poppins"), 
                color = "#b8321a",
                fill = NA, 
                label.color = NA) +
  geom_curve(
    aes(
      x = 11, xend = 2,
      y = -13.5, yend = -6.5,
      color = "#b8321a"
    ),
    curvature = .35
  ) +
  guides(colour = "none") +
  theme_void()+
  theme(plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black"))

# Side to Side pumpkins size
gg_ott_perp <- ggplot()+
  geom_image(aes(x = 0, y = 0, image = ott_perp), size = 0.5) +
  scale_x_continuous(limits = c(-20,35)) +
  scale_y_continuous(limits = c(-20,20)) +
  coord_fixed(ratio = 1) +
  geom_richtext(aes(x=8, 
                    y=13.5, 
                    label = "<span style='color:#f7ca18 font-size:18pt'>**Side to Side**</span>    – Placing the tape perpendicular<br>to the plant stem at the widest point<br>of the pumpkin.",
                    hjust = 0.5, 
                    vjust = 0.3,
                    family = "Poppins"), 
                color = "#b8321a",
                fill = NA, 
                label.color = NA) +
  geom_curve(
    aes(
      x = 8, xend = 2.5,
      y = 10.5, yend = 6,
      color = "#b8321a"
    ),
    curvature = -.30
  ) +
  guides(colour = "none") +
  theme_void()+
  theme(plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black"))

# Arrange plot objects with patchwork package
gg <- (gg_ott_circ + gg_ott_endtoend + gg_ott_perp + plot_layout(widths = c(1, 1, 1))) / plot + 
  plot_layout(heights = c(0.28,0.72)) +
  plot_annotation(title = "<b style='font-size:28pt'>OTT Giant Pumpkin Weight Estimation is a good predictor of final Pumpkins's Weight </b><br>",
                  subtitle = "<b style='font-size:13pt'>Pumpkin weight estimation is as easy as taking <span style='color:#f7ca18 font-size:18pt'>**3 measurements**</span> and entering it into an OTT calculator:</b><br>",
                  caption = "<b style='font-size:8pt'>Visualization by Guillaume Abgrall • Data from BigPumpkins.com</b>",
                  theme = theme(plot.title = element_markdown(color = "#b8321a", face = "bold", family = "Permanent Marker", hjust = 0.5),
                                plot.subtitle = element_markdown(color = "#b8321a",  size = "13pt", family = "Permanent Marker", hjust = 0.5),
                                plot.caption =  element_markdown(color = "#b8321a", face = "italic", family = "Poppins", hjust = 0.97),
                                plot.background = element_rect(fill = "black", color = "black"),
                                plot.margin = margin(5,5,5,5)))

# Save plot
path <- here::here("pumpkins_plot")
ggsave(glue::glue("{path}.pdf"), width = 20, height = 12, device = cairo_pdf)
pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"), 
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 300)