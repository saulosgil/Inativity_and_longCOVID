# Packages  ------------------------------------------------------------------------------------
library(tidyverse)
library(viridis)

# Making dataframe ____-------------------------------------------------------------------------

data <- data.frame(sintomas = c("None",
                                "At least 1 symptom",
                                "1 to 4 symptoms",
                                "5 or more symtoms",
                                "Fatigue",
                                "Dyspnea",
                                "Severe muscle\nand joint pain",
                                "Insomnia",
                                "Post-Traumatic stress",
                                "Memory impairments",
                                "Anxiety",
                                "Taste loss",
                                "Smell loss",
                                "Depression"),
                   percentual = c(51, 68, 58, 71, 77, 69, 66, 66, 65, 65, 65, 65, 63, 62)
)

# Set a number of 'empty bar'

empty_bar <- 1

# Add lines to the initial dataset

to_add <- matrix(NA, empty_bar, ncol(data))
colnames(to_add) <- colnames(data)
data <- rbind(data, to_add)
data$id <- seq(1, nrow(data))

# prepare a data frame for base lines

base_data <- data %>%
  group_by(sintomas) %>%
  summarize(start=min(id), end=max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title=mean(c(start, end)))


# Get the name and the y position of each label

label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot

data |>
  ggplot(mapping = aes(x = as.factor(id),
                       y = percentual,
                       fill = percentual)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity") +
  labs(title = "Prevalence of physical inactivity according to the presence\n of post-acute sequelae of SARS-CoV-2",
       caption = "https://www.researchsquare.com/article/rs-1638885/v1") +
  ylim(-30,120) +
  theme_minimal() +
  theme(title = element_text(size = 15,
                             face = "bold"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 2.2)
  ) +
  coord_polar(start = 0) +
  scale_fill_gradientn(name = "percentage",
                       colours = viridis(n = 256, option = "D")) +
  geom_text(data  = label_data,
            aes(x = id,
                y = percentual + 10,
                label = sintomas,
                hjust = hjust,
                bold = TRUE),
            color = "black",
            fontface = "bold",
            size = 4,
            angle = label_data$angle,
            inherit.aes = FALSE) +
  geom_segment(data=base_data, aes(x = start,
                                   y = 0,
                                   xend = end,
                                   yend = 0),  # Add base line information
               colour = "black",
               alpha = 0.8,
               size = 0.6,
               inherit.aes = FALSE)


