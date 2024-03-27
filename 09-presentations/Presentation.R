#+ Presentation Analysis
#+ 27 Mar 2024

# Load packages
library(tidyverse)
library(haven)
library(patchwork)
library(readxl)

# Load data
df = read_excel('/Users/Colby/Desktop/SIS-750/09-presentations/Presentation/cleaned_data.xlsm')

# Change date format
df =
  df |>
  mutate(Date = as_date(Date))

# Analysis 1
Q = ggplot(df, aes(x = Date, y = Discharge)) +
  geom_line(color = 'dodgerblue2') + 
  geom_point(alpha = 1/4, color = 'dodgerblue2') +
  labs(
  x = 'Time', 
  y = 'Discharge (cfs)',
  title = 'Daily Potomac River Flow (2023)'
  ) +
  scale_y_continuous(
    limits = c(0, 35000),
    breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000),
    labels = c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000),
    expand = c(0, 0)) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_bw() +
  theme(
    legend.position = 'none',
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor.x = element_blank()
  )

Q

# Analysis 2
O = ggplot(df, aes(x = Date, y = DO_Mean)) +
  geom_line(color = 'forestgreen') + 
  geom_point(alpha = 1/4, color = 'forestgreen') +
  labs(
    x = 'Time', 
    y = 'DO (mg/L)',
    #title = 'Daily Potomac River Dissolved Oxygen (2023)'
  ) +
  scale_y_continuous(
    limits = c(0, 16),
  #  breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000),
  #  labels = c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000),
    expand = c(0, 0)) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_bw() +
  theme(
    legend.position = 'none',
    #plot.title = element_text(hjust = 0.5),
    panel.grid.minor.x = element_blank()
  )
 
O

N = ggplot(df, aes(x = Date, y = NO3_NO2_Mean)) +
  geom_line(color = 'firebrick2') + 
  geom_point(alpha = 1/4, color = 'firebrick2') +
  labs(
    x = 'Time', 
    y = 'NO3 + NO2 (mg/L)',
    #title = 'Daily Potomac River Dissolved Nitrogen (2023)'
  ) +
  scale_y_continuous(
    limits = c(0, 2.5),
  #  breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000),
  #  labels = c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000),
    expand = c(0, 0)) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_bw() +
  theme(
    legend.position = 'none',
    #plot.title = element_text(hjust = 0.5),
    panel.grid.minor.x = element_blank()
  )

N

patchy =
  O + N + plot_layout(guides = "collect") +
  plot_annotation(
    title = 'Daily Potomac River Dissolved Oxygen & Nitrogen (2023)'
  ) &
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )

patchy
