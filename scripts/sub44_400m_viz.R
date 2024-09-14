## Load packages  ------------------------------------------------------

library(rvest)  # scraping 
library(dplyr)  # cleaning
library(janitor)  # cleaning
library(stringr)  # cleaning
library(ggplot2)  # plotting
library(forcats)  # factors
library(ggtext)  # plotting
library(showtext)  # plotting with fonts


#### Scraping ---------------------------------------------------------------

## 1. Function to scrape data from a single page
scrape_page <- function(page_number) {
  # Construct the URL for the current page
  url <- paste0("https://worldathletics.org/records/all-time-toplists/sprints/400-metres/all/men/senior?regionType=world&timing=electronic&page=", page_number, "&bestResultsOnly=false&firstDay=1899-12-31&lastDay=2024-09-13&maxResultsByCountry=all&eventId=10229631&ageCategory=senior")
  
  # Read the webpage and extract the table
  data <- read_html(url) %>%
    html_elements("table") %>%
    html_table()
  
  # Return the data
  return(data[[1]])
}


## 2. Loop to get data from multiple pages
# Initialize an empty list to store the data
all_data <- list()

# Define the max number of pages to scrape
num_pages <- 10 

# Loop through each page and scrape data
for (i in 1:num_pages) {
  page_data <- scrape_page(i)
  all_data[[i]] <- page_data
}

## 3. Combine all pages into one data frame
combined_data <- bind_rows(all_data)


#### Cleaning ---------------------------------------------------------------

# Get rid of unneeded columns, clean the column names, and update names to title case
clean_data <- combined_data %>% 
  select(-WIND, -...8) %>% 
  clean_names() %>% 
  mutate(competitor = str_to_title(competitor)) 

# Filter for Sub 44 sec performances and tally
plot_data <- clean_data %>% 
  filter(mark < 44) %>% 
  group_by(competitor) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(mj_id = ifelse(competitor == "Michael Johnson", "mj", "not_mj"))
  

#### Plotting  ---------------------------------------------------------------

# Add fonts
font_add_google("Roboto")
showtext_auto()

sub44_400m_plot <- plot_data %>% 
  ggplot(mapping = aes(x = n, y = fct_reorder(competitor, n), fill = mj_id, label = n)) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto"),
    plot.margin = margin(10, 20, 10, 20), 
    plot.background = element_rect(fill = "white"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_markdown(size = 16, face = "bold", margin = margin(t = 5, b = 10)),
    plot.subtitle = element_markdown(size = 13, margin = margin(b = 15)),
    plot.caption = element_text(size = 9, face = "italic", hjust = 1, margin = margin(t = 10)),
    axis.text.y = element_text(size = 11, hjust = 1, margin = margin(r = -25))
  ) +
  labs(
    title = "<span style = 'color:#3498DB;'>Michael Johnson</span> has more than twice as many sub 44 second 400m performances as any other athlete",
    subtitle = "Number of sub 44 second 400m performances",
    caption = c("Data: World Athletics\nViz: Tim Fulton, PhD")
  ) +
  geom_bar(
    stat = "identity",
    width = 0.8
  ) +
  geom_text(
    hjust = 1.5,
    color = "white"
  ) +
  scale_fill_manual(values = c("#3498DB", "gray60")) +
  guides(fill = "none")
  

# Save figure
ggsave(
  "plots/sub44_400m_plot.png", 
  plot = sub44_400m_plot,
  scale = 1, 
  width = 11, 
  height = 8, 
  dpi = 600,
  units = "in"
)
  
  
  
