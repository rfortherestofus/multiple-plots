
# Load Packages -----------------------------------------------------------

library(tidyverse)

# Import Data -------------------------------------------------------------

median_income_data <- read_csv("oregon_median_income.csv")

# Make Single Plot --------------------------------------------------------

median_income_data_filtered <- median_income_data %>% 
  filter(geography %in% c("Oregon", "Baker")) %>% 
  mutate(geography = fct_inorder(geography)) 

ggplot(data = median_income_data_filtered,
       aes(x = median_income,
           y = geography,
           fill = geography)) +
  geom_col() +
  scale_fill_manual(values = c("#a8a8a8", "#265142")) +
  theme_void() +
  theme(axis.text.y = element_text(),
        legend.position = "none")

ggsave(plot = last_plot(),
       filename = "sample-median-income-plot.png",
       width = 4,
       height = 1,
       bg = "white")

# Make Function -----------------------------------------------------------

median_income_plot <- function(county) {
  
  median_income_data_filtered <- median_income_data %>% 
    filter(geography %in% c("Oregon", county)) %>% 
    mutate(geography = fct_inorder(geography)) 
  
  ggplot(data = median_income_data_filtered,
         aes(x = median_income,
             y = geography,
             fill = geography)) +
    geom_col() +
    scale_fill_manual(values = c("#a8a8a8", "#265142")) +
    theme_void() +
    theme(axis.text.y = element_text(),
          legend.position = "none")
  
  ggsave(plot = last_plot(),
         filename = str_glue("median-income-plot-{county}.png"),
         width = 4,
         height = 1,
         bg = "white")
  
}

median_income_plot(county = "Benton")


# Make Multiple Plots -----------------------------------------------------

walk(c("Baker", "Benton", "Clackamas"), median_income_plot)

oregon_counties <- median_income_data %>% 
  filter(geography != "Oregon") %>% 
  pull(geography)

oregon_counties

walk(oregon_counties, median_income_plot)
 