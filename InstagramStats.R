library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggExtra)
library(stringr)
library(purrr)

ig_stats <- read.csv("Instagram_cy2025.csv")

impressions_max <- 40000
engagements_max <- 1000

# Clean date format to yyyy-mm-dd-hh-mm
ig_stats <- ig_stats |>
  mutate(Date = mdy_hm(Date)) |>
  select(Date, Post.Type, Content.Type, Impressions, Reach, Engagements,
         Likes, Comments, Shares, Saves, Video.Views, Tags) |>
  # Convert all numeric columns, removing commas if present
  mutate(across(c(Impressions, Reach, Engagements, Likes, Comments, 
                  Shares, Saves, Video.Views), 
                ~as.numeric(gsub(",", "", as.character(.))))) |>
  # Filter out rows where EITHER impressions OR engagements exceed the max
  filter(Impressions < impressions_max | Engagements < engagements_max) |>
  filter(Post.Type == "Reel" | Post.Type == "Post")

ig_stats$Date <- as.Date(ig_stats$Date)

ig_stats <- ig_stats |>
  arrange(Date)

# Function to split tags while preserving "Student Centered, Innovation Driven"
split_tags <- function(tag_string) {
  if (is.na(tag_string) || tag_string == "") {
    return(character(0))
  }
  
  # Temporarily replaces the special tag with a placeholder
  temp_string <- gsub("Student Centered, Innovation Driven", 
                      "SCID_placeholder", 
                      tag_string, 
                      fixed = TRUE)
  
  # Split by comma and trim whitespace
  tags <- str_split(temp_string, ",")[[1]] %>%
    str_trim()
  
  # Replace the placeholder back with the original tag
  tags <- gsub("SCID_placeholder", 
               "Student Centered, Innovation Driven", 
               tags, 
               fixed = TRUE)
  
  return(tags)
}

# Split tags into separate columns
ig_stats <- ig_stats |>
  mutate(
    # Split the tags into a list column
    tag_list = map(Tags, split_tags),
    # Get the maximum number of tags across all rows
    max_tags = max(map_int(tag_list, length))
  ) |>
  # Create separate columns for each tag position
  mutate(
    tag_columns = map(tag_list, ~{
      # Pad with NA to ensure consistent length
      c(.x, rep(NA_character_, max(0, first(max_tags) - length(.x))))
    })
  ) |>
  # Unnest the tag columns into separate columns
  unnest_wider(tag_columns, names_sep = "_") |>
  # Clean up column names
  rename_with(~str_replace(.x, "tag_columns_", "Tag_"), starts_with("tag_columns_")) |>
  # Remove temporary columns
  select(-tag_list, -max_tags)

# Creates dot plot
p_ig_stats <- ggplot(ig_stats, aes(Impressions, Engagements, color = Content.Type)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)
print(ggMarginal(p_ig_stats, type = "density", groupColour = TRUE, groupFill = TRUE))

engagements_reg <- lm(Engagements ~ Impressions, data = ig_stats)
engagements_r_sqrd <- signif(summary(engagements_reg)$r.squared, digits = 3)
print(engagements_r_sqrd)
