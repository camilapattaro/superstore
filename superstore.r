## Data Preparation

```{r echo=TRUE, results='hide', message=FALSE, warning=FALSE}
# Loading libraries
library(readxl)
library(janitor)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(treemapify)
library(tidyr)
library(sf)
library(scales)
library(maps)
```

```{r echo=TRUE, results='hide', message=FALSE, warning=FALSE}
# Loading the data
ss <-read_excel("C:\\Users\\camil\\OneDrive\\Documents\\KAGGLE\\sample_-_superstore.xls")
```

```{r}
# Getting a overview of the data
glimpse(ss)
```

```{r}
# Cleaning and standardizing column names
ss <- clean_names(ss)
colnames(ss)
```

```{r}
# Checking missing values
colSums(is.na(ss))
```

```{r}
# Removing duplicate rows
ss <- ss[!duplicated(ss),]
```

## Data Analysis

### 1. How have sales evolved over time?

```{r}
# Extracting month and year from order_date column
ss$year_month <- format(ss$order_date, "%Y-%m")
head(ss$year_month)
```

```{r}
# Aggregating total sales by year and month
monthly_sales <- aggregate(sales ~ year_month, data = ss, sum)
head(monthly_sales)
```
```{r}
class(ss$year_month)
```

# We can see the year_month collumn have the wrong data type.

```{r}
# Converting year_month to Date by appending "-01" to represent the first day of the month
ss$year_month <- as.Date(paste0(ss$year_month, "-01"), format="%Y-%m-%d")
head(ss$year_month)
```

```{r}
# Convert 'year_month' to Date format
monthly_sales$year_month <- as.Date(paste0(monthly_sales$year_month, "-01"), format="%Y-%m-%d")
head(monthly_sales)
```
Now we have the right data type.

```{r}
# Creating a line plot with a trend line
ggplot(monthly_sales, aes(x = year_month, y = sales)) +
  geom_line(color = "steelblue") +   
  geom_smooth(method = "lm", color = "black", se = FALSE) +  
  labs(title = "Sales Over Time", x = "Year", y = "Sales")
```
# We observe a **growth** in sales over the years.

### 2. What percentage of sales and profit is represented by each category?

First let`s analyse Sales:
```{r}
# Grouping sales by category and calculate percentages
category_sales <- ss %>%
  group_by(category) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE)) %>%
  mutate(percentage = (total_sales / sum(total_sales)) * 100)
```

```{r}
# Creating a pie chart
# Define custom colors (SteelBlue and Tomato)
custom_colors <- c("steelblue", "tomato", "forestgreen")

pie(category_sales$percentage, 
    labels = paste0(round(category_sales$percentage, 1), "%"), 
    main = "Sales by Category", 
    col = custom_colors,
    font.main = 1)
legend("topright", legend = category_sales$category, fill = custom_colors)
```
Then Profits:
```{r}
# Grouping profit by category and calculate percentages
category_profit <- ss %>% 
  group_by(category) %>% 
  summarise(total_profit = sum(profit)) %>% 
  mutate(percentage = (total_profit / sum(total_profit)) * 100)
```

```{r}
# Creating a pie chart
pie(category_profit$percentage,
    labels = paste0(round(category_profit$percentage, 1), "%"),
    main = "Profit by Category",
    col = custom_colors,
    font.main = 1)
legend("topright", legend = category_sales$category, fill = custom_colors)
```

# We can see that sales are distributed almost equally among the categories; however, profits are not. *Technology* accounts for more than half of the total profit, followed by *Office Supplies* with 43%, while *Furniture* represents only 6% of the total profit.

### 3. Which product sub-categories have the highest sales?
```{r}
# Summarizing sales by sub_category
sub_category_sales <- ss %>%
  group_by(sub_category) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE)) %>% 
  mutate(percentage = (total_sales / sum(total_sales))*100)
```

```{r}
# Creating a treemap
ggplot(sub_category_sales, aes(area = total_sales, fill = sub_category, 
                               label = paste0(sub_category, "\n", round(percentage, 1), "%"))) +
  geom_treemap() +
  geom_treemap_text(fontface = "bold", colour = "white", place = "centre", grow = TRUE) +
  labs(title = "Sales by Sub-Category") +
  theme_minimal() +
  theme(legend.position = "none")
```

# *Phones* and *Chairs* are the sub-categories with the highest sales, each representing 14% of the total sales.

### 4. Which product sub-category generates the highest sales and which one generates the most profit? What is the relationship between them?
```{r}
# Summarizing sales and profit by category & sub-category
category_sales_profit <- ss %>%
  group_by(category, sub_category) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE),
            total_profit = sum(profit, na.rm = TRUE),
            .groups = "drop"  # Drop the grouping after summarizing
  ) %>%
  pivot_longer(cols = c(total_sales, total_profit), 
               names_to = "metric", 
               values_to = "value")  # Convert to long format
```


```{r}
# Creating a side-by-side bar chart
ggplot(category_sales_profit, aes(x = value, y = reorder(sub_category, value), fill = metric)) +
  geom_col(position = "dodge") +  
  facet_wrap(~category, scales = "free_y") +
  labs(title = "Sales & Profit by Sub-Category", x = "Value ($)", y = "Sub-Category") +
  theme_minimal() +
  scale_fill_manual(values = c("total_sales" = "steelblue", "total_profit" = "tomato")) +  
  scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme(legend.title = element_blank())
```

# As we saw earlier, *Phones* and *Chairs* have the same revenue, but now we can observe that Phones generate much more profit than Chairs. 
*Tables* represent a high sales value but without profits, resulting in a significant loss. *Bookcases*, *Supplies*, and *Fasteners* also show losses. 
The only sub-category within Technology that <u>does not present significant profit</u> is *Machines*.

### 5. What are the TOP 10 products according to their sales? 

```{r}
# Summarizing the top 10 products by total sales
top_products <- ss %>%
  group_by(product_name) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE)) %>%
  arrange(desc(total_sales)) %>%
  slice_head(n = 10)
```

```{r}
# Creating the horizontal bar chart
ggplot(top_products, aes(x = total_sales, y = reorder(product_name, total_sales))) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(title = "Top 10 Best-Selling Products", x = "Sales ($)", y = "Product Name") +
  theme_minimal()
```

# *The Canon imageCLASS 2200 Advanced Copier* sells twice as much as the second-best product.

### 6. Which  states have the highest sales?

```{r}
# Summarizing sales by state
state_sales <- ss %>%
  group_by(state) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE))
```

```{r}
# Using the 'maps' package to get a map of US states
us_states_map <- map_data("state")
```

```{r}
# Merging the sales data with the map data (by state)
state_sales$region <- tolower(state_sales$state)  # Ensure state names match
merged_data <- left_join(us_states_map, state_sales, by = "region")
```

```{r}
# Creating a choropleth map
ggplot(merged_data, aes(x = long, y = lat, group = group, fill = total_sales)) +
  geom_polygon(color = "white") +
  scale_fill_viridis_c(option = "C", labels = label_number(scale_cut = cut_short_scale())) +
  theme_minimal() +
  labs(title = "Sales by State", fill = "Total Sales") +
  theme(axis.text = element_blank(), axis.title = element_blank())
