library(ggplot2)
library(ggpubr)
library(plotly)
library(ggrepel)
library(dplyr)
library(stringr)
library(tidyr)
library(caTools)
library(isotree)
library(knitr)
library(vcd)
library(reshape2)
library(kableExtra)
library(car)
library(DescTools)

attach(zomato)
View(zomato)

sum(duplicated(zomato)) # No. of duplicates 


#############################################################################################
#                                     DATA PRE-PROCESSING                                   #
#############################################################################################


# =============================== Dropping irrelevant columns ===============================

zomato <- subset(zomato, select = -c(url, address, name, phone, location, dish_liked, 
                                     reviews_list, menu_item))
View(zomato)


# ============================ Cleaning 'rate' column: Part 1 ===============================

# Part 1.1: Extract the decimal part except '/5' 
zomato$rate <- gsub("/5", "", zomato$rate)

# Part 1.2: Dropping 'NEW' restaurants and convert the ratings into numerical values
zomato <- zomato %>%
  filter(rate != "NEW") %>%
  mutate(rate = as.numeric(as.character(rate)))

unique(zomato$rate)

# ================= Cleaning 'approx_cost(for two people)' column: Part 1 ===================

# Part 1.1: Replace ',' in the cost
zomato$`approx_cost(for two people)` <- gsub(",", "", zomato$`approx_cost(for two people)`)

# Part 1.2: Convert the cost into numerical values
zomato$`approx_cost(for two people)` <- as.numeric(zomato$`approx_cost(for two people)`)


# ================================ Cleaning 'votes' column ==================================

# Part 1: Convert the votes into numerical values
zomato$votes <- as.numeric(zomato$votes)
sum(is.na(zomato$votes)) # No. of 'NA' 


# ============================= Feature engineering: Part 1 =================================

zomato$VoteCategory <- ifelse(zomato$votes <= 50, "Low Popularity",
                              ifelse(zomato$votes <= 250, "Moderate Popularity",
                                     ifelse(zomato$votes <= 1000, "Popular", "Highly Popular")))


# ========================= Cleaning 'cuisines' column: Part 1 ==============================

# Part 1.1: Categorize the cuisines 

categorize_cuisines <- function(cuisine) {
  # Check if the cuisine is NA
  if (is.na(cuisine)) {
    return(NA)
  }
  
  cuisine_list <- str_split(cuisine, ",\\s*")[[1]]
  
  replacements <- c(
    "Afghan" = "Afghani",
    "Bubble Tea" = "Beverages",
    "Coffee" = "Beverages",
    "Cafe" = "Beverages",
    "Tea" = "Beverages",
    "Bubble Beverages" = "Beverages",
    "Ice Cream" = "Desserts",
    "Mithai" = "Desserts",
    "Bar Food" = "Fast Food",
    "Burger" = "Fast Food",
    "Finger Food" = "Fast Food",
    "Momos" = "Fast Food",
    "Rolls" = "Fast Food",
    "Wraps" = "Fast Food",
    "Street Food" = "Fast Food",
    "Juices" = "Healthy Food",
    "Salad" = "Healthy Food",
    "Sandwich" = "Healthy Food",
    "Grill" = "BBQ",
    "Steak" = "BBQ",
    "Sushi" = "Japanese",
    "Tex-Mex" = "Mexican",
    "Roast Chicken" = "Chinese",
    "Charcoal Chicken" = "Chinese",
    "Pizza" = "Italian",
    "Biryani" = "South Indian",
    "Kebab" = "North Indian"
  )
  
  cuisine_list <- sapply(cuisine_list, function(x) {
    if (x %in% names(replacements)) {
      replacements[x]
    } else {
      x
    }
  })
  
  unique_cuisines <- paste(unique(cuisine_list), collapse = ", ")
  return(unique_cuisines)
}

# Apply the function while preserving NA values
zomato <- zomato %>% 
  mutate(cuisines = if_else(is.na(cuisines), NA_character_, sapply(cuisines, categorize_cuisines)))

sum(is.na(zomato$cuisines))


# =============================== Cleaning 'rest_type' column ===============================

# Part 1: Consider any type of restaurant with a count < 1000. Change their type as 'Other'

# Number of restaurants with each type
rest_type_counts <- zomato %>%
  dplyr::select(rest_type) %>%
  tidyr::separate_rows(rest_type, sep = ", ") %>%  
  dplyr::count(rest_type, sort = TRUE)

print(rest_type_counts, n = 30)

# Identify restaurant types with counts < 1000
low_count_types <- rest_type_counts %>%
  filter(n < 1000) %>%
  pull(rest_type)  # Extract the names of types with low counts

print(low_count_types)

# Replace these low-count types with 'Other' 
zomato <- zomato %>%
  mutate(rest_type = strsplit(as.character(rest_type), ", ")) %>%
  rowwise() %>%
  mutate(rest_type = paste(ifelse(rest_type %in% low_count_types, "Other", rest_type), 
                           collapse = ", ")) %>%
  ungroup()

# The updated counts 
updated_counts <- zomato %>%
  separate_rows(rest_type, sep = ", ") %>%
  count(rest_type, sort = TRUE)

print(updated_counts)


# ============================ Cleaning 'listed_in(city)' column ===========================

# Part 1: Count the restaurants in each city
 
listed_in_city_counts <- zomato %>%
  count(`listed_in(city)`, sort = TRUE)

print(listed_in_city_counts, n = 35)

# Part 2: Combine multiple 'Koramangala' block locations

zomato$`listed_in(city)` <- ifelse(grepl("Koramangala", zomato$`listed_in(city)`), "Koramangala", 
                                   zomato$`listed_in(city)`)

# The updated counts 
updated_city_counts <- zomato %>%
  count(`listed_in(city)`, sort = TRUE)

print(updated_city_counts, n = 30)


# ============================== Rename the existing columns ================================

# Get the current column names
current_names <- colnames(zomato)

# Define the new names for the columns you want to rename
new_names <- c(
  "OnlineOrder", "TableBooking", "Rating", "Votes", "RestaurantType", "Cuisines", "AvgCostForTwo",
  "MealType", "City"
)

# Replace only the relevant column names
colnames(zomato)[1:9] <- new_names

# =================== Split the data set into 'training' and 'testing' ======================

set.seed(123)

# Part 1: Define the split ratio as 80% for training and 20% for testing
SplitRatio <- 0.8
split <- sample(1:nrow(zomato), size = round(SplitRatio*nrow(zomato)))

# Part 2: Create training and testing sets
train <- zomato[split, ]
test <- zomato[-split, ]

# Part 3: Check the dimensions of the split data sets
dim(train)
dim(test)

View(train)
View(test)

# Part 4: Ensuring a balanced distribution of ratings in 'training' and 'testing'

# Create the density data
generate_density_data <- function(variable) {
  density_data <- density(variable, na.rm = T)
  data.frame(x = density_data$x, y = density_data$y)
}

train_density <- generate_density_data(train$Rating)
test_density <- generate_density_data(test$Rating)

plot_ly() %>%
  add_trace(data = train_density, x = ~x, y = ~y, type = 'scatter', mode = 'lines', 
            name = 'Train Set', line = list(color = 'red', width = 2)) %>%
  add_trace(data = test_density, x = ~x, y = ~y, type = 'scatter', mode = 'lines', 
            name = 'Test Set', line = list(color = 'black', width = 2, dash = 'dash')) %>%
  layout(title = list(text = 'Density Plot of Ratings in Train and Test Sets', x = 0.5),
         xaxis = list(title = 'Rating', titlefont = list(size = 14, face = 'bold'),
                      tickfont = list(size = 12, face = 'bold'), showgrid = TRUE, gridcolor = 'gray80'),
         yaxis = list(title = 'Density', titlefont = list(size = 14, face = 'bold'),
                      tickfont = list(size = 12, face = 'bold'), showgrid = TRUE, gridcolor = 'gray80'),
         legend = list(x = 0.8, y = 0.95),
         template = "plotly_white")


# ========================== Cleaning 'rate' column: Part 2 =================================

# Part 2.1: Consider the distribution of rate in 'training' set
# Since, the distribution is negatively skewed, 'median' will be the appropriate central measure 
# for replacing 'NA'

# Part 2.2: Calculate the median of ratings in 'training' set
median_rate <- median(train$Rating, na.rm = TRUE)

# Part 2.3: Impute the median of rate for '-' and 'NA' in 'training' set 
train$Rating[is.na(train$Rating)] <- median_rate

# Part 2.4: Apply the same median to 'test' set
test$Rating[is.na(test$Rating)] <- median_rate


# ============================= Feature engineering: Part 2 =================================

# Categorize 'Rate' column based on fixed rating thresholds
train$RatingCategory <- ifelse(train$Rating >= 4.0, "Excellent",
                                    ifelse(train$Rating >= 3.0, "Good",
                                           ifelse(train$Rating >= 2.5, "Average", "Poor")))

test$RatingCategory <- ifelse(test$Rating >= 4.0, "Excellent",
                                   ifelse(test$Rating >= 3.0, "Good",
                                          ifelse(test$Rating >= 2.5, "Average", "Poor")))


# ================= Cleaning 'approx_cost(for two people)' column: Part 2 ===================

# Part 2.1: Consider the distribution of cost in 'training' set

# Generate density data
cost_density <- generate_density_data(train$AvgCostForTwo)

# Create the density plot
plot_ly(cost_density, x = ~x, y = ~y, type = 'scatter', mode = 'lines', 
        line = list(color = 'red', width = 2), name = 'Density') %>%
  layout(title = "Density Plot of Cost (for two people)", 
         xaxis = list(title = "Cost (₹)"), 
         yaxis = list(title = "Density"),
         template = "plotly_white")


# Since, the distribution is positively skewed, 'median' will be the appropriate central measure 
# for replacing 'NA'

# Part 2.2: Calculate the median of cost in 'training' set
median_cost <- median(train$AvgCostForTwo, na.rm = TRUE)

# Part 2.3: Impute the median of cost for 'NA' in 'training' set
train$AvgCostForTwo[is.na(train$AvgCostForTwo)] <- median_cost

# Part 2.4: Apply the same median to 'test' set
test$AvgCostForTwo[is.na(test$AvgCostForTwo)] <- median_cost


# ============================= Feature engineering: Part 3 =================================

# Categorize 'approx_cost(for two people)' column based on fixed cost thresholds
train$CostCategory <- ifelse(train$AvgCostForTwo <= 100, "Cheap",
                                  ifelse(train$AvgCostForTwo <= 250, "Reasonable",
                                         ifelse(train$AvgCostForTwo <= 500, "Affordable", "Expensive")))

test$CostCategory <- ifelse(test$AvgCostForTwo <= 100, "Cheap",
                                 ifelse(test$AvgCostForTwo <= 250, "Reasonable",
                                        ifelse(test$AvgCostForTwo <= 500, "Affordable", "Expensive")))


# ========================= Cleaning 'cuisines' column: Part 2 ==============================

# Part 2.1: Split multiple cuisines into separate rows and count occurrences in 'training' set

cuisine_counts <- train %>%
  dplyr::select(Cuisines) %>%
  dplyr::filter(!is.na(Cuisines)) %>%
  tidyr::separate_rows(Cuisines, sep = ", ") %>%  # Split multiple cuisines
  dplyr::count(Cuisines, sort = TRUE)  # Count occurrences and sort

# Compute percentages for top 10 cuisines
cuisine_top10 <- head(cuisine_counts, 10)
cuisine_top10$percentage <- (cuisine_top10$n / sum(cuisine_top10$n)) * 100  # Convert to %

plot_ly(cuisine_top10, 
        x = ~n, 
        y = ~reorder(Cuisines, n), 
        type = 'bar', 
        orientation = 'h',
        text = ~paste0(round(percentage, 1), '%'), 
        textposition = 'outside', 
        marker = list(color = colorRampPalette(c("red", "darkorange", "goldenrod", "yellow"))(10))) %>%
  layout(title = 'Top 10 Most Frequent Cuisines',
         xaxis = list(title = 'Count'),
         yaxis = list(title = 'Cuisine Type'),
         showlegend = FALSE)

most_frequent_cuisine <- cuisine_top10$Cuisines[1]  # Get the top cuisine

# Part 2.2: Impute the most frequent cousin type for 'NA' in 'training' set 
train$Cuisines[is.na(train$Cuisines)] <- most_frequent_cuisine

# Part 2.3: Apply the same most frequent cousin type for 'NA' in 'test' set
test$Cuisines[is.na(test$Cuisines)] <- most_frequent_cuisine


# ============================= Feature engineering: Part 4 =================================

# Part 3.1: Consider the 'training' set

# Split the 'cuisines' column and extract unique individual cuisines
unique_cuisines_train <- train %>%
  dplyr::select(Cuisines) %>%
  tidyr::separate_rows(Cuisines, sep = ", ") %>%  # Split multiple cuisines into separate rows
  dplyr::distinct() %>%
  dplyr::arrange(Cuisines)  # Arrange alphabetically

print(unique_cuisines_train, n = 100)

# Categorize 'cuisines' column based on vegetarian or non-vegetarian classification
veg_cuisines <- c("Gujarati", "Jain", "Rajasthani", "Vegan", "Sattvic")
non_veg_cuisines <- c("Afghani", "BBQ", "Bengali", "Chinese", "Goan", "Hyderabadi", "Italian", 
                      "Japanese", "Lebanese", "Mughlai", "Seafood", "Thai", "Turkish", "Mediterranean")

train$CuisineType <- ifelse(grepl(paste(veg_cuisines, collapse="|"), train$Cuisines, ignore.case = TRUE), 
                             "Vegetarian",
                             ifelse(grepl(paste(non_veg_cuisines, collapse="|"), train$Cuisines, 
                                          ignore.case = TRUE), 
                                    "Non-Vegetarian", "Mixed"))

# Part 3.2: Consider the 'test set

# Categorize 'cuisines' column based on vegetarian or non-vegetarian classification
test$CuisineType <- ifelse(grepl(paste(veg_cuisines, collapse="|"), test$Cuisines, ignore.case = TRUE), 
                            "Vegetarian",
                            ifelse(grepl(paste(non_veg_cuisines, collapse="|"), test$Cuisines, 
                                         ignore.case = TRUE), 
                                   "Non-Vegetarian", "Mixed"))


# ================================== Save cleaned data ======================================

write.csv(train, "train.csv", row.names = FALSE)
write.csv(test, "test.csv", row.names = FALSE)


#############################################################################################
#                             EXPLORATORY DATA ANALYSIS (EDA)                               #
#############################################################################################

attach(train)
summary(train)

# ================================== Uni-variate Analysis ===================================


# ============================== Part 1: Qualitative Variables ==============================

# ------------------------------- Graphical method: Pie charts ------------------------------

create_pie_chart <- function(data, variable, title, color_mapping, text_color = "black") {
  counts <- data %>%
    count(!!sym(variable)) %>%
    mutate(percentage = round(n / sum(n) * 100, 1))
  
  # Filter color mapping to match existing categories
  val_filtered <- color_mapping[names(color_mapping) %in% counts[[variable]]]
  
  fig <- plot_ly(
    counts,
    labels = ~get(variable),
    values = ~n,
    type = 'pie',
    textinfo = 'label+percent',
    insidetextorientation = 'radial',
    marker = list(colors = val_filtered),
    textfont = list(size = 16, color = text_color, family = "Arial", weight = "bold")
  )
  
  fig <- fig %>%
    layout(
      title = list(text = title, x = 0.5),
      showlegend = F
    )
  
  fig
}

# 1.1 OnlineOrder
online_order_colors <- c("Yes" = "darkorange", "No" = "yellow")
create_pie_chart(train, "OnlineOrder", "Online Order Availability", online_order_colors, "black")

# 1.2 TableBooking
table_booking_colors <- c("Yes" = "red", "No" = "orange")
create_pie_chart(train, "TableBooking", "Table Booking Availability", table_booking_colors, "white")

# 1.3 CuisineType
cuisine_type_colors <- c("Vegetarian" = "yellow", "Non-Vegetarian" = "orange", "Mixed" = "black")
create_pie_chart(train, "CuisineType", "Cuisine Categories", cuisine_type_colors, "black")

#1.4 VoteCategory
vote_category_colors <- c("Highly Popular" = "red", "Popular" = "darkorange", 
                          "Moderate Popularity" = "goldenrod", "Low Popularity" = "yellow")

create_pie_chart(train, "VoteCategory", "Vote Category Distribution", vote_category_colors)

#1.5 RatingCategory
rate_category_colors <- c("Excellent" = "red", "Good" = "orange", "Average" = "yellow", "Poor" = "black")

create_pie_chart(train, "RatingCategory", "Rating Category Distribution", rate_category_colors)

#1.6 CostCategory
cost_category_colors <- c("Expensive" = "red", "Affordable" = "orange", "Reasonable" = "yellow", "Cheap" = "gold")

create_pie_chart(train, "CostCategory", "Cost Category Distribution", cost_category_colors)


# ------------------------------- Graphical method: Bar charts -------------------------------

#1.7 Cuisines

# Compute percentages for top 10 cuisines
cuisine_top10 <- head(cuisine_counts, 10)
cuisine_top10$percentage <- (cuisine_top10$n / sum(cuisine_top10$n)) * 100  # Convert to %

plot_ly(cuisine_top10, 
        x = ~n, 
        y = ~reorder(Cuisines, n), 
        type = 'bar', 
        orientation = 'h',
        text = ~paste0(round(percentage, 1), '%'), 
        textposition = 'outside', 
        marker = list(color = colorRampPalette(c("red", "darkorange", "goldenrod", "yellow"))(10))) %>%
  layout(title = 'Top 10 Most Frequent Cuisines',
         xaxis = list(title = 'Count'),
         yaxis = list(title = 'Cuisine Type'),
         showlegend = FALSE)

#1.8 RestaurantType

# Number of restaurants with each type 
rest_type_counts_train <- train %>%
  select(RestaurantType) %>%
  separate_rows(RestaurantType, sep = ", ") %>%  
  count(RestaurantType, sort = TRUE)  

rest_type_counts_train$percentage <- (rest_type_counts_train$n / sum(rest_type_counts_train$n)) * 100  # Convert to %

plot_ly(rest_type_counts_train, 
        x = ~n, 
        y = ~reorder(RestaurantType, n), 
        type = 'bar', 
        orientation = 'h',
        text = ~paste0(round(percentage, 1), '%'), 
        textposition = 'outside', 
        marker = list(color = colorRampPalette(c("red", "orange", "goldenrod", "yellow"))(10))) %>%
  layout(title = 'Restaurant Types',
         xaxis = list(title = 'Count'),
         yaxis = list(title = 'Type'),
         showlegend = FALSE)

#1.9 MealType

meal_type_counts_train <- table(train$MealType)
meal_type_counts_train <- as.data.frame(meal_type_counts_train)
colnames(meal_type_counts_train) <- c("MealType", "Count")
meal_type_counts_train$percentage <- (meal_type_counts_train$Count / sum(meal_type_counts_train$Count)) * 100

plot_ly(meal_type_counts_train, 
        x = ~Count, 
        y = ~reorder(MealType, Count), 
        type = 'bar', 
        orientation = 'h',
        text = ~paste0(round(percentage, 1), '%'), 
        textposition = 'outside', 
        marker = list(color = colorRampPalette(c("red", "orange", "brown", "yellow"))(nrow(meal_type_counts_train)))) %>%
  layout(title = 'Meal Types',
         xaxis = list(title = 'Count'),
         yaxis = list(title = 'Type'),
         showlegend = FALSE)

#1.10 City

city_counts_train <- table(train$City)
city_counts_train <- as.data.frame(city_counts_train)
colnames(city_counts_train) <- c("City", "Count")
city_counts_train <- city_counts_train %>%
  arrange(desc(Count)) %>%
  head(10)
city_counts_train$percentage <- (city_counts_train$Count / sum(city_counts_train$Count)) * 100

plot_ly(city_counts_train, 
        x = ~Count, 
        y = ~reorder(City, Count), 
        type = 'bar', 
        orientation = 'h',
        text = ~paste0(round(percentage, 1), '%'), 
        textposition = 'outside', 
        marker = list(color = colorRampPalette(c("gold", "orange", "brown", "red"))(nrow(city_counts_train)))) %>%
  layout(title = 'Top 10 Cities',
         xaxis = list(title = 'Count'),
         yaxis = list(title = 'City'),
         showlegend = FALSE)


# =============================== Part 2: Quantitative Variables ==============================

# -------------------------------- Graphical method: Histograms -------------------------------

# 2.1 Rating
plot_ly(train, x = ~Rating, type = "histogram", 
        nbinsx = 50, marker = list(color = 'gold', line = list(color = 'black', width = 1)), opacity = 0.6) %>% 
  layout(title = "Histogram of Rating", 
         xaxis = list(title = "Rate"), 
         yaxis = list(title = "Count"),
         template = "plotly_white")

# 2.2 Votes
plot_ly(train, x = ~Votes, type = "histogram", 
        nbinsx = 50, marker = list(color = 'yellow', line = list(color = 'black', width = 1)), opacity = 0.6) %>% 
  layout(title = "Histogram of Votes", 
         xaxis = list(title = "Votes"), 
         yaxis = list(title = "Count"),
         template = "plotly_white")

# 2.3 AvgCostForTwo
plot_ly(train, x = ~AvgCostForTwo, type = "histogram", 
        nbinsx = 50, marker = list(color = 'red', line = list(color = 'black', width = 1)), opacity = 0.6) %>% 
  layout(title = "Histogram of Cost (for two people)", 
         xaxis = list(title = "Cost (₹)"), 
         yaxis = list(title = "Count"),
         template = "plotly_white")


# ----------------------------- Graphical method: Density plots -----------------------------

# Create the density data
generate_density_data <- function(variable) {
  density_data <- density(variable)
  data.frame(x = density_data$x, y = density_data$y)
}

#2.1 Rating

# Generate density data
rate_density <- generate_density_data(train$Rating)

# Create the density plot
plot_ly(data = rate_density, x = ~x, y = ~y, type = 'scatter', mode = 'lines', 
        line = list(color = 'black', width = 2), name = 'Density') %>%
  layout(title = "Density Plot of Rating", 
         xaxis = list(title = "Rate"), 
         yaxis = list(title = "Density"),
         template = "plotly_white")

#2.2 Votes

# Generate density data
vote_density <- generate_density_data(train$Votes)

# Create the density plot
plot_ly(data = vote_density, x = ~x, y = ~y, type = 'scatter', mode = 'lines', 
        line = list(color = 'brown', width = 2), name = 'Density') %>%
  layout(title = "Density Plot of Votes", 
         xaxis = list(title = "Votes"), 
         yaxis = list(title = "Density"),
         template = "plotly_white")


#2.3 AvgCostForTwo

# Generate density data
cost_density <- generate_density_data(train$AvgCostForTwo)

# Create the density plot
plot_ly(data = cost_density, x = ~x, y = ~y, type = 'scatter', mode = 'lines', 
        line = list(color = 'red', width = 2), name = 'Density') %>%
  layout(title = "Density Plot of Cost (for two people)", 
         xaxis = list(title = "Cost (₹)"), 
         yaxis = list(title = "Density"),
         template = "plotly_white")


# ------------------------------- Graphical method: Box plots -------------------------------

# 2.1 Rating 
plot_ly(train, y = ~Rating, type = 'box', 
        boxpoints = 'outliers', 
        marker = list(color = 'black'), 
        fillcolor = 'gold', 
        line = list(color = 'black'), 
        opacity = 0.8) %>% 
  layout(title = list(text = 'Box Plot of Rating', x = 0.5),
         yaxis = list(title = 'Rating', titlefont = list(size = 14, face = 'bold'),
                      tickfont = list(size = 12, face = 'bold')),
         xaxis = list(showticklabels = FALSE),
         template = "plotly_white")
sum(boxplot.stats(train$Rating)$out)
# 2.2 Votes 
plot_ly(train, y = ~Votes, type = 'box', 
        boxpoints = 'outliers', 
        marker = list(color = 'black'), 
        fillcolor = 'orange', 
        line = list(color = 'black'), 
        opacity = 0.8) %>% 
  layout(title = list(text = 'Box Plot of Votes', x = 0.5),
         yaxis = list(title = 'Votes', titlefont = list(size = 14, face = 'bold'),
                      tickfont = list(size = 12, face = 'bold')),
         xaxis = list(showticklabels = FALSE),
         template = "plotly_white")

# 2.3 AvgCostForTwo 
plot_ly(train, y = ~AvgCostForTwo, type = 'box', 
        boxpoints = 'outliers', 
        marker = list(color = 'black'), 
        fillcolor = 'red', 
        line = list(color = 'black'), 
        opacity = 0.8) %>% 
  layout(title = list(text = 'Box Plot of Cost (for two people)', x = 0.5),
         yaxis = list(title = 'Cost (₹)', titlefont = list(size = 14, face = 'bold'),
                      tickfont = list(size = 12, face = 'bold')),
         xaxis = list(showticklabels = FALSE),
         template = "plotly_white")


# ------------------------------------- Summary Statistics -----------------------------------

generate_summary_stats <- function(data, variable, caption) {
  summary_stats <- summary(data[[variable]])
  summary_table <- data.frame(
    Statistic = names(summary_stats),
    Value = as.numeric(summary_stats)
  )
  
  # Calculate additional statistics
  std_dev <- sd(data[[variable]], na.rm = TRUE)
  variance <- var(data[[variable]], na.rm = TRUE)
  additional_stats <- data.frame(
    Statistic = c("Std. Dev.", "Variance"),
    Value = c(std_dev, variance)
  )
  
  # Combine with the original summary table
  summary_table <- rbind(summary_table, additional_stats)
  kable(summary_table, caption = caption, align = 'c')
}

#2.1 Summary Statistics for Rating
generate_summary_stats(train, 'Rating', 'Summary Statistics of Rating')

#2.2 Summary Statistics for Votes
generate_summary_stats(train, 'Votes', 'Summary Statistics of Votes')

#2.3 Summary Statistics for AvgCostForTwo
generate_summary_stats(train, 'AvgCostForTwo', 'Summary Statistics of Cost (₹)')


# =================================== Bi-variate Analysis ===================================


# ========================== Part 1: Qualitative vs. Quantitative ===========================

# ------------------------------- Graphical method: Box plots -------------------------------

generate_boxplot <- function(data, x_var, y_var, title, x_label, colors_range) {
  custom_colors <- colorRampPalette(colors_range)(length(unique(data[[x_var]])))
  
  plot_ly(data = data, 
          x = as.formula(paste("~factor(`", x_var, "`)", sep="")), 
          y = as.formula(paste("~`", y_var, "`", sep="")), 
          type = 'box', 
          color = as.formula(paste("~factor(`", x_var, "`)", sep="")), 
          colors = custom_colors,  # Apply custom color palette
          boxpoints = 'outliers',
          marker = list(size = 5),
          line = list(color = 'black', width = 1)) %>%  # Black borders for clarity
    layout(title = list(text = title, x = 0.5),
           xaxis = list(title = x_label, titlefont = list(size = 14, face = 'bold'),
                        tickfont = list(size = 12, face = 'bold')),
           yaxis = list(title = 'Rating', titlefont = list(size = 14, face = 'bold'),
                        tickfont = list(size = 12, face = 'bold')),
           showlegend = FALSE,
           template = "plotly_white")
}

#1.1 Ratings by Top 12 Cuisines
train_separated <- train %>% separate_rows(Cuisines, sep = ", ")
train_cuisine_ratings <- train_separated %>% group_by(Cuisines) %>% 
  summarise(Med_Rating = median(Rating, na.rm = TRUE))

top_10_cuisines <- train_cuisine_ratings %>% top_n(10, Med_Rating) %>% pull(Cuisines)
train_filtered <- train_separated %>% filter(Cuisines %in% top_10_cuisines) %>%
  left_join(train_cuisine_ratings, by = "Cuisines")

generate_boxplot(train_filtered, 'Cuisines', 'Rating', 'Ratings by Top 12 Cuisines', 'Cuisines', 
                 c("red", "darkorange", "orange", "gold", "yellow"))

#1.1.1 Number of Cuisines per Restaurant vs Rating
generate_boxplot(train %>% mutate(Number_of_Cuisines = str_count(Cuisines, ",") + 1),
                 'Number_of_Cuisines', 'Rating', 'Effect of Number of Cuisines on Ratings', 'Number of Cuisines', 
                 c("red", "darkorange", "orange", "gold", "yellow"))

#1.2 CostCategory vs Rating
generate_boxplot(train, 'CostCategory', 'Rating', 'Ratings by Cost Category', 'Cost Category', 
                 c("red", "darkorange", "orange", "gold", "yellow"))

#1.3 VoteCategory vs Rating 
generate_boxplot(train, 'VoteCategory', 'Rating', 'Ratings vs Voting Categories', 'Voting Category',
                 c("red", "orange", "gold", "yellow"))

#1.4 OnlineOrder vs Rating
generate_boxplot(train, 'OnlineOrder', 'Rating', 'Ratings vs Ability to Order Online', 'Order Online', 
                 c("red", "gold"))

#1.5 TableBooking vs Rating
generate_boxplot(train, 'TableBooking', 'Rating', 'Ratings vs Ability to Book a Table', 'Table Booking', 
                 c("yellow", "orange"))

#1.6 MealType vs Rating
generate_boxplot(train, 'MealType', 'Rating', 'Ratings vs Meal Type', 'Meal Type', 
                 c("red", "orange", "yellow"))

#1.7 CuisineType vs Rating
generate_boxplot(train, 'CuisineType', 'Rating', 'Ratings vs Cuisine Type', 'Cuisine Type', 
                 c("red", "orange", "gold"))


# ========================== Part 2: Qualitative vs. Qualitative ============================

# ------------------------------ Graphical method: Bar charts -------------------------------

#2.1. Rating by City
ratings_by_city <- train %>%
  group_by(City) %>%
  summarise(Med_Rating = median(Rating), .groups = 'drop')

plot_ly(ratings_by_city, 
        x = ~City, 
        y = ~Med_Rating, 
        type = 'bar', 
        color = ~City, 
        colors = c("red", "darkorange", "orange", "goldenrod", "gold", "yellow")) %>%
  layout(title = list(text = "Median Rating by City", x = 0.5),
         xaxis = list(title = "City", titlefont = list(size = 14, face = 'bold'),
                      tickangle = -45, tickfont = list(size = 12)),
         yaxis = list(title = "Median Rating", titlefont = list(size = 14, face = 'bold'),
                      tickfont = list(size = 12)),
         legend = list(title = list(text = "City"), orientation = "v"),
         template = 'plotly_white')

# ---------------------------- Graphical method: Interactive map ----------------------------

library(tidygeocoder) # For geocoding
library(leaflet)      # For mapping
library(dplyr)        # For data manipulation

# Step 1: Extract unique cities from the train dataset
unique_cities <- train %>%
  distinct(City) %>%
  mutate(location_query = paste(City, "Bangalore, India"))  # Ensure location is in Bangalore

# Step 2: Perform geocoding on the unique cities
geocoded_cities <- unique_cities %>%
  geocode(location_query, method = 'osm') %>%  # Perform geocoding
  rename(city_latitude = lat, city_longitude = long)  # Rename columns

# Step 3: Calculate median ratings by city
ratings_by_city <- train %>%
  group_by(City) %>%
  summarise(Med_Rating = median(Rating), .groups = 'drop')

# Step 4: Merge geocoding results with median ratings
city_data <- geocoded_cities %>%
  left_join(ratings_by_city, by = "City")

# Step 5: Create an interactive Leaflet map

# Labels for interactivity
labels <- paste0(
  "<strong>City: </strong>", city_data$City,
  "<br/><strong>Median Rating: </strong>", city_data$Med_Rating
)

# Define a red-to-yellow color palette (YlOrRd)
pal <- colorNumeric(palette = "YlOrRd", domain = city_data$Med_Rating)

# Create Leaflet map
m <- leaflet(city_data) %>%
  addTiles() %>%
  addCircles(
    lng = ~city_longitude, lat = ~city_latitude, 
    radius = 500, # Adjust radius as needed
    fillColor = ~pal(Med_Rating), fillOpacity = 0.8, 
    color = "#BDBDC3", weight = 1,
    popup = labels
  ) %>%
  addLegend(
    "bottomright", pal = pal, values = ~Med_Rating,
    title = "Median Rating", opacity = 1
  )

# Display the map
m


# ------------------------- Graphical method: Clustered bar charts --------------------------

create_clustered_bar_plot <- function(data, group_var, color_var, color_scheme, plot_title) {
  
  # Grouping and summarizing the data
  clustered_data <- data %>%
    group_by(!!sym(group_var), !!sym(color_var)) %>%
    summarise(Count = n(), .groups = 'drop')
  
  # Plotting the clustered bar chart
  plot_ly(clustered_data, 
          x = as.formula(paste0("~", group_var)), 
          y = ~Count, 
          color = as.formula(paste0("~", color_var)), 
          colors = color_scheme,
          type = 'bar') %>%
    layout(barmode = 'group',
           title = list(text = plot_title, x = 0.5),
           xaxis = list(title = group_var, titlefont = list(size = 14, face = 'bold'),
                        tickfont = list(size = 12, face = 'bold')),
           yaxis = list(title = "Count", titlefont = list(size = 14, face = 'bold'),
                        tickfont = list(size = 12, face = 'bold')),
           template = 'plotly_white')
}

#2.2 RatingCategory vs CuisineType
create_clustered_bar_plot(train, group_var = "RatingCategory", color_var = "CuisineType", 
                          color_scheme = c("orange", "gold", "black"), 
                          plot_title = "Clustered Bar Chart: Rating Category vs Cuisine Type")

#2.3 RatingCategory vs OnlineOrder
create_clustered_bar_plot(train, group_var = "RatingCategory", color_var = "OnlineOrder", 
                          color_scheme = c("chocolate", "gold"), 
                          plot_title = "Clustered Bar Chart: Rating Category vs Online Ordering Availability")

#2.4 RatingCategory vs TableBooking
create_clustered_bar_plot(train, group_var = "RatingCategory", color_var = "TableBooking", 
                          color_scheme = c("chocolate", "orange"), 
                          plot_title = "Clustered Bar Chart: Rating Category vs Table Booking Availability")


# ========================== Part 3: Quantitative vs. Quantitative ==========================

# ----------------------------- Graphical method: Scatter plots -----------------------------

plot_scatter_histogram <- function(data, x_var, y_var, x_label, color_scatter, color_hist) {
  
  # Create a temporary dataset with the log-transformed variable
  temp_data <- data %>%
    mutate(!!paste0(x_var, "_log") := log1p(.data[[x_var]]))
  
  # Correlation Calculation
  correlation <- cor(temp_data[[x_var]], temp_data[[y_var]], use = "complete.obs")
  correlation_log <- cor(temp_data[[paste0(x_var, "_log")]], temp_data[[y_var]], use = "complete.obs")
  
  # Scatter plot without Log-Transformed Variable
  scatter_plot_original <- plot_ly(temp_data, 
                                   x = ~get(x_var), 
                                   y = ~get(y_var), 
                                   type = 'scatter', 
                                   mode = 'markers', 
                                   marker = list(color = color_scatter, opacity = 0.5, size = 6)) %>%
    add_lines(x = ~get(x_var), 
              y = fitted(lm(get(y_var) ~ get(x_var), data = temp_data)), 
              line = list(color = 'black', width = 2), 
              name = 'Linear Fit') %>%
    layout(title = list(text = paste("Scatter Plot:", x_label, "vs", y_var, "(Corr =", 
                                     round(correlation, 2), ")"), x = 0.5),
           xaxis = list(title = x_label, titlefont = list(size = 14, face = 'bold'),
                        tickfont = list(size = 12, face = 'bold')),
           yaxis = list(title = y_var, titlefont = list(size = 14, face = 'bold'),
                        tickfont = list(size = 12, face = 'bold')),
           template = 'plotly_white')
  
  # Histograms in the same plot window (1 row, 2 columns)
  hist_original <- plot_ly(temp_data, 
                           x = ~get(x_var), 
                           type = 'histogram', 
                           nbinsx = 30, 
                           marker = list(color = color_hist, line = list(color = 'black', width = 1)), 
                           opacity = 0.7) %>%
    layout(title = list(text = paste("Histogram of", x_label), x = 0.5),
           xaxis = list(title = x_label, titlefont = list(size = 14, face = 'bold'),
                        tickfont = list(size = 12, face = 'bold')),
           yaxis = list(title = "Count", titlefont = list(size = 14, face = 'bold'),
                        tickfont = list(size = 12, face = 'bold')),
           template = 'plotly_white')
  
  hist_log <- plot_ly(temp_data, 
                      x = ~get(paste0(x_var, "_log")), 
                      type = 'histogram', 
                      nbinsx = 30, 
                      marker = list(color = 'red', line = list(color = 'black', width = 1)), 
                      opacity = 0.7) %>%
    layout(title = list(text = paste("Histogram of Log-Transformed", x_label), x = 0.5),
           xaxis = list(title = paste("Log(", x_label, ")", sep = ""), 
                        titlefont = list(size = 14, face = 'bold'),
                        tickfont = list(size = 12, face = 'bold')),
           yaxis = list(title = "Count", titlefont = list(size = 14, face = 'bold'),
                        tickfont = list(size = 12, face = 'bold')),
           template = 'plotly_white')
  
  combined_hist <- subplot(hist_original, hist_log, nrows = 1, shareY = TRUE, titleX = TRUE, titleY = TRUE) %>%
    layout(title = list(text = paste("Histograms of", x_label, "(Original and Log-Transformed)"), x = 0.5))
  
  # Scatter plot with Log-Transformed Variable
  scatter_plot_log <- plot_ly(temp_data, 
                              x = ~get(paste0(x_var, "_log")), 
                              y = ~get(y_var), 
                              type = 'scatter', 
                              mode = 'markers', 
                              marker = list(color = color_scatter, opacity = 0.5, size = 6)) %>%
    add_lines(x = ~get(paste0(x_var, "_log")), 
              y = fitted(lm(get(y_var) ~ get(paste0(x_var, "_log")), data = temp_data)), 
              line = list(color = 'black', width = 2), 
              name = 'Linear Fit') %>%
    layout(title = list(text = paste("Scatter Plot (Log):", x_label, "vs", y_var, 
                                     "(Corr =", round(correlation_log, 2), ")"), x = 0.5),
           xaxis = list(title = paste("Log(", x_label, ")", sep = ""), titlefont = list(size = 14, face = 'bold'),
                        tickfont = list(size = 12, face = 'bold')),
           yaxis = list(title = y_var, titlefont = list(size = 14, face = 'bold'),
                        tickfont = list(size = 12, face = 'bold')),
           template = 'plotly_white')
  
  list(scatter_plot_original = scatter_plot_original, combined_hist = combined_hist, 
       scatter_plot_log = scatter_plot_log)
}

#3.1 Rating vs AvgCostForTwo
plots_avg_cost <- plot_scatter_histogram(train, "AvgCostForTwo", "Rating", "Avg Cost for Two", "red", "orange")
# Part 1: 
plots_avg_cost$scatter_plot_original
plots_avg_cost$combined_hist
# Part 2:
plots_avg_cost$scatter_plot_log

#3.2 Rating vs Votes
plots_votes <- plot_scatter_histogram(train, "Votes", "Rating", "Votes", "goldenrod", "yellow")
# Part 1: 
plots_votes$scatter_plot_original
plots_votes$combined_hist
# Part 2:
plots_votes$scatter_plot_log


# ================================== Multivariate Analysis ==================================



# =========================== Part 1: Qualitative vs. Qualitative ===========================

# ------------------- Graphical method: Point-biserial correlation heatmap ------------------

# Compute point-biserial correlations without modifying train
cor_matrix <- matrix(NA, nrow = 3, ncol = 2)
rownames(cor_matrix) <- c("Rating", "AvgCostForTwo", "Votes")
colnames(cor_matrix) <- c("OnlineOrder", "TableBooking")

# Fill correlation matrix directly without modifying the dataset
cor_matrix["Rating", "OnlineOrder"] <- cor(train$Rating, as.numeric(train$OnlineOrder == "Yes"))
cor_matrix["AvgCostForTwo", "OnlineOrder"] <- cor(train$AvgCostForTwo, as.numeric(train$OnlineOrder == "Yes"))
cor_matrix["Votes", "OnlineOrder"] <- cor(train$Votes, as.numeric(train$OnlineOrder == "Yes"))

cor_matrix["Rating", "TableBooking"] <- cor(train$Rating, as.numeric(train$TableBooking == "Yes"))
cor_matrix["AvgCostForTwo", "TableBooking"] <- cor(train$AvgCostForTwo, as.numeric(train$TableBooking == "Yes"))
cor_matrix["Votes", "TableBooking"] <- cor(train$Votes, as.numeric(train$TableBooking == "Yes"))

# Plot heatmap using plotly
plot_ly(
  z = cor_matrix,
  x = colnames(cor_matrix),
  y = rownames(cor_matrix),
  type = "heatmap",
  colors = c("red", "darkorange", "orange", "goldenrod", "gold", "yellow"),
  text = round(cor_matrix, 2),  # Show correlation values in heatmap
  texttemplate = "%{text}",
  textfont = list(color = "black", size = 14),
  showscale = TRUE
) %>%
  layout(
    title = "Point-Biserial Correlation Heatmap",
    xaxis = list(title = "Dichotomous Variables"),
    yaxis = list(title = "Numerical Variables")
  )


# ========================== Part 2: Quantitative vs. Quantitative ==========================

# -------------------------- Graphical method: Correlation heatmap --------------------------

#1. Rating, AvgCostForTwo, and Votes
correlation_data <- train %>% select(Rating, AvgCostForTwo, Votes)
cor_matrix <- cor(correlation_data, use = "complete.obs")

plot_ly(x = colnames(cor_matrix), 
        y = rownames(cor_matrix), 
        z = cor_matrix, 
        type = "heatmap", 
        text = round(cor_matrix, 2),
        texttemplate = "%{text}",
        colors = colorRamp(c("chocolate", "goldenrod1")),
        colorbar = list(title = "Correlation")) %>%
  layout(title = list(text = "Correlation Heatmap", x = 0.5),
         xaxis = list(title = "", tickfont = list(size = 12, face = 'bold')),
         yaxis = list(title = "", tickfont = list(size = 12, face = 'bold')),
         template = 'plotly_white')


# =============================== Part 3: Ordinal vs. Nominal ===============================

# --------------------------- Graphical method: Cramér's V heatmap --------------------------

data_subset <- train[, c('RatingCategory', 'Cuisines', 'OnlineOrder', 'TableBooking', 'MealType', 'City', 'CuisineType')]

# Function to calculate Cramér's V
cramers_v <- function(x, y) {
  tbl <- table(x, y)
  chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE)) # Correct=FALSE is often recommended
  n <- sum(tbl)
  min_dim <- min(nrow(tbl) - 1, ncol(tbl) - 1)
  if (min_dim == 0) return(0) # Handle cases where min_dim is 0 to avoid errors
  sqrt(chi2$statistic / (n * min_dim))
}

variables <- c('Cuisines', 'OnlineOrder', 'TableBooking', 'MealType', 'City', 'CuisineType')
cramers_v_results <- numeric(length(variables))
names(cramers_v_results) <- variables

for (i in seq_along(variables)) {
  var <- variables[i]
  cramers_v_results[i] <- cramers_v(data_subset$RatingCategory, data_subset[[var]])
}

# Convert results to a matrix for heatmap
cramers_v_matrix <- matrix(cramers_v_results, nrow = 1)
colnames(cramers_v_matrix) <- variables
rownames(cramers_v_matrix) <- 'RatingCategory'

plot_ly(
  z = cramers_v_matrix,
  x = colnames(cramers_v_matrix),
  y = rownames(cramers_v_matrix),
  type = "heatmap",
  colors = c("red", "darkorange", "orange", "goldenrod", "gold", "yellow"),
  text = round(cramers_v_matrix, 2),
  texttemplate = "%{text}",
  textfont = list(color = "black", size = 14),
  hoverinfo = 'text',
  showscale = TRUE
) %>%
  layout(
    title = "Cramér's V Heatmap",
    xaxis = list(title = "Categorical Variables"),
    yaxis = list(title = "Rating Category")
  )


# ----------------------------- Graphical method: Mosaic plots -----------------------------

# Visualize detailed associations between 'RatingCategory' and 'OnlineOrder', 'TableBooking', 
# 'MealType', 'CuisineType'

mosaic_vars <- c('OnlineOrder', 'TableBooking', 'MealType', 'CuisineType')

for (var in mosaic_vars) {
  data_subset[[var]] <- factor(data_subset[[var]])  # Force to factor
  mosaicplot(table(data_subset$RatingCategory, data_subset[[var]]), 
             main = paste("Mosaic Plot of RatingCategory vs", var),
             color = c("red", "darkorange", "orange", "goldenrod", "gold", "yellow"),  
             las = 1)     
}
par(mfrow = c(1, 1))


# ============================= Part 4: Ordinal vs. Ordinal ===============================

# ------------------ Graphical method: Spearman rank correlation heatmap ------------------

# Convert categorical variables to ordinal ranks
temp_ranks <- data.frame(
  Rating_Category = as.numeric(factor(train$RatingCategory,
                                      levels = c("Poor", "Average", "Good", "Excellent"),
                                      ordered = TRUE)),
  Cost_Category = as.numeric(factor(train$CostCategory, 
                                    levels = c("Cheap", "Reasonable", "Affordable", "Expensive"), 
                                    ordered = TRUE)),
  Vote_Category = as.numeric(factor(train$VoteCategory, 
                                    levels = c("Low Popularity", "Moderate Popularity", "Popular", "Highly Popular"), 
                                    ordered = TRUE))
)

cor_matrix <- cor(temp_ranks, method = "spearman")

plot_ly(
  z = cor_matrix, x = colnames(cor_matrix), y = rownames(cor_matrix), 
  type = "heatmap", colors = c("red", "darkorange", "orange", "goldenrod", "gold", "yellow"), 
  text = round(cor_matrix, 2), texttemplate = "%{text}", textfont = list(color = "black", size = 14), 
  showscale = TRUE
) %>% layout(title = "Spearman Rank Correlation Heatmap", xaxis = list(title = "Variables"),
             yaxis = list(title = "Variables"))


# ----------------------- Graphical method: Goodman-Kruskal's gamma -----------------------

vars_list <- list(RatingCategory = factor(train$RatingCategory, 
                                          levels = c("Poor", "Average", "Good", "Excellent"), 
                                          ordered = TRUE),
                  CostCategory = factor(train$CostCategory, 
                                        levels = c("Cheap", "Reasonable", "Affordable", "Expensive"), 
                                        ordered = TRUE),
                  VoteCategory = factor(train$VoteCategory, 
                                        levels = c("Low Popularity", "Moderate Popularity", "Popular", "Highly Popular"),
                                        ordered = TRUE))

gamma_matrix <- outer(names(vars_list), names(vars_list), 
                      Vectorize(function(x, y) GoodmanKruskalGamma(table(vars_list[[x]], vars_list[[y]]))[1]))

dimnames(gamma_matrix) <- list(names(vars_list), names(vars_list))

plot_ly(
  z = gamma_matrix, x = colnames(gamma_matrix), y = rownames(gamma_matrix), 
  type = "heatmap", colors = c("red", "darkorange", "orange", "goldenrod", "gold", "yellow"), 
  text = round(gamma_matrix, 2), texttemplate = "%{text}", textfont = list(color = "black", size = 14), 
  showscale = TRUE
) %>% layout(title = "Goodman-Kruskal's Gamma Heatmap", xaxis = list(title = "Variables"), 
             yaxis = list(title = "Variables"))


# ================================== Detecting outliers ===================================

# ------------------------------ Method: Isolation Forest ---------------------------------

data_subset <- train[, c("AvgCostForTwo", "Rating", "Votes")]
iso_forest <- isolation.forest(data_subset, ntrees = 100)

# Compute anomaly scores
anomaly_scores <- predict(iso_forest, data_subset, type = "score")

# Define threshold (top 5% most anomalous points)
threshold <- quantile(anomaly_scores, 0.95)

# Identify outliers (scores above the threshold)
outliers <- anomaly_scores > threshold

# Count total outliers
total_outliers <- sum(outliers)
percentage_outliers <- (total_outliers / nrow(train)) * 100

# Print results
print(paste("Total number of outliers detected:", total_outliers))
print(paste("Percentage of outliers:", round(percentage_outliers, 2), "%"))

# Create 3D plot with annotations
plot_ly(train, x = ~AvgCostForTwo, y = ~Votes, z = ~Rating, 
        color = ~ifelse(outliers, "Outlier", "Normal"), colors = c("black", "red"),
        type = "scatter3d", mode = "markers") %>%
  layout(title = "3D Outlier Detection using Isolation Forest",
         scene = list(xaxis = list(title = "Avg Cost for Two"),
                      yaxis = list(title = "Votes"),
                      zaxis = list(title = "Rating")),
         annotations = list(
           list(x = 1.05, y = 1, z = 0, 
                text = paste("Total Outliers:", total_outliers, "<br>",
                             "Percentage:", round(percentage_outliers, 2), "%"),
                showarrow = FALSE, 
                xref = "paper", yref = "paper", zref = "paper",
                font = list(size = 14, color = "blue"))
         )
  )



#############################################################################################
#                           FACTOR ANALYSIS OF MIXED DATA (FAMD)                            #
#############################################################################################
library(FactoMineR)
library(factoextra)


# Part 1:
# ============================= Feature engineering: Part 5 =================================

# Categorize cuisines into broader groups
categorize_cuisine <- function(cuisine) {
  asian_cuisines <- c('Asian', 'Burmese', 'Cantonese', 'Chinese', 'Indonesian', 'Japanese', 
                      'Korean', 'Malaysian', 'Mongolian', 'Pan Asian', 'Singaporean', 
                      'Thai', 'Tibetan', 'Vietnamese')
  
  indian_cuisines <- c('Andhra', 'Assamese', 'Awadhi', 'Bengali', 'Bihari', 'Bohri', 
                       'Chettinad', 'Goan', 'Gujarati', 'Hyderabadi', 'Kashmiri', 'Kerala', 
                       'Konkan', 'Lucknowi', 'Maharashtrian', 'Mangalorean', 'Mughlai', 
                       'Naga', 'North Eastern', 'North Indian', 'Oriya', 'Rajasthani', 
                       'Sindhi', 'South Indian', 'Tamil')
  
  european_cuisines <- c('Belgian', 'British', 'Continental', 'European', 'French', 'German', 
                         'Greek', 'Italian', 'Portuguese', 'Russian', 'Spanish')
  
  american_cuisines <- c('African', 'American', 'Australian', 'BBQ', 'Fast Food', 'Hot dogs', 
                         'Mexican', 'South American') 
  
  middle_eastern_cuisines <- c('Arabian', 'Iranian', 'Lebanese', 'Middle Eastern', 'Turkish')
  
  fusion_modern_cuisines <- c('Modern Indian', 'Vegan', 'Healthy Food')
  
  if (any(str_detect(cuisine, paste(asian_cuisines, collapse = '|')))) return('Asian')
  if (any(str_detect(cuisine, paste(indian_cuisines, collapse = '|')))) return('IndianRegional')
  if (any(str_detect(cuisine, paste(european_cuisines, collapse = '|')))) return('European')
  if (any(str_detect(cuisine, paste(american_cuisines, collapse = '|')))) return('American/Western')
  if (any(str_detect(cuisine, paste(middle_eastern_cuisines, collapse = '|')))) return('MiddleEastern')
  if (any(str_detect(cuisine, paste(fusion_modern_cuisines, collapse = '|')))) return('Fusion/Modern')  
  return('Other/Exotic')
}

train$CuisineOrigin <- sapply(train$Cuisines, categorize_cuisine)
test$CuisineOrigin <- sapply(test$Cuisines, categorize_cuisine)


# Part 2:
# ============================ Cleaning 'RestaurantType' column =============================

# Frequency table of 'RestaurantType''
rest_freq <- tibble(
  RestaurantType = c("Quick Bites", "Casual Dining", "Cafe", "Other", "Delivery",
                     "Dessert Parlor", "Bar", "Bakery", "Takeaway", "Beverage Shop"),
  n = c(15248, 12283, 4649, 4100, 2985, 2711, 2312, 1395, 1378, 1136)
)

# Convert frequency table to a lookup list
rest_freq_list <- setNames(rest_freq$n, rest_freq$RestaurantType)

# Function to get the most frequent type
get_most_frequent_type <- function(types) {
  types <- unlist(strsplit(types, ", "))  # Split multiple types
  types <- types[types %in% names(rest_freq_list)]  # Keep only valid types
  if (length(types) == 0) return(NA)  # Handle empty cases
  return(types[which.max(rest_freq_list[types])])  # Return the most frequent type
}

# Apply transformation to the 'RestaurantType' column in 'Training' set
train <- train %>%
  mutate(RestaurantType = sapply(RestaurantType, get_most_frequent_type))


# Apply transformation to the 'RestaurantType' column in 'Test' set
test <- test %>%
  mutate(RestaurantType = sapply(RestaurantType, get_most_frequent_type))


# Part 3: 

# Convert categorical variables to factors in 'Training' set
train$OnlineOrder <- as.factor(train$OnlineOrder)
train$TableBooking <- as.factor(train$TableBooking)
train$RestaurantType <- as.factor(train$RestaurantType)
train$MealType <- as.factor(train$MealType)
train$City <- as.factor(train$City)
train$CuisineOrigin <- as.factor(train$CuisineOrigin)

# Convert categorical variables to factors in 'Test' set
test$OnlineOrder <- as.factor(test$OnlineOrder)
test$TableBooking <- as.factor(test$TableBooking)
test$RestaurantType <- as.factor(test$RestaurantType)
test$MealType <- as.factor(test$MealType)
test$City <- as.factor(test$City)
test$CuisineOrigin <- as.factor(test$CuisineOrigin)

# Drop Unnecessary Columns in 'Training' set
train_expanded <- subset(train, select = -c(Cuisines, VoteCategory, RatingCategory, CostCategory, CuisineType))
View(train_expanded)

# Drop Unnecessary Columns in 'Test' set
test_expanded <- subset(test, select = -c(Cuisines, VoteCategory, RatingCategory, CostCategory, CuisineType))
View(test_expanded)

# Consider 'Training' set
# Modify factor levels to ensure uniqueness
train_expanded$OnlineOrder <- factor(train_expanded$OnlineOrder, labels = c("OnlineOrder_No", "OnlineOrder_Yes"))
train_expanded$TableBooking <- factor(train_expanded$TableBooking, labels = c("TableBooking_No", "TableBooking_Yes"))

# Convert factor to character to allow direct modification
train_expanded$RestaurantType <- as.character(train_expanded$RestaurantType)

# Replace "Delivery" with "Delivering" in 'RestaurantType' column
train_expanded$RestaurantType[train_expanded$RestaurantType == "Delivery"] <- "Delivering"

# Convert back to factor
train_expanded$RestaurantType <- as.factor(train_expanded$RestaurantType)


# Consider 'Test' set
# Modify factor levels to ensure uniqueness
test_expanded$OnlineOrder <- factor(test_expanded$OnlineOrder, labels = c("OnlineOrder_No", "OnlineOrder_Yes"))
test_expanded$TableBooking <- factor(test_expanded$TableBooking, labels = c("TableBooking_No", "TableBooking_Yes"))

# Convert factor to character to allow direct modification
test_expanded$RestaurantType <- as.character(test_expanded$RestaurantType)

# Replace "Delivery" with "Delivering" in 'RestaurantType' column
test_expanded$RestaurantType[test_expanded$RestaurantType == "Delivery"] <- "Delivering"

# Convert back to factor
test_expanded$RestaurantType <- as.factor(test_expanded$RestaurantType)


# Part 4:
# ============================ Visualization and interpretation =============================

res.famd <- FAMD(train_expanded, graph = FALSE)
print(res.famd)

# Eigenvalues / Variances
eig.val <- get_eigenvalue(res.famd)
head(eig.val)

# Scree plot 
fviz_screeplot(res.famd)

# ---------------------------------- Graph of variables -------------------------------------

# Part 3.1: All variables 
var <- get_famd_var(res.famd)
var

# Coordinates of variables
head(var$coord)
# Cos2: quality of representation on the factor map
head(var$cos2)
# Contributions to the  dimensions
head(var$contrib)

# Plot of all variables
fviz_famd_var(res.famd, repel = TRUE)
# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)


# Part 3.2: Quantitative variables
quanti.var <- get_famd_var(res.famd, "quanti.var")
quanti.var 

fviz_famd_var(res.famd, "quanti.var", repel = TRUE,
              col.var = "black")

fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

# Color by cos2 values: quality on the factor map
fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)


# Part 3.3: Qualitative variables
quali.var <- get_famd_var(res.famd, "quali.var")
quali.var

fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


loadings <- as.data.frame(res.famd$var$coord)

# Part 3.4: Print loadings to check
print(loadings)

fviz_pca_var(res.famd, repel = TRUE, col.var = "red") +
  ggtitle("Loading Plot (Correlation Circle)")



#############################################################################################
#                                      CLUSTERING                                           #
#############################################################################################

famd_scores <- as.data.frame(res.famd$ind$coord[, 1:2])  # Extract individual coordinates

# Compute within-cluster sum of squares for different k values
fviz_nbclust(famd_scores, kmeans, method = "wss") + 
  ggtitle("Elbow Method for Optimal Clusters")

# Alternative: Silhouette method
fviz_nbclust(famd_scores, kmeans, method = "silhouette") +
  ggtitle("Silhouette Method for Optimal Clusters")

set.seed(123)  # For reproducibility
k <- 3  # Replace with the chosen number of clusters

km_res <- kmeans(famd_scores, centers = k, nstart = 25)

# Add cluster labels to original data
famd_scores$Cluster <- as.factor(km_res$cluster)

# Convert Cluster column back to numeric
famd_scores$Cluster <- as.numeric(famd_scores$Cluster)

# Now, try visualizing again
fviz_cluster(km_res, data = famd_scores[, -ncol(famd_scores)], 
             geom = "point",
             ellipse = FALSE,   # Disable cluster ellipses 
             ggtheme = theme_minimal())


# Add cluster labels to the original data
train_expanded$Cluster <- as.factor(km_res$cluster)


# ---------------------------------- Graph of variables -------------------------------------

# Boxplots
ggplot(train_expanded, aes(x = Cluster, y = Rating, fill = Cluster)) +
  geom_boxplot() +
  ggtitle("Rating Distribution by Cluster")

ggplot(train_expanded, aes(x = Cluster, y = AvgCostForTwo, fill = Cluster)) +
  geom_boxplot() +
  ggtitle("Cost Distribution by Cluster")

ggplot(train_expanded, aes(x = Cluster, y = Votes, fill = Cluster)) +
  geom_boxplot() +
  ggtitle("Voting Distribution by Cluster")

# Bar plots
ggplot(train_expanded, aes(x = CuisineOrigin, fill = Cluster)) +
  geom_bar(position = "dodge") +
  ggtitle("CuisineOrigin Distribution by Cluster")

ggplot(train_expanded, aes(x = TableBooking, fill = Cluster)) +
  geom_bar(position = "dodge") +
  ggtitle("Table Booking Availability by Cluster")

ggplot(train_expanded, aes(x = OnlineOrder, fill = Cluster)) +
  geom_bar(position = "dodge") +
  ggtitle("Online Presence by Cluster")

ggplot(train_expanded, aes(x = MealType, fill = Cluster)) +
  geom_bar(position = "dodge") +
  ggtitle("Meal Types by Cluster")

ggplot(train_expanded, aes(x = RestaurantType, fill = Cluster)) +
  geom_bar(position = "dodge") +
  ggtitle("Restaurant Types by Cluster")

ggplot(train_expanded, aes(x = City, fill = Cluster)) +
  geom_bar(position = "dodge") +
  ggtitle("City by Cluster")


# ------------------------------- Clustering the 'Test' set ---------------------------------

# Step 1: Project the test set onto FAMD dimensions
train_famd_scores <- as.data.frame(res.famd$ind$coord[, 1:2])  
colnames(train_famd_scores) <- c("FAMD1", "FAMD2")  # Rename for clarity

# Step 2: Assign clusters using the trained k-means model
test_famd <- predict(res.famd, newdata = test_expanded)
test_scores <- as.data.frame(test_famd$coord[, 1:2])
colnames(test_scores) <- c("FAMD1", "FAMD2")  # Keep naming consistent

test_clusters <- apply(test_scores, 1, function(row) {
  which.min(colSums((t(km_res$centers) - row)^2))  # Find nearest cluster center
})

# Step 3: Add cluster labels to test data
test_expanded$Cluster <- as.factor(test_clusters)

write.csv(train_expanded, "train_expanded.csv", row.names = FALSE)
write.csv(test_expanded, "test_expanded.csv", row.names = FALSE)



#############################################################################################
#                                    ADVANCED ANALYSIS                                      #
#############################################################################################

# Part 1: Divide Training Data into Subsets (With & Without Outliers)

train_with_outliers <- train_expanded
train_without_outliers <- train_expanded %>% filter(!outliers)  # Outliers removed

View(train_with_outliers)
View(train_without_outliers)

#############################################################################################
#                             MULTIPLE LINEAR REGRESSION (MLR)                              #
#############################################################################################

library(tidyverse)    
library(car)          
library(MASS)         
library(stats)        
library(ggplot2)      
library(gridExtra)    
library(dplyr)        
library(broom)       

# Function to perform comprehensive MLR analysis and predictions
perform_mlr_analysis <- function(train_data, test_data, cluster_num) {
  
  # Filter training data for specific cluster and remove Cluster column
  cluster_train <- train_data %>% 
    filter(Cluster == cluster_num) %>%
    dplyr::select(-Cluster)
  
  # Filter test data and prepare it properly
  cluster_test <- test_data %>% 
    filter(Cluster == cluster_num) %>%
    dplyr::select(-Cluster)
  
  # Store actual test ratings separately and remove from test data
  actual_test_ratings <- cluster_test$Rating
  cluster_test_predictors <- cluster_test %>% dplyr::select(-Rating)
  
  # Prepare full model formula
  full_model <- lm(Rating ~ ., data = cluster_train)
  
  # ----------------------------- Part 1: Forward Selection ---------------------------------
  forward_model <- step(lm(Rating ~ 1, data = cluster_train),
                        scope = list(lower = ~1, upper = formula(full_model)),
                        direction = "forward",
                        trace = 0)
  
  # ---------------------------- Part 2: Backward Elimination ------------------------------- 
  backward_model <- step(full_model, 
                         direction = "backward",
                         trace = 0)
  
  # ------------------------ Part 3: Predict 'Rating' on Test Data --------------------------
  predictions <- list(
    forward = predict(forward_model, newdata = cluster_test_predictors),
    backward = predict(backward_model, newdata = cluster_test_predictors)
  )
  
  # ------------------- Part 4: Compare Model Performance (RMSE, MAE, R²) -------------------
  get_prediction_metrics <- function(pred, actual) {
    rmse <- sqrt(mean((pred - actual)^2, na.rm = TRUE))
    mae <- mean(abs(pred - actual), na.rm = TRUE)
    r2 <- cor(pred, actual)^2
    
    return(list(RMSE = rmse, MAE = mae, R2 = r2))
  }
  
  prediction_metrics <- list(
    forward = get_prediction_metrics(predictions$forward, actual_test_ratings),
    backward = get_prediction_metrics(predictions$backward, actual_test_ratings)
  )
  
  # Model analysis
  analyze_model <- function(model) {
    metrics <- list(
      R2 = summary(model)$r.squared,
      Adj_R2 = summary(model)$adj.r.squared,
      RMSE = sqrt(mean(model$residuals^2)),
      MAE = mean(abs(model$residuals))
    )
    
    # Calculate VIF if there are multiple predictors
    if (length(coef(model)) > 2) {
      vif_values <- try(car::vif(model), silent = TRUE)
      if (!inherits(vif_values, "try-error")) {
        metrics$mean_vif <- mean(vif_values)
      } else {
        metrics$mean_vif <- NA
      }
    } else {
      metrics$mean_vif <- 1
    }
    
    return(metrics)
  }
  
  forward_results <- analyze_model(forward_model)
  backward_results <- analyze_model(backward_model)
  
  return(list(
    training_metrics = list(
      forward = forward_results,
      backward = backward_results
    ),
    predictions = predictions,
    prediction_metrics = prediction_metrics,
    selected_variables = list(
      forward = names(coef(forward_model)),
      backward = names(coef(backward_model))
    )
  ))
}

# Print function with clean tables
print_prediction_results <- function(with_outliers, without_outliers, cluster_num) {
  # Helper function to print metrics table
  print_metrics_table <- function(results, outlier_status) {
    cat(sprintf("\n%s Outliers:\n", outlier_status))
    cat("-------------------------------------------------------\n")
    
    # Training metrics
    cat("\nTraining Metrics:\n")
    cat("-------------------------------------------------------\n")
    cat(sprintf("%-20s %-15s %-15s\n", "Metric", "Forward", "Backward"))
    cat("-------------------------------------------------------\n")
    
    train_metrics <- c("R2", "Adj_R2", "RMSE", "MAE", "mean_vif")
    for (metric in train_metrics) {
      cat(sprintf("%-20s %-15.4f %-15.4f\n",
                  metric,
                  results$training_metrics$forward[[metric]],
                  results$training_metrics$backward[[metric]]))
    }
    
    # Test metrics
    cat("\nTest Metrics:\n")
    cat("-------------------------------------------------------\n")
    cat(sprintf("%-20s %-15s %-15s\n", "Metric", "Forward", "Backward"))
    cat("-------------------------------------------------------\n")
    
    test_metrics <- c("RMSE", "MAE", "R2")
    for (metric in test_metrics) {
      cat(sprintf("%-20s %-15.4f %-15.4f\n",
                  metric,
                  results$prediction_metrics$forward[[metric]],
                  results$prediction_metrics$backward[[metric]]))
    }
    
    # Selected variables
    cat("\nSelected Variables:\n")
    cat("-------------------------------------------------------\n")
    
    # Print forward selection variables
    cat("\nForward Selection:\n")
    if (length(results$selected_variables$forward) > 0) {
      for (i in seq_along(results$selected_variables$forward)) {
        cat(sprintf("%d. %s\n", i, results$selected_variables$forward[i]))
      }
    } else {
      cat("None\n")
    }
    
    # Print backward elimination variables
    cat("\nBackward Elimination:\n")
    if (length(results$selected_variables$backward) > 0) {
      for (i in seq_along(results$selected_variables$backward)) {
        cat(sprintf("%d. %s\n", i, results$selected_variables$backward[i]))
      }
    } else {
      cat("None\n")
    }
    
    cat("\n=======================================================\n")
  }
  
  # Print header
  cat("\n=======================================================")
  cat(sprintf("\nCluster %d - Model Comparison\n", cluster_num))
  cat("=======================================================\n")
  
  # Print tables for with outliers
  print_metrics_table(with_outliers, "With")
  
  # Print tables for without outliers
  print_metrics_table(without_outliers, "Without")
}

# Apply analysis to each cluster for both with and without outliers
clusters <- 1:3

# Results with outliers
results_with_outliers <- lapply(clusters, function(i) {
  perform_mlr_analysis(train_with_outliers, test_expanded, i)
})

# Results without outliers
results_without_outliers <- lapply(clusters, function(i) {
  perform_mlr_analysis(train_without_outliers, test_expanded, i)
})

# 1st cluster
print_prediction_results(results_with_outliers[[1]], results_without_outliers[[1]], 1)

# 2nd cluster
print_prediction_results(results_with_outliers[[2]], results_without_outliers[[2]], 2)

# 3rd cluster
print_prediction_results(results_with_outliers[[3]], results_without_outliers[[3]], 3)



#############################################################################################
#                                   SHRINKAGE METHODS                                       #
#############################################################################################


# ======================================= Lasso =============================================
library(glmnet)
library(dplyr)
library(caret)
library(ggplot2)
library(tidyr)

# Function to calculate RMSE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Function to calculate R-squared
r_squared <- function(actual, predicted) {
  1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
}

# Function to perform Lasso regression and return RMSE and R-squared
perform_lasso <- function(train_data, test_data, response_var, cluster_var) {
  results <- list()
  
  for (cluster in unique(train_data[[cluster_var]])) {
    # Filter data for the current cluster
    train_cluster <- train_data %>% filter(!!sym(cluster_var) == cluster)
    test_cluster <- test_data %>% filter(!!sym(cluster_var) == cluster)
    
    # Prepare data for glmnet
    x_train <- model.matrix(~ . - 1, data = train_cluster %>% dplyr::select(-all_of(response_var)))
    y_train <- train_cluster[[response_var]]
    
    x_test <- model.matrix(~ . - 1, data = test_cluster %>% dplyr::select(-all_of(response_var)))
    y_test <- test_cluster[[response_var]]
    
    # Perform Lasso regression with cross-validation
    cv_model <- cv.glmnet(x_train, y_train, alpha = 1)
    best_lambda <- cv_model$lambda.min
    lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = cv_model$lambda)
    
    # Predict on training and test data
    train_pred <- predict(lasso_model, s = best_lambda, newx = x_train)
    test_pred <- predict(lasso_model, s = best_lambda, newx = x_test)
    
    # Calculate RMSE and R-squared for training and test sets
    train_rmse <- rmse(y_train, train_pred)
    train_r2 <- r_squared(y_train, train_pred)
    
    test_rmse <- rmse(y_test, test_pred)
    test_r2 <- r_squared(y_test, test_pred)
    
    # Store results
    results[[as.character(cluster)]] <- list(
      best_lambda = best_lambda,
      train_rmse = train_rmse,
      train_r2 = train_r2,
      test_rmse = test_rmse,
      test_r2 = test_r2
    )
    
    # Plot 1: Log Lambda vs Coefficients
    # Extract coefficients for all lambda values
    coeff_plot_data <- as.matrix(coef(lasso_model))
    coeff_plot_data <- as.data.frame(t(coeff_plot_data))
    coeff_plot_data$lambda <- log(lasso_model$lambda)
    coeff_plot_data <- tidyr::gather(coeff_plot_data, key = "variable", value = "coefficient", -lambda)
    
    p1 <- ggplot(coeff_plot_data, aes(x = lambda, y = coefficient, color = variable)) +
      geom_line() +
      geom_vline(xintercept = log(best_lambda), linetype = "dashed", color = "red") +
      labs(title = paste("Log Lambda vs Coefficients for Cluster", cluster),
           x = "Log Lambda",
           y = "Coefficient Value") +
      theme_minimal() +
      theme(legend.position = "none")
    
    print(p1)
    
    # Plot 2: Log Lambda vs MSE (Cross-Validation)
    mse_plot_data <- data.frame(
      log_lambda = log(cv_model$lambda),
      mse = cv_model$cvm,
      mse_upper = cv_model$cvup,
      mse_lower = cv_model$cvlo
    )
    
    p2 <- ggplot(mse_plot_data, aes(x = log_lambda, y = mse)) +
      geom_line(color = "blue") +
      geom_ribbon(aes(ymin = mse_lower, ymax = mse_upper), alpha = 0.2) +
      geom_vline(xintercept = log(best_lambda), linetype = "dashed", color = "red") +
      labs(title = paste("Log Lambda vs MSE for Cluster", cluster),
           x = "Log Lambda",
           y = "Mean Squared Error (MSE)") +
      theme_minimal()
    
    print(p2)
  }
  
  return(results)
}

# Apply the function to both training datasets (with and without outliers)
# and evaluate on the same test set (test_expanded)

# Case 1: Train on train_with_outliers and evaluate on test_expanded
results_with_outliers <- perform_lasso(train_with_outliers, test_expanded, "Rating", "Cluster")

# Case 2: Train on train_without_outliers and evaluate on test_expanded
results_without_outliers <- perform_lasso(train_without_outliers, test_expanded, "Rating", "Cluster")

# Create a table for training and test set results
combined_results <- data.frame(
  Cluster = integer(),
  Data_Set = character(),
  Best_Lambda = numeric(),
  Train_RMSE = numeric(),
  Train_R_Squared = numeric(),
  Test_RMSE = numeric(),
  Test_R_Squared = numeric(),
  stringsAsFactors = FALSE
)

# Sort clusters in ascending order (1, 2, 3)
sorted_clusters <- sort(as.numeric(names(results_with_outliers)))

# Populate the table with results in the correct order
for (cluster in sorted_clusters) {
  combined_results <- rbind(combined_results, data.frame(
    Cluster = cluster,
    Data_Set = "With Outliers",
    Best_Lambda = results_with_outliers[[as.character(cluster)]]$best_lambda,
    Train_RMSE = results_with_outliers[[as.character(cluster)]]$train_rmse,
    Train_R_Squared = results_with_outliers[[as.character(cluster)]]$train_r2,
    Test_RMSE = results_with_outliers[[as.character(cluster)]]$test_rmse,
    Test_R_Squared = results_with_outliers[[as.character(cluster)]]$test_r2
  ))
  
  combined_results <- rbind(combined_results, data.frame(
    Cluster = cluster,
    Data_Set = "Without Outliers",
    Best_Lambda = results_without_outliers[[as.character(cluster)]]$best_lambda,
    Train_RMSE = results_without_outliers[[as.character(cluster)]]$train_rmse,
    Train_R_Squared = results_without_outliers[[as.character(cluster)]]$train_r2,
    Test_RMSE = results_without_outliers[[as.character(cluster)]]$test_rmse,
    Test_R_Squared = results_without_outliers[[as.character(cluster)]]$test_r2
  ))
}

View(combined_results)

# ==================================== Elastic Net ==========================================
library(glmnet)
library(dplyr)
library(caret)
library(ggplot2)

# Function to calculate RMSE
rmse_EN <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Function to calculate R-squared
r_squared_EN <- function(actual, predicted) {
  1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
}

# Function to perform Elastic Net regression and return RMSE, R-squared, and plots
perform_elastic_net_EN <- function(train_data, test_data, response_var, cluster_var, alpha = 0.5) {
  results_EN <- list()
  
  for (cluster in unique(train_data[[cluster_var]])) {
    # Filter data for the current cluster
    train_cluster_EN <- train_data %>% filter(!!sym(cluster_var) == cluster)
    test_cluster_EN <- test_data %>% filter(!!sym(cluster_var) == cluster)
    
    # Prepare data for glmnet
    x_train_EN <- model.matrix(~ . - 1, data = train_cluster_EN %>% dplyr::select(-all_of(response_var)))
    y_train_EN <- train_cluster_EN[[response_var]]
    
    x_test_EN <- model.matrix(~ . - 1, data = test_cluster_EN %>% dplyr::select(-all_of(response_var)))
    y_test_EN <- test_cluster_EN[[response_var]]
    
    # Perform cross-validated Elastic Net regression
    cv_model_EN <- cv.glmnet(x_train_EN, y_train_EN, alpha = alpha, nfolds = 10)  # 10-fold cross-validation
    best_lambda_EN <- cv_model_EN$lambda.min
    
    # Fit the final Elastic Net model
    elastic_net_model_EN <- glmnet(x_train_EN, y_train_EN, alpha = alpha, lambda = best_lambda_EN)
    
    # Predict on training and test data
    train_pred_EN <- predict(elastic_net_model_EN, s = best_lambda_EN, newx = x_train_EN)
    test_pred_EN <- predict(elastic_net_model_EN, s = best_lambda_EN, newx = x_test_EN)
    
    # Calculate RMSE and R-squared for training and test sets
    train_rmse_EN <- rmse_EN(y_train_EN, train_pred_EN)
    train_r2_EN <- r_squared_EN(y_train_EN, train_pred_EN)
    
    test_rmse_EN <- rmse_EN(y_test_EN, test_pred_EN)
    test_r2_EN <- r_squared_EN(y_test_EN, test_pred_EN)
    
    # Store results
    results_EN[[as.character(cluster)]] <- list(
      best_lambda_EN = best_lambda_EN,
      train_rmse_EN = train_rmse_EN,
      train_r2_EN = train_r2_EN,
      test_rmse_EN = test_rmse_EN,
      test_r2_EN = test_r2_EN,
      cv_model_EN = cv_model_EN,
      elastic_net_model_EN = elastic_net_model_EN
    )
    
    # Plot 1: Log Lambda vs Coefficients
    plot(elastic_net_model_EN, xvar = "lambda", label = TRUE, main = paste("Cluster", cluster, "- Log Lambda vs Coefficients"))
    
    # Plot 2: Log Lambda vs MSE (from cross-validation)
    plot(cv_model_EN, main = paste("Cluster", cluster, "- Log Lambda vs MSE"))
  }
  
  return(results_EN)
}

# Apply the function to both training datasets (with and without outliers)
# and evaluate on the same test set (test_expanded)

# Case 1: Train on train_with_outliers and evaluate on test_expanded
results_with_outliers_EN <- perform_elastic_net_EN(train_with_outliers, test_expanded, "Rating", "Cluster", alpha = 0.5)

# Case 2: Train on train_without_outliers and evaluate on test_expanded
results_without_outliers_EN <- perform_elastic_net_EN(train_without_outliers, test_expanded, "Rating", "Cluster", alpha = 0.5)

# Create a table for training and test set results
combined_results_EN <- data.frame(
  Cluster = integer(),
  Data_Set = character(),
  Best_Lambda_EN = numeric(),
  Train_RMSE_EN = numeric(),
  Train_R_Squared_EN = numeric(),
  Test_RMSE_EN = numeric(),
  Test_R_Squared_EN = numeric(),
  stringsAsFactors = FALSE
)

# Sort clusters in ascending order (1, 2, 3)
sorted_clusters_EN <- sort(as.numeric(names(results_with_outliers_EN)))

# Populate the table with results in the correct order
for (cluster in sorted_clusters_EN) {
  combined_results_EN <- rbind(combined_results_EN, data.frame(
    Cluster = cluster,
    Data_Set = "With Outliers",
    Best_Lambda_EN = results_with_outliers_EN[[as.character(cluster)]]$best_lambda_EN, 
    Train_RMSE_EN = results_with_outliers_EN[[as.character(cluster)]]$train_rmse_EN,
    Train_R_Squared_EN = results_with_outliers_EN[[as.character(cluster)]]$train_r2_EN,
    Test_RMSE_EN = results_with_outliers_EN[[as.character(cluster)]]$test_rmse_EN,
    Test_R_Squared_EN = results_with_outliers_EN[[as.character(cluster)]]$test_r2_EN
  ))
  
  combined_results_EN <- rbind(combined_results_EN, data.frame(
    Cluster = cluster,
    Data_Set = "Without Outliers",
    Best_Lambda_EN = results_without_outliers_EN[[as.character(cluster)]]$best_lambda_EN,
    Train_RMSE_EN = results_without_outliers_EN[[as.character(cluster)]]$train_rmse_EN,
    Train_R_Squared_EN = results_without_outliers_EN[[as.character(cluster)]]$train_r2_EN,
    Test_RMSE_EN = results_without_outliers_EN[[as.character(cluster)]]$test_rmse_EN,
    Test_R_Squared_EN = results_without_outliers_EN[[as.character(cluster)]]$test_r2_EN
  ))
}

View(combined_results_EN)


#############################################################################################
#                                   TREE-BASED METHODS                                      #
#############################################################################################

# ================================== Regression Trees =======================================
library(rpart)       # For Decision Trees
library(dplyr)
library(Metrics)
library(parallel)    # For parallel processing
library(caret)       # For k-fold cross-validation

# -------- Part 1: Regression Tree Models for Each Cluster (With & Without Outliers) --------

# Define the hyperparameter grid for Decision Tree
hyperparameter_grid <- expand.grid(
  cp = c(0.01, 0.1),    # Complexity parameter for pruning
  minsplit = c(5, 10),  # Minimum number of observations in a node to split
  maxdepth = c(3, 6)    # Maximum depth of the tree
)

# Define k-Fold Cross-Validation Control
set.seed(123)
cv_control <- trainControl(
  method = "cv",        
  number = 10,           
  savePredictions = "final",
  allowParallel = TRUE  
)

# Function to train a Decision Tree model with k-Fold Cross-Validation
train_dt_model_cv <- function(data, params) {
  train(
    Rating ~ ., 
    data_subset <- dplyr::select(data, -Cluster),  # Exclude Cluster column
    method = "rpart",
    trControl = cv_control,
    tuneGrid = data.frame(cp = params$cp),  # Only cp is tunable in caret
    control = rpart.control(
      minsplit = params$minsplit,
      maxdepth = params$maxdepth
    )
  )
}

# Initialize model storage
dt_models <- list()

# Loop through each cluster
for (k in unique(train_expanded$Cluster)) {
  
  # Get data subsets
  cluster_with_outliers <- train_with_outliers %>% filter(Cluster == k)
  cluster_without_outliers <- train_without_outliers %>% filter(Cluster == k)
  
  # Convert hyperparameter grid to a list of parameter sets
  param_list <- split(hyperparameter_grid, seq(nrow(hyperparameter_grid)))
  
  # Set up a cluster for parallel processing
  num_cores <- detectCores() - 1  # Use all but one core
  cl <- makeCluster(num_cores)
  
  # Export necessary objects and functions to the cluster
  clusterExport(cl, c("train_dt_model_cv", "train", "cluster_with_outliers", "cluster_without_outliers", "cv_control"))
  
  # Load required libraries on the workers
  clusterEvalQ(cl, library(dplyr))
  clusterEvalQ(cl, library(caret))
  clusterEvalQ(cl, library(rpart))
  
  # Train models WITH outliers in parallel
  cat("Training Decision Tree for Cluster", k, "- With Outliers\n")
  dt_models_with_outliers <- parLapply(cl, param_list, train_dt_model_cv, data = cluster_with_outliers)
  
  # Evaluate models WITH outliers and select the best one
  tuning_results_with_outliers <- do.call(rbind, lapply(seq_along(dt_models_with_outliers), function(i) {
    params <- param_list[[i]]
    model <- dt_models_with_outliers[[i]]
    train_predictions <- predict(model, newdata = cluster_with_outliers)
    train_mae <- mae(cluster_with_outliers$Rating, train_predictions)
    data.frame(
      cp = params$cp,
      minsplit = params$minsplit,
      maxdepth = params$maxdepth,
      MAE = train_mae
    )
  }))
  
  best_with_outliers <- tuning_results_with_outliers %>%
    arrange(MAE) %>%
    head(1)
  
  # Train the final model WITH outliers using the best hyperparameters
  dt_with_outliers <- train_dt_model_cv(cluster_with_outliers, best_with_outliers)
  
  # Train models WITHOUT outliers in parallel
  cat("Training Decision Tree for Cluster", k, "- Without Outliers\n")
  dt_models_without_outliers <- parLapply(cl, param_list, train_dt_model_cv, data = cluster_without_outliers)
  
  # Evaluate models WITHOUT outliers and select the best one
  tuning_results_without_outliers <- do.call(rbind, lapply(seq_along(dt_models_without_outliers), function(i) {
    params <- param_list[[i]]
    model <- dt_models_without_outliers[[i]]
    train_predictions <- predict(model, newdata = cluster_without_outliers)
    train_mae <- mae(cluster_without_outliers$Rating, train_predictions)
    data.frame(
      cp = params$cp,
      minsplit = params$minsplit,
      maxdepth = params$maxdepth,
      MAE = train_mae
    )
  }))
  
  best_without_outliers <- tuning_results_without_outliers %>%
    arrange(MAE) %>%
    head(1)
  
  # Train the final model WITHOUT outliers using the best hyperparameters
  dt_without_outliers <- train_dt_model_cv(cluster_without_outliers, best_without_outliers)
  
  # Store models
  dt_models[[paste0("Cluster_", k, "_With_Outliers")]] <- dt_with_outliers
  dt_models[[paste0("Cluster_", k, "_Without_Outliers")]] <- dt_without_outliers
  
  # Stop the cluster
  stopCluster(cl)
}

# ------------------------- Part 2: Predict 'Rating' on Test Data ---------------------------

# Exclude the 'Rating' column from the test data
test_data_for_prediction_RT <- test_expanded %>% dplyr::select(-Rating)

# Initialize storage for predictions
test_data_for_prediction_RT$Predicted_Rating_With_Outliers <- NA
test_data_for_prediction_RT$Predicted_Rating_Without_Outliers <- NA

# Loop through each cluster and make predictions
for (k in unique(test_expanded$Cluster)) {
  
  # Subset test data for the current cluster
  test_cluster_data <- test_data_for_prediction_RT %>% filter(Cluster == k)
  
  # Predict using the model trained WITH outliers
  if (!is.null(dt_models[[paste0("Cluster_", k, "_With_Outliers")]])) {
    test_cluster_data$Predicted_Rating_With_Outliers <- predict(
      dt_models[[paste0("Cluster_", k, "_With_Outliers")]], 
      newdata = test_cluster_data
    )
  }
  
  # Predict using the model trained WITHOUT outliers
  if (!is.null(dt_models[[paste0("Cluster_", k, "_Without_Outliers")]])) {
    test_cluster_data$Predicted_Rating_Without_Outliers <- predict(
      dt_models[[paste0("Cluster_", k, "_Without_Outliers")]], 
      newdata = test_cluster_data
    )
  }
  
  # Update the test data with predictions
  test_expanded[test_expanded$Cluster == k, "Predicted_Rating_With_Outliers"] <- 
    test_cluster_data$Predicted_Rating_With_Outliers
  
  test_expanded[test_expanded$Cluster == k, "Predicted_Rating_Without_Outliers"] <- 
    test_cluster_data$Predicted_Rating_Without_Outliers
}

# View the test data with predictions
View(test_expanded)
write.csv(test_expanded, "C:/Users/Deelaka/Desktop/Predictions_RT.csv", row.names = FALSE)

# -------------------- Part 3: Compare Model Performance (RMSE, MAE, R²) --------------------

# Initialize storage for evaluation results
performance_results_RT <- list()

# Loop through each cluster's trained model
for (k in unique(train_expanded$Cluster)) {
  
  # Subset training and test data for the current cluster
  train_cluster_data_with_outliers <- train_with_outliers %>% filter(Cluster == k)
  train_cluster_data_without_outliers <- train_without_outliers %>% filter(Cluster == k)
  test_cluster_data <- test_expanded %>% filter(Cluster == k)
  
  # Evaluate model trained WITH outliers
  if (!is.null(dt_models[[paste0("Cluster_", k, "_With_Outliers")]])) {
    # Predict on training data
    train_predictions <- predict(dt_models[[paste0("Cluster_", k, "_With_Outliers")]], 
                                 newdata = train_cluster_data_with_outliers)
    # Predict on test data
    test_predictions <- predict(dt_models[[paste0("Cluster_", k, "_With_Outliers")]], 
                                newdata = test_cluster_data)
    
    # Calculate metrics for training data
    train_mae <- mae(train_cluster_data_with_outliers$Rating, train_predictions)
    train_rmse <- rmse(train_cluster_data_with_outliers$Rating, train_predictions)
    train_r2 <- cor(train_cluster_data_with_outliers$Rating, train_predictions)^2
    
    # Calculate metrics for test data
    test_mae <- mae(test_cluster_data$Rating, test_predictions)
    test_rmse <- rmse(test_cluster_data$Rating, test_predictions)
    test_r2 <- cor(test_cluster_data$Rating, test_predictions)^2
    
    # Store results
    performance_results_RT[[paste0("Cluster_", k, "_With_Outliers")]] <- list(
      Train = list(MAE = train_mae, RMSE = train_rmse, R2 = train_r2),
      Test = list(MAE = test_mae, RMSE = test_rmse, R2 = test_r2)
    )
  }
  
  # Evaluate model trained WITHOUT outliers
  if (!is.null(dt_models[[paste0("Cluster_", k, "_Without_Outliers")]])) {
    # Predict on training data
    train_predictions <- predict(dt_models[[paste0("Cluster_", k, "_Without_Outliers")]], 
                                 newdata = train_cluster_data_without_outliers)
    # Predict on test data
    test_predictions <- predict(dt_models[[paste0("Cluster_", k, "_Without_Outliers")]], 
                                newdata = test_cluster_data)
    
    # Calculate metrics for training data
    train_mae <- mae(train_cluster_data_without_outliers$Rating, train_predictions)
    train_rmse <- rmse(train_cluster_data_without_outliers$Rating, train_predictions)
    train_r2 <- cor(train_cluster_data_without_outliers$Rating, train_predictions)^2
    
    # Calculate metrics for test data
    test_mae <- mae(test_cluster_data$Rating, test_predictions)
    test_rmse <- rmse(test_cluster_data$Rating, test_predictions)
    test_r2 <- cor(test_cluster_data$Rating, test_predictions)^2
    
    # Store results
    performance_results_RT[[paste0("Cluster_", k, "_Without_Outliers")]] <- list(
      Train = list(MAE = train_mae, RMSE = train_rmse, R2 = train_r2),
      Test = list(MAE = test_mae, RMSE = test_rmse, R2 = test_r2)
    )
  }
}

# Convert evaluation results into a structured dataframe
performance_table_RT <- do.call(rbind, lapply(names(performance_results_RT), function(cluster) {
  
  train_results <- performance_results_RT[[cluster]]$Train
  test_results <- performance_results_RT[[cluster]]$Test
  
  data.frame(
    Cluster = cluster,
    Train_MAE = round(train_results$MAE, 3),
    Train_RMSE = round(train_results$RMSE, 3),
    Train_R2 = round(train_results$R2, 3),
    Test_MAE = round(test_results$MAE, 3),
    Test_RMSE = round(test_results$RMSE, 3),
    Test_R2 = round(test_results$R2, 3),
    MAE_Difference = round(abs(train_results$MAE - test_results$MAE), 3),
    RMSE_Difference = round(abs(train_results$RMSE - test_results$RMSE), 3),
    R2_Difference = round(abs(train_results$R2 - test_results$R2), 3)
  )
}))

View(performance_table_RT)
write.csv(performance_table_RT, "C:/Users/Deelaka/Desktop/Performance_RT.csv", row.names = FALSE)

# ------------------------- Part 4: Feature Importance per Cluster --------------------------

# Initialize a list to store feature importance results
feature_importance_list_RT <- list()

# Loop through each cluster's trained model
for (k in unique(train_expanded$Cluster)) {
  
  # Extract feature importance for the model trained WITH outliers
  if (!is.null(dt_models[[paste0("Cluster_", k, "_With_Outliers")]])) {
    # Access the underlying rpart object
    dt_model_with_outliers <- dt_models[[paste0("Cluster_", k, "_With_Outliers")]]$finalModel
    importance_with_outliers <- dt_model_with_outliers$variable.importance
    importance_with_outliers_df <- data.frame(Feature = names(importance_with_outliers), 
                                              Importance = importance_with_outliers, 
                                              Cluster = paste0("Cluster_", k), 
                                              Model = "With Outliers")
    feature_importance_list_RT[[paste0("Cluster_", k, "_With_Outliers")]] <- importance_with_outliers_df
  }
  
  # Extract feature importance for the model trained WITHOUT outliers
  if (!is.null(dt_models[[paste0("Cluster_", k, "_Without_Outliers")]])) {
    # Access the underlying rpart object
    dt_model_without_outliers <- dt_models[[paste0("Cluster_", k, "_Without_Outliers")]]$finalModel
    importance_without_outliers <- dt_model_without_outliers$variable.importance
    importance_without_outliers_df <- data.frame(Feature = names(importance_without_outliers), 
                                                 Importance = importance_without_outliers, 
                                                 Cluster = paste0("Cluster_", k), 
                                                 Model = "Without Outliers")
    feature_importance_list_RT[[paste0("Cluster_", k, "_Without_Outliers")]] <- importance_without_outliers_df
  }
}

# Combine all feature importance results into one dataframe
feature_importance_df_RT <- bind_rows(feature_importance_list_RT)

# Function to extract base feature name from category
get_base_feature_RT <- function(feature_name) {
  # Split the feature name by the first occurrence of specific patterns
  patterns <- c("City", "Restaurant", "Cuisine", "Meal", "Table", "Online")
  
  for (pattern in patterns) {
    if (grepl(pattern, feature_name)) {
      return(pattern)
    }
  }
  
  # For features like 'Votes' and 'AvgCostForTwo' that don't need splitting
  return(feature_name)
}

# Aggregate feature importance at categorical variable level
feature_importance_aggregated_percentage_RT <- feature_importance_df_RT %>%
  mutate(Base_Feature = sapply(Feature, get_base_feature_RT)) %>%
  group_by(Cluster, Model, Base_Feature) %>%
  summarise(
    Total_Importance = sum(Importance),
    .groups = 'drop'
  ) %>%
  group_by(Cluster, Model) %>%
  mutate(Importance_Percentage = (Total_Importance / sum(Total_Importance)) * 100) %>%
  ungroup()

View(feature_importance_aggregated_percentage_RT)
write.csv(feature_importance_aggregated_percentage_RT, "C:/Users/Deelaka/Desktop/Feature_Importance_RT.csv", row.names = FALSE)

# Create a more interpretable plot
ggplot(feature_importance_aggregated_percentage_RT, 
       aes(x = reorder(Base_Feature, Importance_Percentage), 
           y = Importance_Percentage, 
           fill = Model)) +
  geom_bar(stat = "identity", 
           position = "dodge",
           width = 0.7) +
  geom_text(aes(label = sprintf("%.0f%%", Importance_Percentage)),
            position = position_dodge(width = 0.9),
            hjust = -0.1, 
            size = 3) +
  facet_wrap(~ Cluster, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Feature Importance per Cluster",
    x = "Feature Category",
    y = "Percentage Importance"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(
    values = c("With Outliers" = "gold", "Without Outliers" = "darkorange")
  )


# Back to the initial testing dataset
test_expanded <- subset(test_expanded, select = -c(Predicted_Rating_With_Outliers, Predicted_Rating_Without_Outliers))
View(test_expanded)


# ==================================== Random Forest ========================================
library(randomForest)
library(dplyr)
library(Metrics)
library(parallel)  # For parallel processing
library(caret)     # For k-fold cross-validation
library(glmnet)    # For regularization (optional)

# ------ Part 1: Train Random Forest Models for Each Cluster (With & Without Outliers) ------

# Define the hyperparameter grid for Random Forest
hyperparameter_grid <- expand.grid(
  ntree = c(100),       # Number of trees
  mtry = c(2, 4),       # Number of features to consider at each split
  maxdepth = c(3, 6),   # Maximum depth of trees
  nodesize = c(1)       # Minimum size of terminal nodes
)

# Define k-Fold Cross-Validation Control
set.seed(123)
cv_control <- trainControl(
  method = "cv",        
  number = 10,           
  savePredictions = "final",
  allowParallel = TRUE  
)

# Function to train a Random Forest model with k-Fold Cross-Validation
train_rf_model_cv <- function(data, params) {
  train(
    Rating ~ ., 
    data = data %>% dplyr::select(-Cluster),  # Exclude Cluster column
    method = "rf",
    trControl = cv_control,
    tuneGrid = data.frame(mtry = params$mtry),  # Only mtry is tunable in caret
    ntree = params$ntree,
    maxdepth = params$maxdepth,
    nodesize = params$nodesize
  )
}

# Initialize model storage
rf_models <- list()

# Loop through each cluster
for (k in unique(train_expanded$Cluster)) {
  
  # Get data subsets
  cluster_with_outliers <- train_with_outliers %>% filter(Cluster == k)
  cluster_without_outliers <- train_without_outliers %>% filter(Cluster == k)
  
  # Convert hyperparameter grid to a list of parameter sets
  param_list <- split(hyperparameter_grid, seq(nrow(hyperparameter_grid)))
  
  # Set up a cluster for parallel processing
  num_cores <- detectCores() - 1  # Use all but one core
  cl <- makeCluster(num_cores)
  
  # Export necessary objects and functions to the cluster
  clusterExport(cl, c("train_rf_model_cv", "train", "cluster_with_outliers", "cluster_without_outliers", "cv_control"))
  
  # Load required libraries on the workers
  clusterEvalQ(cl, library(dplyr))
  clusterEvalQ(cl, library(caret))
  clusterEvalQ(cl, library(randomForest))
  
  # Train models WITH outliers in parallel
  cat("Training Random Forest for Cluster", k, "- With Outliers\n")
  rf_models_with_outliers <- parLapply(cl, param_list, train_rf_model_cv, data = cluster_with_outliers)
  
  # Evaluate models WITH outliers and select the best one
  tuning_results_with_outliers <- do.call(rbind, lapply(seq_along(rf_models_with_outliers), function(i) {
    params <- param_list[[i]]
    model <- rf_models_with_outliers[[i]]
    train_predictions <- predict(model, newdata = cluster_with_outliers)
    train_mae <- mae(cluster_with_outliers$Rating, train_predictions)
    data.frame(
      ntree = params$ntree,
      mtry = params$mtry,
      maxdepth = params$maxdepth,
      nodesize = params$nodesize,
      MAE = train_mae
    )
  }))
  
  best_with_outliers <- tuning_results_with_outliers %>%
    arrange(MAE) %>%
    head(1)
  
  # Train the final model WITH outliers using the best hyperparameters
  rf_with_outliers <- train_rf_model_cv(cluster_with_outliers, best_with_outliers)
  
  # Train models WITHOUT outliers in parallel
  cat("Training Random Forest for Cluster", k, "- Without Outliers\n")
  rf_models_without_outliers <- parLapply(cl, param_list, train_rf_model_cv, data = cluster_without_outliers)
  
  # Evaluate models WITHOUT outliers and select the best one
  tuning_results_without_outliers <- do.call(rbind, lapply(seq_along(rf_models_without_outliers), function(i) {
    params <- param_list[[i]]
    model <- rf_models_without_outliers[[i]]
    train_predictions <- predict(model, newdata = cluster_without_outliers)
    train_mae <- mae(cluster_without_outliers$Rating, train_predictions)
    data.frame(
      ntree = params$ntree,
      mtry = params$mtry,
      maxdepth = params$maxdepth,
      nodesize = params$nodesize,
      MAE = train_mae
    )
  }))
  
  best_without_outliers <- tuning_results_without_outliers %>%
    arrange(MAE) %>%
    head(1)
  
  # Train the final model WITHOUT outliers using the best hyperparameters
  rf_without_outliers <- train_rf_model_cv(cluster_without_outliers, best_without_outliers)
  
  # Store models
  rf_models[[paste0("Cluster_", k, "_With_Outliers")]] <- rf_with_outliers
  rf_models[[paste0("Cluster_", k, "_Without_Outliers")]] <- rf_without_outliers
  
  # Stop the cluster
  stopCluster(cl)
}

# ------------------------- Part 2: Predict 'Rating' on Test Data ---------------------------

# Exclude the 'Rating' column from the test data
test_data_for_prediction_RF <- test_expanded %>% dplyr::select(-Rating)

# Initialize storage for predictions
test_data_for_prediction_RF$Predicted_Rating_With_Outliers <- NA
test_data_for_prediction_RF$Predicted_Rating_Without_Outliers <- NA

# Loop through each cluster and make predictions
for (k in unique(test_expanded$Cluster)) {
  
  # Subset test data for the current cluster
  test_cluster_data <- test_data_for_prediction_RF %>% filter(Cluster == k)
  
  # Predict using the model trained WITH outliers
  if (!is.null(rf_models[[paste0("Cluster_", k, "_With_Outliers")]])) {
    test_cluster_data$Predicted_Rating_With_Outliers <- predict(
      rf_models[[paste0("Cluster_", k, "_With_Outliers")]], 
      newdata = test_cluster_data
    )
  }
  
  # Predict using the model trained WITHOUT outliers
  if (!is.null(rf_models[[paste0("Cluster_", k, "_Without_Outliers")]])) {
    test_cluster_data$Predicted_Rating_Without_Outliers <- predict(
      rf_models[[paste0("Cluster_", k, "_Without_Outliers")]], 
      newdata = test_cluster_data
    )
  }
  
  # Update the test data with predictions
  test_expanded[test_expanded$Cluster == k, "Predicted_Rating_With_Outliers"] <- 
    test_cluster_data$Predicted_Rating_With_Outliers
  
  test_expanded[test_expanded$Cluster == k, "Predicted_Rating_Without_Outliers"] <- 
    test_cluster_data$Predicted_Rating_Without_Outliers
}

# View the test data with predictions
View(test_expanded)
write.csv(test_expanded, "C:/Users/Deelaka/Desktop/Predictions_RF.csv", row.names = FALSE)

# -------------------- Part 3: Compare Model Performance (RMSE, MAE, R²) --------------------

# Initialize storage for evaluation results
performance_results_RF <- list()

# Loop through each cluster's trained model
for (k in unique(train_expanded$Cluster)) {
  
  # Subset training and test data for the current cluster
  train_cluster_data_with_outliers <- train_with_outliers %>% filter(Cluster == k)
  train_cluster_data_without_outliers <- train_without_outliers %>% filter(Cluster == k)
  test_cluster_data <- test_expanded %>% filter(Cluster == k)
  
  # Evaluate model trained WITH outliers
  if (!is.null(rf_models[[paste0("Cluster_", k, "_With_Outliers")]])) {
    # Predict on training data
    train_predictions <- predict(rf_models[[paste0("Cluster_", k, "_With_Outliers")]], 
                                 newdata = train_cluster_data_with_outliers)
    # Predict on test data
    test_predictions <- predict(rf_models[[paste0("Cluster_", k, "_With_Outliers")]], 
                                newdata = test_cluster_data)
    
    # Calculate metrics for training data
    train_mae <- mae(train_cluster_data_with_outliers$Rating, train_predictions)
    train_rmse <- rmse(train_cluster_data_with_outliers$Rating, train_predictions)
    train_r2 <- cor(train_cluster_data_with_outliers$Rating, train_predictions)^2
    
    # Calculate metrics for test data
    test_mae <- mae(test_cluster_data$Rating, test_predictions)
    test_rmse <- rmse(test_cluster_data$Rating, test_predictions)
    test_r2 <- cor(test_cluster_data$Rating, test_predictions)^2
    
    # Store results
    performance_results_RF[[paste0("Cluster_", k, "_With_Outliers")]] <- list(
      Train = list(MAE = train_mae, RMSE = train_rmse, R2 = train_r2),
      Test = list(MAE = test_mae, RMSE = test_rmse, R2 = test_r2)
    )
  }
  
  # Evaluate model trained WITHOUT outliers
  if (!is.null(rf_models[[paste0("Cluster_", k, "_Without_Outliers")]])) {
    # Predict on training data
    train_predictions <- predict(rf_models[[paste0("Cluster_", k, "_Without_Outliers")]], 
                                 newdata = train_cluster_data_without_outliers)
    # Predict on test data
    test_predictions <- predict(rf_models[[paste0("Cluster_", k, "_Without_Outliers")]], 
                                newdata = test_cluster_data)
    
    # Calculate metrics for training data
    train_mae <- mae(train_cluster_data_without_outliers$Rating, train_predictions)
    train_rmse <- rmse(train_cluster_data_without_outliers$Rating, train_predictions)
    train_r2 <- cor(train_cluster_data_without_outliers$Rating, train_predictions)^2
    
    # Calculate metrics for test data
    test_mae <- mae(test_cluster_data$Rating, test_predictions)
    test_rmse <- rmse(test_cluster_data$Rating, test_predictions)
    test_r2 <- cor(test_cluster_data$Rating, test_predictions)^2
    
    # Store results
    performance_results_RF[[paste0("Cluster_", k, "_Without_Outliers")]] <- list(
      Train = list(MAE = train_mae, RMSE = train_rmse, R2 = train_r2),
      Test = list(MAE = test_mae, RMSE = test_rmse, R2 = test_r2)
    )
  }
}

# Convert evaluation results into a structured dataframe
performance_table_RF <- do.call(rbind, lapply(names(performance_results_RF), function(cluster) {
  
  train_results <- performance_results_RF[[cluster]]$Train
  test_results <- performance_results_RF[[cluster]]$Test
  
  data.frame(
    Cluster = cluster,
    Train_MAE = round(train_results$MAE, 3),
    Train_RMSE = round(train_results$RMSE, 3),
    Train_R2 = round(train_results$R2, 3),
    Test_MAE = round(test_results$MAE, 3),
    Test_RMSE = round(test_results$RMSE, 3),
    Test_R2 = round(test_results$R2, 3),
    MAE_Difference = round(abs(train_results$MAE - test_results$MAE), 3),
    RMSE_Difference = round(abs(train_results$RMSE - test_results$RMSE), 3),
    R2_Difference = round(abs(train_results$R2 - test_results$R2), 3)
  )
}))

View(performance_table_RF)
write.csv(performance_table_RF, "C:/Users/Deelaka/Desktop/performance_table_RF.csv", row.names = FALSE)

# ------------------------- Part 4: Feature Importance per Cluster --------------------------

# Initialize a list to store feature importance results
feature_importance_list_RF <- list()

# Loop through each cluster's trained model
for (k in unique(train_expanded$Cluster)) {
  
  # Extract feature importance for the model trained WITH outliers
  if (!is.null(rf_models[[paste0("Cluster_", k, "_With_Outliers")]])) {
    # Access the underlying randomForest object
    rf_model_with_outliers <- rf_models[[paste0("Cluster_", k, "_With_Outliers")]]$finalModel
    importance_with_outliers <- importance(rf_model_with_outliers)
    importance_with_outliers_df <- data.frame(Feature = rownames(importance_with_outliers), 
                                              Importance = importance_with_outliers[, 1], 
                                              Cluster = paste0("Cluster_", k), 
                                              Model = "With Outliers")
    feature_importance_list_RF[[paste0("Cluster_", k, "_With_Outliers")]] <- importance_with_outliers_df
  }
  
  # Extract feature importance for the model trained WITHOUT outliers
  if (!is.null(rf_models[[paste0("Cluster_", k, "_Without_Outliers")]])) {
    # Access the underlying randomForest object
    rf_model_without_outliers <- rf_models[[paste0("Cluster_", k, "_Without_Outliers")]]$finalModel
    importance_without_outliers <- importance(rf_model_without_outliers)
    importance_without_outliers_df <- data.frame(Feature = rownames(importance_without_outliers), 
                                                 Importance = importance_without_outliers[, 1], 
                                                 Cluster = paste0("Cluster_", k), 
                                                 Model = "Without Outliers")
    feature_importance_list_RF[[paste0("Cluster_", k, "_Without_Outliers")]] <- importance_without_outliers_df
  }
}

# Combine all feature importance results into one dataframe
feature_importance_df_RF <- bind_rows(feature_importance_list_RF)

# Function to extract base feature name from category
get_base_feature_RF <- function(feature_name) {
  # Split the feature name by the first occurrence of specific patterns
  patterns <- c("City", "Restaurant", "Cuisine", "Meal", "Table", "Online")
  
  for (pattern in patterns) {
    if (grepl(pattern, feature_name)) {
      return(pattern)
    }
  }
  
  # For features like 'Votes' and 'AvgCostForTwo' that don't need splitting
  return(feature_name)
}

# Aggregate feature importance at categorical variable level
feature_importance_aggregated_percentage_RF <- feature_importance_df_RF %>%
  mutate(Base_Feature = sapply(Feature, get_base_feature_RF)) %>%
  group_by(Cluster, Model, Base_Feature) %>%
  summarise(
    Total_Importance = sum(Importance),
    .groups = 'drop'
  ) %>%
  group_by(Cluster, Model) %>%
  mutate(Importance_Percentage = (Total_Importance / sum(Total_Importance)) * 100) %>%
  ungroup()

View(feature_importance_aggregated_percentage_RF)
write.csv(feature_importance_aggregated_percentage_RF, "C:/Users/Deelaka/Desktop/Feature_Importance_RF.csv", row.names = FALSE)

# Create a more interpretable plot
ggplot(feature_importance_aggregated_percentage_RF, 
       aes(x = reorder(Base_Feature, Importance_Percentage), 
           y = Importance_Percentage, 
           fill = Model)) +
  geom_bar(stat = "identity", 
           position = "dodge",
           width = 0.7) +
  geom_text(aes(label = sprintf("%.0f%%", Importance_Percentage)),
            position = position_dodge(width = 0.9),
            hjust = -0.1, 
            size = 3) +
  facet_wrap(~ Cluster, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Feature Importance per Cluster",
    x = "Feature Category",
    y = "Percentage Importance"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(
    values = c("With Outliers" = "gold", "Without Outliers" = "darkorange")
  )

# Back to the initial testing dataset
test_expanded <- subset(test_expanded, select = -c(Predicted_Rating_With_Outliers, Predicted_Rating_Without_Outliers))
View(test_expanded)


# ======================================= XGBoost ===========================================
library(xgboost)    # For XGBoost
library(dplyr)      # For data manipulation
library(Metrics)    # For evaluation metrics
library(parallel)   # For parallel processing
library(caret)      # For k-fold cross-validation

# ------------------------------ Part 1: Data Preprocessing ---------------------------------

# Label Encoding function
label_encode <- function(column) {
  ifelse(column == "OnlineOrder_Yes" | column == "TableBooking_Yes", 1, 0)
}

# Function to one-hot encode multiple columns
one_hot_encode_columns <- function(data, columns_to_encode) {
  for (col in columns_to_encode) {
    dummy_vars <- dummyVars(as.formula(paste("~", col)), data = data)
    encoded_data <- predict(dummy_vars, newdata = data)
    encoded_data <- as.data.frame(encoded_data)[, -1, drop = FALSE]
    colnames(encoded_data) <- paste0(col, "_", colnames(encoded_data))
    data <- cbind(data, encoded_data)
    data <- data %>% dplyr::select(-all_of(col))
  }
  return(data)
}

# Target encoding (Apply median-based target encoding) function
target_encode <- function(data, categorical_col, target_col) {
  median_target <- data %>%
    group_by(!!sym(categorical_col)) %>%
    summarise(median_target = median(!!sym(target_col), na.rm = TRUE)) %>%
    ungroup()
  
  data <- data %>%
    left_join(median_target, by = categorical_col) %>%
    rename(!!paste0(categorical_col, "_Encoded") := median_target) %>%
    dplyr::select(-all_of(categorical_col))
  
  return(data)
}

# Preprocess training data with outliers
train_with_outliers <- train_with_outliers %>%
  mutate(
    OnlineOrder_Encoded = label_encode(OnlineOrder),
    TableBooking_Encoded = label_encode(TableBooking)
  ) %>%
  dplyr::select(-OnlineOrder, -TableBooking)

columns_to_encode <- c("RestaurantType", "MealType", "CuisineOrigin")
train_with_outliers <- one_hot_encode_columns(train_with_outliers, columns_to_encode)
train_with_outliers <- target_encode(train_with_outliers, "City", "Rating")

# Preprocess training data without outliers
train_without_outliers <- train_without_outliers %>%
  mutate(
    OnlineOrder_Encoded = label_encode(OnlineOrder),
    TableBooking_Encoded = label_encode(TableBooking)
  ) %>%
  dplyr::select(-OnlineOrder, -TableBooking)

train_without_outliers <- one_hot_encode_columns(train_without_outliers, columns_to_encode)
train_without_outliers <- target_encode(train_without_outliers, "City", "Rating")

# Preprocess test data
test_expanded <- test_expanded %>%
  mutate(
    OnlineOrder_Encoded = label_encode(OnlineOrder),
    TableBooking_Encoded = label_encode(TableBooking)
  ) %>%
  dplyr::select(-OnlineOrder, -TableBooking)

test_expanded <- one_hot_encode_columns(test_expanded, columns_to_encode)
test_expanded <- target_encode(test_expanded, "City", "Rating")

# ------------------------- Part 2: Check and Transform Skewness ----------------------------
library(e1071)

# Columns to check for skewness
columns_to_check <- c("Rating", "Votes", "AvgCostForTwo", "City_Encoded")

check_skewness_and_transform <- function(data, columns_to_check) {
  
  # Check skewness for the specified columns
  skewness_values <- sapply(data[columns_to_check], skewness)
  print("Skewness values before transformation:")
  print(skewness_values)
  
  # Apply log transformation to columns with high skewness (e.g., |skewness| > 1)
  for (col in columns_to_check) {
    if (abs(skewness_values[col]) > 1) {
      data[[col]] <- log(data[[col]] + 1)  # Add 1 to handle zero values
    }
  }
  
  # Check skewness again after transformation
  skewness_values_after <- sapply(data[columns_to_check], skewness)
  print("Skewness values after transformation:")
  print(skewness_values_after)
  
  return(data)
}

train_with_outliers <- check_skewness_and_transform(train_with_outliers, columns_to_check)
train_without_outliers <- check_skewness_and_transform(train_without_outliers, columns_to_check)
test_expanded <- check_skewness_and_transform(test_expanded, columns_to_check)

# ---------- Part 3: Train XGBoost Models for Each Cluster (With & Without Outliers) --------

# Define the hyperparameter grid
hyperparameter_grid <- expand.grid(
  nrounds = c(100),              # Number of boosting rounds
  max_depth = c(3, 6),           # Maximum depth of a tree
  eta = c(0.01, 0.1),            # Learning rate
  gamma = c(0),                  # Minimum loss reduction to make a split
  colsample_bytree = c(0.8),     # Subsample ratio of columns
  min_child_weight = c(1, 3),    # Minimum sum of instance weight
  subsample = c(0.8)             # Subsample ratio of the training instances
)

# Define k-Fold Cross-Validation Control
set.seed(123)
cv_control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  allowParallel = TRUE
)

# Function to train XGBoost model
train_xgb_model_cv <- function(data, params) {
  train(
    Rating ~ .,
    data = data %>% dplyr::select(-Cluster),
    method = "xgbTree",
    trControl = cv_control,
    tuneGrid = params,      # Use the provided hyperparameters
    verbose = FALSE         # Suppress output
  )
}

# Initialize model storage
xgb_models <- list()

# Loop through each cluster
for (k in unique(train_with_outliers$Cluster)) {
  
  # Get data subsets
  cluster_with_outliers <- train_with_outliers %>% filter(Cluster == k)
  cluster_without_outliers <- train_without_outliers %>% filter(Cluster == k)
  
  # Convert hyperparameter grid to list
  param_list <- split(hyperparameter_grid, seq(nrow(hyperparameter_grid)))
  
  # Set up parallel processing
  num_cores <- detectCores() - 1  # Use all but one core
  cl <- makeCluster(num_cores)
  
  # Export necessary objects
  clusterExport(cl, c("train_xgb_model_cv", "train", "cluster_with_outliers", 
                      "cluster_without_outliers", "cv_control", "hyperparameter_grid"))
  
  # Load required libraries on workers
  clusterEvalQ(cl, library(dplyr))
  clusterEvalQ(cl, library(caret))
  clusterEvalQ(cl, library(xgboost))
  
  # Train models WITH outliers
  cat("Training XGBoost for Cluster", k, "- With Outliers\n")
  xgb_models_with_outliers <- parLapply(cl, param_list, train_xgb_model_cv, 
                                        data = cluster_with_outliers)
  
  # Select best model WITH outliers
  tuning_results_with_outliers <- do.call(rbind, lapply(seq_along(xgb_models_with_outliers), 
                                                        function(i) {
                                                          params <- param_list[[i]]
                                                          model <- xgb_models_with_outliers[[i]]
                                                          train_predictions <- predict(model, newdata = cluster_with_outliers)
                                                          train_mae <- mae(cluster_with_outliers$Rating, train_predictions)
                                                          data.frame(
                                                            nrounds = params$nrounds,
                                                            max_depth = params$max_depth,
                                                            eta = params$eta,
                                                            gamma = params$gamma,
                                                            colsample_bytree = params$colsample_bytree,
                                                            min_child_weight = params$min_child_weight,
                                                            subsample = params$subsample,
                                                            MAE = train_mae
                                                          )
                                                        }))
  
  best_with_outliers <- tuning_results_with_outliers %>%
    arrange(MAE) %>%
    head(1) %>%
    dplyr::select(-MAE)  # Remove the MAE column
  
  # Train final model WITH outliers
  xgb_with_outliers <- train_xgb_model_cv(cluster_with_outliers, best_with_outliers)
  
  # Repeat process for WITHOUT outliers
  cat("Training XGBoost for Cluster", k, "- Without Outliers\n")
  xgb_models_without_outliers <- parLapply(cl, param_list, train_xgb_model_cv, 
                                           data = cluster_without_outliers)
  
  tuning_results_without_outliers <- do.call(rbind, lapply(seq_along(xgb_models_without_outliers), 
                                                           function(i) {
                                                             params <- param_list[[i]]
                                                             model <- xgb_models_without_outliers[[i]]
                                                             train_predictions <- predict(model, newdata = cluster_without_outliers)
                                                             train_mae <- mae(cluster_without_outliers$Rating, train_predictions)
                                                             data.frame(
                                                               nrounds = params$nrounds,
                                                               max_depth = params$max_depth,
                                                               eta = params$eta,
                                                               gamma = params$gamma,
                                                               colsample_bytree = params$colsample_bytree,
                                                               min_child_weight = params$min_child_weight,
                                                               subsample = params$subsample,
                                                               MAE = train_mae
                                                             )
                                                           }))
  
  best_without_outliers <- tuning_results_without_outliers %>%
    arrange(MAE) %>%
    head(1) %>%
    dplyr::select(-MAE)  # Remove the MAE column
  
  xgb_without_outliers <- train_xgb_model_cv(cluster_without_outliers, best_without_outliers)
  
  # Store models
  xgb_models[[paste0("Cluster_", k, "_With_Outliers")]] <- xgb_with_outliers
  xgb_models[[paste0("Cluster_", k, "_Without_Outliers")]] <- xgb_without_outliers
  
  stopCluster(cl)
}

# ------------------------- Part 4: Predict 'Rating' on Test Data ---------------------------

# Exclude the 'Rating' column from the test data
test_data_for_prediction_XGB <- test_expanded %>% dplyr::select(-Rating)

# Initialize storage for predictions
test_data_for_prediction_XGB$Predicted_Rating_With_Outliers <- NA
test_data_for_prediction_XGB$Predicted_Rating_Without_Outliers <- NA

# Loop through each cluster and make predictions
for (k in unique(test_expanded$Cluster)) {
  
  # Subset test data for the current cluster
  test_cluster_data <- test_data_for_prediction_XGB %>% filter(Cluster == k)
  
  # Predict using the model trained WITH outliers
  if (!is.null(xgb_models[[paste0("Cluster_", k, "_With_Outliers")]])) {
    test_cluster_data$Predicted_Rating_With_Outliers <- predict(
      xgb_models[[paste0("Cluster_", k, "_With_Outliers")]], 
      newdata = test_cluster_data
    )
  }
  
  # Predict using the model trained WITHOUT outliers
  if (!is.null(xgb_models[[paste0("Cluster_", k, "_Without_Outliers")]])) {
    test_cluster_data$Predicted_Rating_Without_Outliers <- predict(
      xgb_models[[paste0("Cluster_", k, "_Without_Outliers")]], 
      newdata = test_cluster_data
    )
  }
  
  # Update the test data with predictions
  test_expanded[test_expanded$Cluster == k, "Predicted_Rating_With_Outliers"] <- 
    test_cluster_data$Predicted_Rating_With_Outliers
  
  test_expanded[test_expanded$Cluster == k, "Predicted_Rating_Without_Outliers"] <- 
    test_cluster_data$Predicted_Rating_Without_Outliers
}

# View the test data with predictions
View(test_expanded)
write.csv(test_expanded, "C:/Users/Deelaka/Desktop/Predictions_XGB.csv", row.names = FALSE)

# -------------------- Part 5: Compare Model Performance (RMSE, MAE, R²) --------------------

# Initialize storage for evaluation results
performance_results_XGB <- list()

# Loop through each cluster's trained model
for (k in unique(train_expanded$Cluster)) {
  
  # Subset training and test data for the current cluster
  train_cluster_data_with_outliers <- train_with_outliers %>% filter(Cluster == k)
  train_cluster_data_without_outliers <- train_without_outliers %>% filter(Cluster == k)
  test_cluster_data <- test_expanded %>% filter(Cluster == k)
  
  # Evaluate model trained WITH outliers
  if (!is.null(xgb_models[[paste0("Cluster_", k, "_With_Outliers")]])) {
    # Predict on training data
    train_predictions <- predict(xgb_models[[paste0("Cluster_", k, "_With_Outliers")]], 
                                 newdata = train_cluster_data_with_outliers)
    # Predict on test data
    test_predictions <- predict(xgb_models[[paste0("Cluster_", k, "_With_Outliers")]], 
                                newdata = test_cluster_data)
    
    # Calculate metrics for training data
    train_mae <- mae(train_cluster_data_with_outliers$Rating, train_predictions)
    train_rmse <- rmse(train_cluster_data_with_outliers$Rating, train_predictions)
    train_r2 <- cor(train_cluster_data_with_outliers$Rating, train_predictions)^2
    
    # Calculate metrics for test data
    test_mae <- mae(test_cluster_data$Rating, test_predictions)
    test_rmse <- rmse(test_cluster_data$Rating, test_predictions)
    test_r2 <- cor(test_cluster_data$Rating, test_predictions)^2
    
    # Store results
    performance_results_XGB[[paste0("Cluster_", k, "_With_Outliers")]] <- list(
      Train = list(MAE = train_mae, RMSE = train_rmse, R2 = train_r2),
      Test = list(MAE = test_mae, RMSE = test_rmse, R2 = test_r2)
    )
  }
  
  # Evaluate model trained WITHOUT outliers
  if (!is.null(xgb_models[[paste0("Cluster_", k, "_Without_Outliers")]])) {
    # Predict on training data
    train_predictions <- predict(xgb_models[[paste0("Cluster_", k, "_Without_Outliers")]], 
                                 newdata = train_cluster_data_without_outliers)
    # Predict on test data
    test_predictions <- predict(xgb_models[[paste0("Cluster_", k, "_Without_Outliers")]], 
                                newdata = test_cluster_data)
    
    # Calculate metrics for training data
    train_mae <- mae(train_cluster_data_without_outliers$Rating, train_predictions)
    train_rmse <- rmse(train_cluster_data_without_outliers$Rating, train_predictions)
    train_r2 <- cor(train_cluster_data_without_outliers$Rating, train_predictions)^2
    
    # Calculate metrics for test data
    test_mae <- mae(test_cluster_data$Rating, test_predictions)
    test_rmse <- rmse(test_cluster_data$Rating, test_predictions)
    test_r2 <- cor(test_cluster_data$Rating, test_predictions)^2
    
    # Store results
    performance_results_XGB[[paste0("Cluster_", k, "_Without_Outliers")]] <- list(
      Train = list(MAE = train_mae, RMSE = train_rmse, R2 = train_r2),
      Test = list(MAE = test_mae, RMSE = test_rmse, R2 = test_r2)
    )
  }
}

# Convert evaluation results into a structured dataframe
performance_table_XGB <- do.call(rbind, lapply(names(performance_results_XGB), function(cluster) {
  
  train_results <- performance_results_XGB[[cluster]]$Train
  test_results <- performance_results_XGB[[cluster]]$Test
  
  data.frame(
    Cluster = cluster,
    Train_MAE = round(train_results$MAE, 3),
    Train_RMSE = round(train_results$RMSE, 3),
    Train_R2 = round(train_results$R2, 3),
    Test_MAE = round(test_results$MAE, 3),
    Test_RMSE = round(test_results$RMSE, 3),
    Test_R2 = round(test_results$R2, 3),
    MAE_Difference = round(abs(train_results$MAE - test_results$MAE), 3),
    RMSE_Difference = round(abs(train_results$RMSE - test_results$RMSE), 3),
    R2_Difference = round(abs(train_results$R2 - test_results$R2), 3)
  )
}))

View(performance_table_XGB)
write.csv(performance_table_XGB, "C:/Users/Deelaka/Desktop/performance_table_XGB.csv", row.names = FALSE)

# ------------------------- Part 6: Feature Importance per Cluster --------------------------

# Initialize a list to store feature importance results
feature_importance_list_XGB <- list()

# Loop through each cluster's trained model
for (k in unique(train_expanded$Cluster)) {
  
  # Extract feature importance for the model trained WITH outliers
  if (!is.null(xgb_models[[paste0("Cluster_", k, "_With_Outliers")]])) {
    # Access the underlying xgb.Booster object
    xgb_model_with_outliers <- xgb_models[[paste0("Cluster_", k, "_With_Outliers")]]$finalModel
    importance_with_outliers <- xgb.importance(model = xgb_model_with_outliers)
    importance_with_outliers_df <- data.frame(Feature = importance_with_outliers$Feature, 
                                              Importance = importance_with_outliers$Gain, 
                                              Cluster = paste0("Cluster_", k), 
                                              Model = "With Outliers")
    feature_importance_list_XGB[[paste0("Cluster_", k, "_With_Outliers")]] <- importance_with_outliers_df
  }
  
  # Extract feature importance for the model trained WITHOUT outliers
  if (!is.null(xgb_models[[paste0("Cluster_", k, "_Without_Outliers")]])) {
    # Access the underlying xgb.Booster object
    xgb_model_without_outliers <- xgb_models[[paste0("Cluster_", k, "_Without_Outliers")]]$finalModel
    importance_without_outliers <- xgb.importance(model = xgb_model_without_outliers)
    importance_without_outliers_df <- data.frame(Feature = importance_without_outliers$Feature, 
                                                 Importance = importance_without_outliers$Gain, 
                                                 Cluster = paste0("Cluster_", k), 
                                                 Model = "Without Outliers")
    feature_importance_list_XGB[[paste0("Cluster_", k, "_Without_Outliers")]] <- importance_without_outliers_df
  }
}

# Combine all feature importance results into one dataframe
feature_importance_df_XGB <- bind_rows(feature_importance_list_XGB)

# Function to extract base feature name from category
get_base_feature_XGB <- function(feature_name) {
  # Split the feature name by the first occurrence of specific patterns
  patterns <- c("City", "Restaurant", "Cuisine", "Meal", "Table", "Online")
  
  for (pattern in patterns) {
    if (grepl(pattern, feature_name)) {
      return(pattern)
    }
  }
  
  # For features like 'Votes' and 'AvgCostForTwo' that don't need splitting
  return(feature_name)
}

# Aggregate feature importance at categorical variable level
feature_importance_aggregated_percentage_XGB <- feature_importance_df_XGB %>%
  mutate(Base_Feature = sapply(Feature, get_base_feature_XGB)) %>%
  group_by(Cluster, Model, Base_Feature) %>%
  summarise(
    Total_Importance = sum(Importance),
    .groups = 'drop'
  ) %>%
  group_by(Cluster, Model) %>%
  mutate(Importance_Percentage = (Total_Importance / sum(Total_Importance)) * 100) %>%
  ungroup()

View(feature_importance_aggregated_percentage_XGB)
write.csv(feature_importance_aggregated_percentage_XGB, "C:/Users/Deelaka/Desktop/Feature_Importance_XGB.csv", row.names = FALSE)

# Create a more interpretable plot
ggplot(feature_importance_aggregated_percentage_XGB, 
       aes(x = reorder(Base_Feature, Importance_Percentage), 
           y = Importance_Percentage, 
           fill = Model)) +
  geom_bar(stat = "identity", 
           position = "dodge",
           width = 0.7) +
  geom_text(aes(label = sprintf("%.0f%%", Importance_Percentage)),
            position = position_dodge(width = 0.9),
            hjust = -0.1, 
            size = 3) +
  facet_wrap(~ Cluster, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Feature Importance per Cluster",
    x = "Feature Category",
    y = "Percentage Importance"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(
    values = c("With Outliers" = "gold", "Without Outliers" = "darkorange")
  )
