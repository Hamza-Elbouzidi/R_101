# Introduction
# This script demonstrates various data manipulation and visualization techniques 
# using R and the tidyverse package. It covers operations such as filtering, 
# mutating, selecting, and summarizing data, as well as generating plots to visualize 
# relationships in datasets like 'starwars', 'msleep', and 'gapminder'. 
# Let's dive into the code!

# Basic arithmetic operation
5 + 6

# Assigning values to variables
a <- 5
b <- 6

# Adding variables
a + b

# Using the sum function
# This is an alternative way to add 'a' and 'b' using the built-in sum function.
sum(a, b)

# Creating a vector of ages
# We are creating a vector called 'ages' that contains the values 5 and 6.
ages <- c(5, 6)

# Displaying the vector
# This line outputs the 'ages' vector to the console.
ages

# Summing vector elements
sum(ages)

# Creating a data frame of names and ages
# We are creating a data frame called 'friends' with names and corresponding ages.
names <- c("John", "James")
friends <- data.frame(names, ages)

# Viewing the data frame
View(friends)

# Getting the structure of the data frame
# Here, we check the structure of the 'friends' data frame, including data types and dimensions.
str(friends)

# Accessing columns in the data frame
# This line retrieves the 'ages' column from the 'friends' data frame.
friends$ages

# Accessing another column
# Here, we access the 'names' column from the 'friends' data frame.
friends$names

# Loading the built-in starwars dataset
# This command loads the starwars dataset for further analysis.
data()
View(starwars)

# Installing and loading the tidyverse package
# We are installing the tidyverse package to utilize its powerful data manipulation functions.
install.packages("tidyverse")
library(tidyverse)

# Data manipulation with dplyr
# Here, we are filtering the 'starwars' dataset for characters taller than 150 cm and weighing less than 200 kg.
starwars %>%
  filter(height > 150 & mass < 200) %>%
  # Creating a new column for height in meters
  mutate(height_in_meters = height / 100) %>%
  # Selecting specific columns for output
  select(height_in_meters, mass) %>%
  # Arranging the data by mass in ascending order
  arrange(mass) %>%
  # Generating a plot of the resulting data
  plot()

# Viewing the msleep dataset
# This command opens the msleep dataset for inspection.
View(msleep)

# Glimpsing the dataset
# This provides a quick overview of the msleep dataset, showing the structure and first few rows.
glimpse(msleep)

# Previewing the first few rows
# This command displays the first few rows of the msleep dataset.
head(msleep)

# Checking the class of the 'name' column
# Here, we determine the data type of the 'name' column in the msleep dataset.
class(msleep$name)

# Counting the number of rows in the dataset
# This line returns the total number of rows in the msleep dataset.
length(msleep)

# Counting the number of names in the dataset
# Here, we count the number of entries in the 'name' column of the msleep dataset.
length(msleep$name)

# Getting the column names
# This line retrieves the names of the columns in the msleep dataset.
names(msleep)

# Finding unique values in the 'vore' column
# Here, we list all unique categories present in the 'vore' column of the msleep dataset.
unique(msleep$vore)

# Identifying missing values
# This line creates a logical vector indicating which rows have missing values in the msleep dataset.
missing <- !complete.cases(msleep)
msleep[missing,]

# Selecting specific columns from the starwars dataset
# This command selects the 'name', 'height', and 'mass' columns from the starwars dataset.
starwars %>%
  select(name, height, mass)

# Selecting a range of columns
# This retrieves the first three columns from the starwars dataset.
starwars %>%
  select(1:3)

# Selecting columns that end with "color"
# Here, we select all columns in the starwars dataset that end with the string "color".
starwars %>%
  select(ends_with("color"))

# Renaming a column in the starwars dataset
# This line renames the 'name' column to 'characters' and retrieves the first few rows.
starwars %>%
  rename("characters" = "name") %>%
  head()

# Checking the class of the hair_color column
# This line checks the data type of the 'hair_color' column in the starwars dataset.
class(starwars$hair_color)

# Converting hair_color to a factor
# Here, we convert the 'hair_color' column to a factor data type for better categorical analysis.
starwars$hair_color <- as.factor(starwars$hair_color)

# Mutating hair_color back to character
# This line converts the 'hair_color' factor back to character type and provides a glimpse of the dataset.
starwars %>%
  mutate(hair_color = as.character(hair_color)) %>%
  glimpse()

# Preparing the starwars dataset for analysis
# We create a new dataframe from starwars and convert the 'sex' column to a factor.
df <- starwars
df$sex <- as.factor(df$sex)
levels(df$sex)

# Re-leveling the sex factor
# Here, we reassign the levels of the 'sex' column in the dataframe to a specific order.
df <- df %>%
  mutate(sex = factor(sex,
                      levels = c("male", "female", "hermaphroditic", "none")))
levels(df$sex)

# Filtering the starwars dataset by mass and sex
# This line selects characters with a mass less than 55 kg and who are male.
starwars %>%
  select(mass, sex) %>%
  filter(mass < 55 & sex == "male")

# Recode the sex column in starwars
# Here, we recode the 'sex' column, changing "male" to "man" and "female" to "woman".
starwars %>%
  select(sex) %>%
  mutate(sex = recode(sex,
                      "male" = "man",
                      "female" = "woman"))

# Calculating the mean height
# We calculate the average height of characters in the starwars dataset, excluding missing values.
mean(starwars$height, na.rm = TRUE)

# Creating a data frame of friends with distinct names and ages
# We create a data frame called 'friends' with names and ages and check for distinct rows.
Names <- c("Peter", "John", "Andrew", "Peter")
Age <- c(22, 33, 44, 22)

friends <- data.frame(Names, Age)

# Displaying the friends data frame
friends

# Finding distinct rows
# Here, we retrieve unique rows from the 'friends' data frame.
friends %>%
  distinct

# Another way to find distinct rows
# This line utilizes the distinct function directly on the 'friends' data frame.
distinct(friends)

# Adding a new height_m column to starwars
# We calculate height in meters and add it as a new column in the starwars dataset.
starwars %>%
  mutate(height_m = height / 100) %>%
  select(name, height, height_m)

# Adding a 'tallness' classification
# Here, we classify characters as "short" or "tall" based on their height in meters.
starwars %>%
  mutate(height_m = height / 100) %>%
  select(name, height, height_m) %>%
  mutate(tallness = if_else(height_m < 1, "short", "tall"))

# Installing and loading the gapminder package
# We are installing the gapminder package to access its dataset for analysis.
install.packages("gapminder")
library(gapminder)

# Viewing the gapminder dataset
# This command opens the gapminder dataset for inspection.
View(gapminder)

# Selecting specific columns from the gapminder dataset
# Here, we select the country, year, and life expectancy columns from gapminder.
data <- select(gapminder, country, year, lifeExp)
View(data)

# Pivoting the data wider
# We transform the dataset into a wider format, spreading years as columns with life expectancy as values.
wide_data <- data %>%
  pivot_wider(names_from = year, values_from = lifeExp)
View(wide_data)

# Pivoting the data longer
# This command converts the wide data back into a longer format.
long_data <- wide_data %>%
  pivot_longer(2:13, names_to = "year", values_to = "lifeExp")

View(long_data)

# Exploring the msleep dataset
# We are checking various statistics for the 'awake' column in the msleep dataset.
View(msleep)
min(msleep$awake)
max(msleep$awake)
range(msleep$awake)
IQR(msleep$awake)

# Generating a summary of the awake column
# This provides a summary of the awake times in the msleep dataset.
summary(msleep$awake)

# Summarizing awake and sleep_total by selecting specific columns
# Here, we summarize the awake and total sleep times in msleep.
msleep %>%
  select(awake, sleep_total) %>%
  summary()

# Grouping by vore and summarizing sleep_total
# We group the msleep dataset by 'vore' and summarize the total sleep data.
msleep %>%
  drop_na(vore) %>%
  group_by(vore) %>%
  summarise(Lower = min(sleep_total),
            Average = mean(sleep_total),
            Upper = max(sleep_total),
            Difference = max(sleep_total) - min(sleep_total)) %>%
  arrange(Average) %>%
  View()

# Creating a frequency table for vore
# This line generates a frequency table for the 'vore' column in the msleep dataset.
table(msleep$vore)

# Filtering by order and creating a frequency table
# Here, we filter the msleep dataset for orders 'Rodentia' and 'Primates' and create a table.
msleep %>%
  select(vore, order) %>%
  filter(order %in% c("Rodentia", "Primates")) %>%
  table()

# Plotting pressure data
# This line creates a simple plot of the 'pressure' dataset.
plot(pressure)

# Creating a bar plot of gender distribution in starwars
# This code generates a bar plot displaying the count of each gender in the starwars dataset.
ggplot(data = starwars, mapping = aes(x = gender)) +
  geom_bar()

# Creating a histogram of character heights
# This command produces a histogram of the height distribution of characters in the starwars dataset.
starwars %>%
  drop_na(height) %>%
  ggplot(mapping = aes(x = height)) +
  geom_histogram()

# Creating a boxplot of character heights
# Here, we create a boxplot to visualize the distribution of character heights in the starwars dataset.
starwars %>%
  drop_na(height) %>%
  ggplot(aes(height)) +
  geom_boxplot(fill = "steelblue") +
  theme_bw() +
  labs(title = "Boxplot of height",
       x = "Height of characters")

# Creating a density plot of heights by sex
# This code generates a density plot of character heights, separated by sex.
starwars %>%
  drop_na(height) %>%
  filter(sex %in% c("male", "female")) %>%
  ggplot(mapping = aes(x = height, color = sex, fill = sex)) +
  geom_density(alpha = 0.2) +
  theme_bw()

# Scatter plot of height vs mass colored by sex
# Here, we create a scatter plot of height versus mass, differentiating by sex.
starwars %>%
  filter(mass < 200) %>%
  ggplot(aes(height, mass, color = sex)) +
  geom_point(size = 5, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Height and mass by sex")

# Scatter plot with smoothing and faceting by sex
# This command generates a scatter plot with a smoothing line and separates the data by sex.
starwars %>%
  filter(mass < 200) %>%
  ggplot(aes(height, mass, color = sex)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth() +
  facet_wrap(~sex) +
  theme_bw() +
  labs(titles = "Height and mass by sex")

# T-test comparing life expectancy between continents
# Here, we conduct a t-test to compare life expectancy across Africa and Europe using the gapminder dataset.
gapminder %>%
  filter(continent %in% c("Africa", "Europe")) %>%
  t.test(lifeExp ~ continent, data = ., alternative = "two.sided")

# ANOVA for life expectancy across continents in 2007
# This line performs an ANOVA test to compare life expectancy across specified continents for the year 2007.
gapminder %>%
  filter(year == 2007) %>%
  filter(continent %in% c("Americas", "Europe", "Asia")) %>%
  aov(lifeExp ~ continent, data = .) %>%
  summary()

# Post-hoc analysis using Tukey's HSD
# Here, we perform a Tukey's HSD test following the ANOVA to see which groups differ significantly.
gapminder %>%
  filter(year == 2007) %>%
  filter(continent %in% c("Americas", "Europe", "Asia")) %>%
  aov(lifeExp ~ continent, data = .) %>%
  TukeyHSD() %>%
  plot()

# Analyzing the iris dataset
# We preview the iris dataset to understand its structure and contents.
head(iris)

# Creating a new dataframe based on iris with size categories
# Here, we create a dataframe that categorizes flowers based on sepal length.
flowers <- iris %>%
  mutate(Size = cut(Sepal.Length,
                    breaks = 3,
                    labels = c("Small", "Medium", "Large"))) %>%
  select(Species, Size)

# Viewing the flowers data frame
view(flowers)

# Conducting a Chi-squared test on flower sizes
# This line creates a frequency table for flower sizes and performs a Chi-squared test.
flowers %>%
  select(Size) %>%
  table() %>%
  chisq.test()

# Analyzing the cars dataset
# We preview the first 10 rows of the cars dataset to understand its structure.
head(cars, 10)

# Creating a linear model to predict stopping distance based on speed
# Here, we fit a linear model to predict the stopping distance based on speed.
cars %>%
  lm(dist ~ speed, data = .) %>%
  summary()

# Visualizing the relationship between speed and stopping distance
# This code generates a scatter plot of speed versus stopping distance, along with a fitted line.
cars %>%
  ggplot(mapping = aes(x = speed, y = dist)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Relationship between Speed and Stopping Distance",
    x = "Speed (mph)",
    y = "Stopping Distance (ft)"
  ) +
  theme_minimal()
