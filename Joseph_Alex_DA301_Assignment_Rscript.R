## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
install.packages('tidyverse')
library(tidyverse)

# Install and import plotly for interactive charts.
install.packages('plotly')
library(plotly) 

# Import the data set.
# Please note that this assumes the CSV file is in the working directory.
df <- read.csv('turtle_sales.csv', header=TRUE)

# Print the data frame.
head(df)
View(df)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
df2 <- select(df, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
head(df2)

# View the descriptive statistics.
summary(df2)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

# Product vs. Global sales.
qplot(Product, Global_Sales, data=df2, geom=c('point', 'smooth'))

# Product vs. North America Sales.
qplot(Product, NA_Sales, data=df2, geom=c('point', 'smooth'))

# Product vs. Europe Sales.
qplot(Product, EU_Sales, data=df2, geom=c('point', 'smooth'))

## For all three trends above comparing the Product column to the Sales,
## the plot shows an inverse relationship between the Product column and the
## Sales column. When the Product id number was low, sales tended to be high.
## It has been acknowledged that the product id is a categorical variable.
## However, it has been kept as a numeric variable to easily visualise
## the distribution of various products vs. the regional sales data using a
## scatterplot.
## The data shows that only a small proportion of games exceeds global sales of
## 20 million, North American sales of 10 million and European sales of 5 
## million.


# Platform vs. Global Sales.
qplot(Global_Sales, Platform, data=df2)

# Platform vs. Europe Sales.
qplot(EU_Sales, Platform, data=df2)

# Platform vs. North America Sales.
qplot(NA_Sales, Platform, data=df2)

# Platform vs. Product.
qplot(Product, Platform, data=df2)

## The Global sales show that Nintendo platforms have the best sellers
## as DS, GB, NES and Wii are Nintendo consoles with products that have
## sold over 20 million. Only Nintendo platforms have products where global
## sales exceed 20 million. A similar trend is seen for European Sales as most
## Nintendo platforms have products exceeding 5 million sales. 
## There's also PS3 and PC products in Europe with over 5 million sales.
## In North America, Nintendo platforms (GB, NES, SNES and Wii) have items
## where sales exceeded 10 million. Microsoft's Xbox 360 console also has an
## item exceeding 10 million in North America which could be a result of 
## domestic bias.
## It was also noted that the current gen console (PS5/PSV) only had 4 products
## for sale.

# The average sales of products by platform will now be visualised for
# each region to explore the data further.

# Create dataframe with average sales of products by platform for each region.
average_sales_by_platform <- df2 %>%
  group_by(Platform) %>%
  summarise(Avg_NA_Sales = round(mean(NA_Sales),2),
            Avg_EU_Sales = round(mean(EU_Sales),2),
            Avg_Global_Sales = round(mean(Global_Sales),2))

# Print dataframe to visualise output.
print(average_sales_by_platform)

# Assign plot for mean global sales of products by platform to variable.
mean_global_sales_product <- ggplot(average_sales_by_platform,
                                    aes(x = reorder(Platform,
                                                    -Avg_Global_Sales),
                                        y = Avg_Global_Sales)) +
  geom_bar(stat = "identity", fill = "green") +
  geom_text(aes(label = round(Avg_Global_Sales, 2)),
            position = position_stack(vjust = 0.5), size = 3) + 
  labs(x = "Platform", y = "Average Global Sales (£M)",
       title = "Average Global Sales of products by Platform" ) +
  scale_y_continuous(breaks = seq(0, 12, 0.5)) +
  coord_flip() +  
  theme_minimal() 
# Make chart interactive and view output.
ggplotly(mean_global_sales_product)


# Assign plot for mean European sales of products by platform to variable.
mean_EU_sales_product <- ggplot(average_sales_by_platform,
                                aes(x = reorder(Platform, -Avg_EU_Sales),
                                    y = Avg_EU_Sales)) +
  geom_bar(stat = "identity", fill = "green") +
  geom_text(aes(label = round(Avg_EU_Sales, 2)), 
            position = position_stack(vjust = 0.5), size = 3) +
  labs(x = "Platform", y = "Average European Sales (£M)",
       title = "Average European Sales of products by Platform" ) +
  scale_y_continuous(breaks = seq(0, 4, 0.5)) +
  coord_flip() +  
  theme_minimal() 
# Make chart interactive and view output.
ggplotly(mean_EU_sales_product)


# Assign plot for mean North American sales of products by platform to variable.
mean_NA_sales_product <- ggplot(average_sales_by_platform,
                                aes(x = reorder(Platform, -Avg_NA_Sales),
                                    y = Avg_NA_Sales)) +
  geom_bar(stat = "identity", fill = "green") +
  geom_text(aes(label = round(Avg_NA_Sales, 2)),
            position = position_stack(vjust = 0.5), size = 3) + 
  labs(x = "Platform", y = "Average North American Sales (£M)",
       title = "Average North American Sales of products by Platform" ) +
  scale_y_continuous(breaks = seq(0, 10, 0.5)) +
  coord_flip() +  
  theme_minimal() 
# Make chart interactive and view output.
ggplotly(mean_NA_sales_product)

## Globally, a product had the highest average sales for legacy(old) Nintendo
## platforms such as NES, Wii, GB, SNES and DS. In contrast, the PS5 average
## global sales for a typical game is the lowest. PS4 games sell on average 
## the best excluding legacy platforms. Whilst a newer platform, the PS4 is 
## not a current generation console.

## In Europe, PS4 games were the second best selling on average at £2.63M per
## game. The rest of the top five were legacy platforms. PS5 games were on 
## average the second worst selling out of all platforms at £0.38M sales per
## product.

## In North America, the five best selling platforms for an average game were 
## all legacy platforms. PS5 is the worst selling platform for an average game
## at £0.11M.

# Europe sales vs. Global Sales.
qplot(EU_Sales, Global_Sales, data=df2)

# North America vs. Global
qplot(NA_Sales, Global_Sales, data=df2)

# Europe sales vs. North America Sales.
qplot(EU_Sales, NA_Sales, data=df2)

## The three qplots above for Sales data show a trend where the sales of a
## product in a particular region increases as the sales of the same product
## increases in another region. The positive correlation appears stronger
## for North American sales vs. Global sales than it is for European sales vs.
## North American sales.

## 2b) Histograms
# Create histograms.

# Histogram for Global Sales.
qplot(Global_Sales, bins=7, data=df2)

# Histogram for Europe Sales.
qplot(EU_Sales, bins=7, data=df2)

# Histogram for North America Sales.
qplot(NA_Sales, bins=7, data=df2)

## All the sales data appears to be very positively skewed.

## 2c) Boxplots
# Create boxplots.

# Boxplot for Platform vs. Global Sales assigned to a variable.
ggplot(data=df2, aes(x= Global_Sales, y= Platform)) +
  geom_boxplot(fill ='green') +
scale_x_continuous(breaks = seq(0, 70, 5),'Global Sales') +
  labs(title = "Global sales of Products based on Platform")

# Boxplot for Platform vs. Europe Sales.
ggplot(data=df2,
       mapping=aes(x= EU_Sales, y= Platform)) +
  geom_boxplot(fill ='green') +
  scale_x_continuous(breaks = seq(0, 30, 5),'Europe Sales') +
  labs(title = "European sales of Products based on Platform")

# Boxplot for Platform vs. North America Sales.
ggplot(data=df2,
       mapping=aes(x= NA_Sales, y= Platform)) +
  geom_boxplot(fill ='green') +
  scale_x_continuous(breaks = seq(0, 35, 5),'North America Sales') +
  labs(title = "North American sales of Products based on Platform")

## From the boxplot comparing Platform vs. Global Sales, the PS, PS2, PS3,
## and PS4 all have similar median sales for games. The outlier is the PS5,
## perhaps this is due to the fact that it's a newer console and hasn't had the
## same amount of time as legacy platforms(PS2,PS3 and PS4) to sell games and it
## likely has a limited library of games + a newer console is likely more
## expensive than older platforms and less affordable to consumers.

## For the Platform vs. North American Sales boxplot:
## It was also noted that median PlayStation sales fell with every successive
## console release. Median of PS4< Median of PS3< Median of PS2< Median of PS. 
## This decline in median PlayStation sales may be down to Microsoft providing
## competition with its xbox consoles and North American consumers favouring
## a domestic product over its international counterparts.

## In the Platform vs. European sales, the median sales was highest for the PS4.


# Create barplot to show the # of products listed for sale in each platform.
ggplot(data = df2, aes(x = Platform)) +
  geom_bar(fill = "blue", color = "black") +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    vjust = -0.5,  # Adjust vertical position of labels
    color = "black",
    size = 3) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  labs(x = "Platform",
       y = "Count",
       title = "The number of products per platform")

## The top 5 platforms with the most products are Xbox360, PS3, PC, Wii and DS.

###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
head(df2)

## Check output: Determine the min, max, and mean values.
# For the North America Sales.
min(df2$NA_Sales)
max(df2$NA_Sales)
mean(df2$NA_Sales)

# For the European Sales.
min(df2$EU_Sales)
max(df2$EU_Sales)
mean(df2$EU_Sales)

# For the Global Sales.
min(df2$Global_Sales)
max(df2$Global_Sales)
mean(df2$Global_Sales)

# View the descriptive statistics.
summary(df2)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
df3 <- df2 %>% group_by(Product) %>%
  summarise(North_America_Sales=sum(NA_Sales),
            Europe_Sales=sum(EU_Sales),
            Global_Sales=sum(Global_Sales),
            .groups='drop')

# View the data frame.
head(df3)
View(df3)

# Explore the data frame.
dim(df3)
str(df3)
summary(df3)

## 2b) Determine which plot is the best to compare game sales.
## Create scatterplots.

# Europe Sales vs. North America Sales.
ggplot(data=df3,
       mapping=aes(x= Europe_Sales, y= North_America_Sales)) +
  geom_point(color = 'red',
             alpha = 0.5,
             size = 1.5) +
  scale_x_continuous(breaks = seq(0, 25, 5),'Europe Sales (£)') +
  scale_y_continuous(breaks = seq(0, 35, 5), 'North America Sales (£)') +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) +
  labs(title = "Relationship between European and North American sales")

# Europe Sales vs. Global Sales.
ggplot(data=df3,
       mapping=aes(x= Europe_Sales, y= Global_Sales)) +
  geom_point(color = 'red',
             alpha = 0.5,
             size = 1.5) +
  scale_x_continuous(breaks = seq(0, 25, 5),'Europe Sales (£)') +
  scale_y_continuous(breaks = seq(0, 70, 5), 'Global Sales (£)') +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) +
  labs(title = "Relationship between European and Global sales")

# North America vs. Global Sales.
ggplot(data=df3,
       mapping=aes(x= North_America_Sales, y= Global_Sales)) +
  geom_point(color = 'red',
             alpha = 0.5,
             size = 1.5) +
  scale_x_continuous(breaks = seq(0, 35, 5),'North America Sales (£)') +
  scale_y_continuous(breaks = seq(0, 70, 5), 'Global Sales (£)') +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) +
  labs(title = "Relationship between North American Sales and Global sales")

## Create histograms.

# For the European Sales.
ggplot(df3, aes(x = Europe_Sales, y = ..count../sum(..count..))) +
  geom_histogram(fill = 'red', color = 'black') + 
  scale_x_continuous(breaks = seq(0, 25, 2.5)) +
  labs(x = "Sales in $M",
       y = "Percent",
       title = "Europe Sales by Percent") +
  scale_y_continuous(label = scales::percent) 

# For the North American Sales.
ggplot(df3, aes(x = North_America_Sales, y = ..count../sum(..count..))) +
  geom_histogram(fill = 'red', color = 'black') +
  scale_x_continuous(breaks = seq(0, 35, 2.5)) +
  labs(x = "Sales in $M",
       y = "Percent",
       title = "North American Sales by Percent") +
  scale_y_continuous(label = scales::percent) 

# For the Global Sales.
ggplot(df3, aes(x = Global_Sales, y = ..count../sum(..count..))) +
  geom_histogram(fill = 'red', color = 'black') + 
  scale_x_continuous(breaks = seq(0, 70, 5)) +
  labs(x = "Sales ($M)",
       y = "Percent",
       title = "Global Sales by Percent") +
  scale_y_continuous(label = scales::percent) 

## Create boxplot.

# Side-by-side boxplot comparing sales in different regions - df created.
side_box <- ggplot(df3, aes(x = factor(1), y = Global_Sales)) +
  geom_boxplot(aes(fill = "Global_Sales"),
               position = position_dodge(width = 0.75)) +
  geom_boxplot(aes(x = factor(2), y = North_America_Sales,
                   fill = "American Sales"),
               position = position_dodge(width = 0.75)) +
  geom_boxplot(aes(x = factor(3), y = Europe_Sales, fill = "Europe Sales"),
               position = position_dodge(width = 0.75)) +
  scale_y_continuous(breaks = seq(0, 70, 5)) +
  labs(fill = "Sales Region",
       y = "Total Sales (£M)",
       title = "Total Sales by Region") +
  # Remove x-axis title and labels.
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  # Set x-axis labels
  scale_x_discrete(labels = c("Global Sales", "Europe Sales", "American Sales"))
# View the boxplot with an interactive element.
ggplotly(side_box)

## After data has been grouped by product id, European game sales rarely exceed
## £10 million, North American game sales rarely £15 million and global game
## sales rarely exceed £30 million.
## All three regional sales after grouping by product id has a positive skew.

###############################################################################


# 3. Determine the normality of the data set.
## Normality be determined on two data sets: the original sales data in df2
## and the sales data that has been grouped by product id in df3.

## 3a) Create Q-Q Plots
## Create Q-Q Plots.

# For European Sales in df3(Sales grouped by product id).
qqnorm(df3$Europe_Sales,
       col='blue',
       xlab="z Value",
       ylab='European Sales ($M)')
# Specify the qqline function.
# Add a reference line to the qqplot. 
qqline(df3$Europe_Sales,
       col='red',
       lwd=2) 

# For North American Sales in df3(Sales grouped by product id).
qqnorm(df3$North_America_Sales,
       col='blue',
       xlab="z Value",
       ylab='North American Sales ($M)')
# Specify the qqline function.
# Add a reference line to the qqplot. 
qqline(df3$North_America_Sales,
       col='red',
       lwd=2) 

# For Global Sales in df3(Sales grouped by product id).
qqnorm(df3$Global_Sales,
       col='blue',
       xlab="z Value",
       ylab='Global Sales ($M)')
# Specify the qqline function.
# Add a reference line to the qqplot. 
qqline(df3$Global_Sales,
       col='red',
       lwd=2) 

## For all three qq-plots above, there appears to be a positive correlation.
## However, data appears to be skewed and not have a normal distribution.

# For European Sales in original sales data (df2).
qqnorm(df2$EU_Sales,
       col='blue',
       xlab="z Value",
       ylab='European Sales ($M)')
# Specify the qqline function.
# Add a reference line to the qqplot. 
qqline(df2$EU_Sales,
       col='red',
       lwd=2) 

# For North American Sales in original sales data(df2).
qqnorm(df2$NA_Sales,
       col='blue',
       xlab="z Value",
       ylab='North American Sales ($M)')
# Specify the qqline function.
# Add a reference line to the qqplot. 
qqline(df2$NA_Sales,
       col='red',
       lwd=2) 

# For Global Sales in original sales data(df2).
qqnorm(df2$Global_Sales,
       col='blue',
       xlab="z Value",
       ylab='Global Sales ($M)')
# Specify the qqline function.
# Add a reference line to the qqplot. 
qqline(df2$Global_Sales,
       col='red',
       lwd=2) 

## In the three qq-plots above, there is a positive correlation.
## However, the data appears skewed and not normally distributed.

## 3b) Perform Shapiro-Wilk test.
# Install and import Moments.
install.packages('moments')
library (moments)

# Perform Shapiro-Wilk test for all three sales columns.
# in the data grouped by product id (df3).

# For the European Sales.
shapiro.test(df3$Europe_Sales)

# For the North American Sales.
shapiro.test(df3$North_America_Sales)

# For the North American Sales.
shapiro.test(df3$Global_Sales)

# Perform Shapiro-Wilk test for all three sales columns
# in the original sales data (df2).

# For the European Sales.
shapiro.test(df2$EU_Sales)

# For the North American Sales.
shapiro.test(df2$NA_Sales)

# For the North American Sales.
shapiro.test(df2$Global_Sales)

## The Shapiro-Wilk test has a p value of under 0.05 for all regions in 
## both data sets. By assuming a size of test (alpha = 0.05) of 5%,
## the assumption/hypothesis of normality can be rejected for both data sets.

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis for both data sets.

# For the European Sales in the grouped data (df3).
skewness(df3$Europe_Sales) 
kurtosis(df3$Europe_Sales)

# For the North American Sales in the grouped data (df3).
skewness(df3$North_America_Sales) 
kurtosis(df3$North_America_Sales)

# For the Global Sales in the grouped data (df3).
skewness(df3$Global_Sales) 
kurtosis(df3$Global_Sales)

# For the European Sales in the original data (df2).
skewness(df2$EU_Sales) 
kurtosis(df2$EU_Sales)

# For the North American Sales in the original data (df2).
skewness(df2$NA_Sales) 
kurtosis(df2$NA_Sales)

# For the Global Sales in the original data (df2).
skewness(df2$Global_Sales) 
kurtosis(df2$Global_Sales)

## For all three regions in both data sets, there is a positive skew.
## The kurtosis values for all three regions in both data sets exceeds 3.
## Therefore, all three regions in both data sets have distributions with 
## fatter tails than a normal distribution. It also has a higher peak than a 
## normal distribution.
## The data in the original sales data(df2) is more skewed and leptokurtic
## than the data in the grouped data(df3).

## 3d) Determine correlation
# Determine correlation.

# Create a new dataframe with only the sales data from the grouped data(df3).
Sales_only_grouped = select(df3, -Product)
head(Sales_only_grouped)

# Determine correlation between all sales data from the grouped data(df3).
round (cor(Sales_only_grouped),
       digits=2)

# Create a new dataframe with only the sales data from the original data(df2).
Sales_only = select(df2, -Product, -Platform)
head(Sales_only)

# Determine correlation between all sales data from the original data(df2).
round (cor(Sales_only),
       digits=2)

## In both data sets, there is a strong positive correlation between 
## North American Sales and global sales.

###############################################################################

# 4. Plot the data (SEE SCATTERPLOTS IN 2b OF ASSIGNMENT 5).
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

## The scatterplots created in part 2b) of Assignment 5 best represent the 
## sales data for different regions. The general trend is one where if sales
## of a product increases in one region then it'll increase in other regions.

## The sales data was very positively skewed.

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
head(Sales_only)

# Determine a summary of the data frame.
summary(Sales_only)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
cor(Sales_only)

# Plot the relationships of Sales with base R graphics.
plot(Sales_only$EU_Sales, Sales_only$NA_Sales)
plot(Sales_only$EU, Sales_only$Global_Sales)
plot(Sales_only$NA_Sales, Sales_only$Global_Sales)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.

## For European vs. North American sales.

# Create a simple linear regression model.
model_EU_NA = lm(NA_Sales~EU_Sales,
                 data=Sales_only)

# View the model.
model_EU_NA

# View more outputs for the model - the full regression table.
summary(model_EU_NA)

# View residuals on a plot.
plot(model_EU_NA$residuals)

# Fit simple linear regression model.
plot(Sales_only$EU_Sales, Sales_only$NA_Sales)
coefficients(model_EU_NA)

# Add line-of-best-fit.
abline(coefficients(model_EU_NA))

## For European vs. Global sales.

# Create a simple linear regression model.
model_EU_GL = lm(Global_Sales~EU_Sales,
                 data=Sales_only)

# View the model.
model_EU_GL

# View more outputs for the model - the full regression table.
summary(model_EU_GL)

# View residuals on a plot.
plot(model_EU_GL$residuals)

# Fit simple linear regression model.
plot(Sales_only$EU_Sales, Sales_only$Global_Sales)
coefficients(model_EU_GL)

# Add line-of-best-fit.
abline(coefficients(model_EU_GL))

## For North American vs. Global sales.

# Create a simple linear regression model.
model_NA_GL = lm(Global_Sales~NA_Sales,
                 data=Sales_only)

# View the model.
model_NA_GL

# View more outputs for the model - the full regression table.
summary(model_NA_GL)

# View residuals on a plot.
plot(model_NA_GL$residuals)

# Fit simple linear regression model.
plot(Sales_only$NA_Sales, Sales_only$Global_Sales)
coefficients(model_NA_GL)

# Add line-of-best-fit.
abline(coefficients(model_NA_GL))

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
# Multiple linear regression model.

# Install the psych package.
install.packages('psych')

# Import the psych package.
library(psych)

# Use the corPlot() function.
# Specify the data frame (Sales_only) and set 
# character size (cex=2).
corPlot(Sales_only, cex=2)

# Create multiple linear regression model.
model_Multi_Sales = lm(Global_Sales~EU_Sales+NA_Sales,
                       data=Sales_only)

# Print the summary statistics.
summary(model_Multi_Sales)

# View residuals on a plot.
plot(model_Multi_Sales)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

## Part a.
# Create dataframe for part a. 
test_a = data.frame(EU_Sales = 23.80, NA_Sales = 34.02)

# View the data
test_a

# Predicted value for part a.
PredictTest_a = predict(model_Multi_Sales, newdata=test_a,
                        interval='confidence')

# View predicted value of a.
PredictTest_a

# Observed value of a.
Observed_a = subset(Sales_only,
                    EU_Sales == 23.80 & NA_Sales == 34.02)

Observed_a

## The observed value of part a is 67.85, this is slightly outside the 
## confidence interval for the predicted value of a which is 71.47.

## Part b.
# Create dataframe for part b. 
test_b = data.frame(EU_Sales = 1.56, NA_Sales = 3.93)

# View the data
test_b

# Predicted value for part b.
PredictTest_b = predict(model_Multi_Sales, newdata=test_b,
                        interval='confidence')

# View predicted value of b.
PredictTest_b

# Observed value of b.
Observed_b = subset(Sales_only,
                    EU_Sales == 1.56 & NA_Sales == 3.93)

Observed_b

## The observed value of part b is 6.04, this is lower than the
## confidence interval for the predicted value of b 6.86.

## Part c.
# Create dataframe for part c. 
test_c = data.frame(EU_Sales = 0.65, NA_Sales = 2.73)

# View the data
test_c

# Predicted value for part c.
PredictTest_c = predict(model_Multi_Sales, newdata=test_c,
                        interval='confidence')

# View predicted value of c.
PredictTest_c

# Observed value of c.
Observed_c = subset(Sales_only,
                    EU_Sales == 0.65 & NA_Sales == 2.73)

Observed_c

## The observed value of part c is 4.32, this is within the confidence interval
## for the predicted value of c (4.10-4.39).

## Part d.
# Create dataframe for part d. 
test_d = data.frame(EU_Sales = 0.97, NA_Sales = 2.26)

# View the data
test_d

# Predicted value for part d.
PredictTest_d = predict(model_Multi_Sales, newdata=test_d,
                        interval='confidence')

# View predicted value of d.
PredictTest_d

# Observed value of d.
Observed_d = subset(Sales_only,
                    EU_Sales == 0.97 & NA_Sales == 2.26)

Observed_d

## The observed value of part d is 3.53, this is slightly below the lower
## value of the confidence interval for the predicted value of d(4.13).

## Part e.
# Create dataframe for part e. 
test_e = data.frame(EU_Sales = 0.52, NA_Sales = 22.08)

# View the data
test_e

# Predicted value for part e.
PredictTest_e = predict(model_Multi_Sales, newdata=test_e,
                        interval='confidence')

# View predicted value of e.
PredictTest_e

# Observed value of e.
Observed_e = subset(Sales_only,
                    EU_Sales == 0.52 & NA_Sales == 22.08)

Observed_e

## The observed value of part d is 23.21, this is slightly below the lower
## value of the confidence interval for the predicted value of e(25.41).


###############################################################################

# 5. Observations and insights
# Your observations and insights here...
## For the North American sales vs. global sales, the R-squared value was 87%.
## North American sales are able to explain 87% of the variability in global
## sales.
## For the European sales vs. global sales, the R-squared value was 77%.
## The European sales are able to explain 77% of the variability of the 
## Global sales. 


## For the values that were tested using the multiple linear regression model,
## most were slightly outside the confidence interval but were reasonably close.
## The adjusted R-squared value for the multiple linear regression model was
## 0.9685 so the x variables are able to explain almost 97% of the variability
## of the Global sales.

## North American sales and European Sales were very strongly positively
## correlated with global sales - correlation values were 0.93 and 0.88
## respectively. European sales and North American sales were also positively
## correlated but to a lesser magnitude with a correlation value of 0.71.



###############################################################################
###############################################################################
