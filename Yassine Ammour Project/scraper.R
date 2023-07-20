library(rvest)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringi)
library(httr)
library(glue)


# STEP 01 : Collect the Links from main web pages: ======================================================
my_agent <- "MyCustomAgent/1.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.5414.120 Safari/537.36"

no_page <- 1:3511

url_pages <- glue("https://www.guru.com/d/freelancers/c/programming-development/sc/math-algorithms-analytics/ssc/data-analysis/pg/{no_page}/")
df_url <- data.frame(url_pages = NULL, scrape_time = NULL)


for (i in seq_along(url_pages)) {
  page_main <- GET(url_pages[i], user_agent(my_agent))
  page_content <- content(page_main)
  url_provider <- page_content %>%
    html_elements("div.avatarinfo a")%>%
    html_attr("href")%>%
    stri_c("https://www.guru.com", .)
  scrape_time <- format(Sys.time(), "%D %X")
  df_temp <- data.frame(url_provider, scrape_time)
  df_url <- bind_rows(df_url, df_temp)
  print(glue("Iteration: {i} | Time: {scrape_time}"))
  #Sys.sleep(1)
}



# STEP 2: Collect The Data from Each Link: ===============================================================
df_data <- data.frame(url_pages = NULL, scrape_time = NULL)

for (i in seq_along(df_url[,1])) {
  page_provider <- GET(df_url[[i,1]], user_agent(my_agent))    
  provider_content <- content(page_provider)
  
  # 1.service provider name =============================================================================
  service_provider_name <- provider_content %>%
    html_elements("div.profile-avatar__info h1")%>%
    html_text2()%>%
    stri_extract(regex = "(\\w+(?:\\s+\\w+)*)")
  
  # 2.The Country =======================================================================================
  country <- provider_content %>%  
    html_elements("div.profile-avatar__info p.profile-avatar__info__location")%>%
    html_text2() %>%
    stri_extract_all(regex = "(?<=,\\s)([A-Za-z]+)")
    country <-country[[1]][2]
  
  # 3.Service Rate ======================================================================================
    hourly_rate <- provider_content %>%
      html_elements("div.serviceListing__details > p.serviceListing__rates")%>%
      html_text2() %>%
      stri_extract(regex = "(\\d+)")%>%
      as.numeric() %>%
      mean()%>%
      stri_extract(regex = "^(\\d+)(?:\\.(\\d))?")%>%
      as.numeric()
    
  # 4.top skills  ========================================================================================
  top_skills <- provider_content %>%
    html_elements("div.p-box__item div.skillsList")%>%
    html_text2()%>%
    stri_extract(regex = "(\\w+(?:\\s+\\w+)*)")
    top_skills <-top_skills[1]
  
  # 5.Yearly Earnings ====================================================================================
  yearly_earnings <- provider_content %>%  
    html_elements("div.profile-avatar__info p.profile-avatar__info__earnings strong")%>%
    html_text2() 
    yearly_earnings <-yearly_earnings[1]%>%  
    stri_extract(regex ="[0-9,]+")
    
  # 6.Ratings ============================================================================================
  ratings <- provider_content %>%  
    html_elements("div.profile-avatar__info p.profile-avatar__info__earnings strong")%>%
    html_text2() 
    ratings <-ratings[2]
  
  # 7.All-Time Earnings ==================================================================================
  all_time_earnings <- provider_content %>%  
    html_elements("div.p-box__item dl.profile-attd")%>%
    html_elements("dd.profile-attd__data")%>%
    html_text() 
    all_time_earnings <-all_time_earnings[1]
  
  # 8.No of Transactions =================================================================================
  transactions <- provider_content %>%  
    html_elements("div.p-box__item dl.profile-attd")%>%
    html_elements("dd.profile-attd__data")%>%
    html_text() 
    transactions <-transactions[2]
  
  # 9.No of Employers ====================================================================================
  employers <- provider_content %>%  
    html_elements("div.p-box__item dl.profile-attd")%>%
    html_elements("dd.profile-attd__data")%>%
    html_text() 
    employers <-employers[3]%>%
    as.numeric()
  
  # 10.Largest Employer($) ==============================================================================
  largest_employer <- provider_content %>%  
    html_elements("div.p-box__item dl.profile-attd")%>%
    html_elements("dd.profile-attd__data")%>%
    html_text() 
    largest_employer <-largest_employer[4]
  
  # 11.Member Since =====================================================================================
  member_since <- provider_content %>%  
    html_elements("div.p-box__item dl.profile-attd")%>%
    html_elements("dd.profile-attd__data")%>%
    html_text() 
    member_since <-member_since[5]
  
  scrape_time <- format(Sys.time(), "%D %X")
  df_temp <- data.frame(df_url[[i,1]],service_provider_name,country,hourly_rate,yearly_earnings,all_time_earnings,ratings,transactions,employers,largest_employer,top_skills,member_since,scrape_time)
  df_data <- bind_rows(df_data, df_temp)
  print(glue("Iteration: {i} | Time: {scrape_time}"))
  #Sys.sleep(1)
}

# Saving the Scraped Data :
write.csv(df_url, "df_url.csv", row.names = FALSE)
write.csv(df_data, "scraped_data.csv", row.names = FALSE)



# Step 3: Data Processing :=============================================================================
df_data <- read.csv("scraped_data.csv")
#df_data <- df_data[, -1]

#  Delete missing values ============================================================
df_data[, 5] <- ifelse(is.na(df_data[, 5]), "0", df_data[, 5])
df_data[, 7] <- ifelse(is.na(df_data[, 7]), "0", df_data[, 7])

# Change to Numeric values ==========================================================
df_data[, 5] <- stri_replace_all(df_data[,5],regex = ",", replace = "")%>%
  as.numeric()
df_data[, 7] <- stri_replace_all(df_data[,7],regex = "%", replace = "")%>%
  as.numeric()
df_data[, 8] <- stri_replace_all(df_data[,8],regex = ",", replace = "")%>%
  as.numeric()

df_data[, 6] <- stri_replace_all(df_data[, 6], regex = "\\$|K|(?<=[0-9])\\.(?=[0-9])",replace = "")
df_data[, 6] <-  stri_replace_all(df_data[, 6], regex = "M", replace = "00")
df_data[, 6] <-  stri_replace_all(df_data[, 6], regex = ",",replace = ".")
df_data[, 6] <-  as.numeric(stri_extract(df_data[, 6], regex ="[0-9,]+"))*1000
  
df_data[, 10] <- stri_replace_all(df_data[, 10], regex = "\\$|K|(?<=[0-9])\\.(?=[0-9])",replace = "")
df_data[, 10] <-  stri_replace_all(df_data[, 10], regex = "M", replace = "00")
df_data[, 10] <-  stri_replace_all(df_data[, 10], regex = ",",replace = ".")
df_data[, 10] <-  as.numeric(stri_extract(df_data[, 10], regex ="[0-9,]+"))

# Am deleting columns of  : URL, skills, scraping time:
df_data <- df_data[, -1]
#df_data <- df_data[, -11]
df_data <- df_data[, -12]




# Check the class :
sapply(df_data, class)

# Save the cleaned data :
write.csv(df_data, "data_to_analyse.csv", row.names = FALSE)

# This data is Now Clean and Ready for Analysis:
df_data <- df_data <- read.csv("data_to_analyse.csv")


# STEP 4: PLOTTING AND ANALYSIS :===============================================================================

# Density Plot of Hourly Rate :=================================================================================
ggplot(df_data, aes(x = hourly_rate)) +
  geom_density() +
  xlab("Hourly Rate") +
  ggtitle("Density Plot of Hourly Rate")





# Top 10 countries based on total yearly earnings :================================================
top_10_countries <- df_data %>%
  group_by(country) %>%
  summarize(total_yearly_earnings = sum(yearly_earnings)) %>%
  arrange(desc(total_yearly_earnings)) %>%
  slice(1:10)

ggplot(top_10_countries, aes(x = reorder(country, total_yearly_earnings), y = total_yearly_earnings)) +
  geom_bar(stat = "identity") +
  xlab("Country") +
  ylab("Total Yearly Earnings") +
  ggtitle("Top 10 Countries based on Total Yearly Earnings") +
  coord_flip()





# Top 10 countries based on number of transactions :===============================================
top_10_countries <- df_data %>%
  group_by(country) %>%
  summarize(total_all_time_earnings = sum(all_time_earnings)) %>%
  arrange(desc(total_all_time_earnings)) %>%
  slice(1:10)

ggplot(top_10_countries, aes(x = reorder(country, total_all_time_earnings), y = total_all_time_earnings)) +
  geom_bar(stat = "identity") +
  xlab("Country") +
  ylab("Total transactions") +
  ggtitle("Top 10 Countries based on Total Number of Transactions") +
  coord_flip()





#Top 10 Countries by Number of Service Providers :======================================================

# Aggregate the data by country
agg_data <- df_data %>%
  group_by(country) %>%
  summarize(num_service_providers = n_distinct(service_provider_name))

# Rank the countries based on the number of service providers
agg_data <- agg_data %>%
  arrange(desc(num_service_providers))

# Select the top 10 countries
top_10_countries <- head(agg_data, 10)

ggplot(top_10_countries, aes(x = reorder(country, num_service_providers), y = num_service_providers)) +
  geom_bar(stat = "identity") +
  xlab("Country") +
  ylab("Number of Service Providers") +
  ggtitle("Top 10 Countries by Number of Service Providers") +
  coord_flip()





#Top 10 Countries with Returning Customers :============================================================

# Aggregate the data by country
agg_data <- df_data %>%
  group_by(country) %>%
  summarize(mean_employers = mean(employers),
            mean_transactions = mean(transactions))

# Rank the countries based on the mean number of transactions per employer
agg_data <- agg_data %>%
  mutate(returning_customers = mean_transactions / mean_employers) %>%
  arrange(desc(returning_customers))

# Select the top 10 countries
top_10_countries <- head(agg_data, 10)

ggplot(top_10_countries, aes(x = reorder(country, returning_customers), y = returning_customers)) +
  geom_bar(stat = "identity") +
  xlab("Country") +
  ylab("Mean Number of Transactions per Employer") +
  ggtitle("Top 10 Countries with Returning Customers") +
  coord_flip()





#Top 10 Countries with the Highest Transaction Prices :================================================= 
top_1000 <- head(df_data[order(df_data$ratings, decreasing = TRUE), ], 1000)

# Aggregate the data by country
agg_data <- top_1000 %>%
  group_by(country) %>%
  summarize(sum_earnings = sum(all_time_earnings),
            sum_transactions = sum(transactions))

# Calculate the average transaction price for each country
agg_data <- agg_data %>%
  mutate(avg_transaction_price = sum_earnings / sum_transactions) %>%
  arrange(desc(avg_transaction_price))

# Select the top 10 countries
top_10_countries <- head(agg_data, 10)

ggplot(top_10_countries, aes(x = reorder(country, avg_transaction_price), y = avg_transaction_price)) +
  geom_bar(stat = "identity") +
  xlab("Country") +
  ylab("Average Transaction Price") +
  ggtitle("Top 10 Countries with the Highest Transaction Prices") +
  coord_flip()






# Members Date of joining from each country of the Top 10 :============================================ 

# Convert the member_since column to a date object "year"
df_data$year <- as.numeric(str_extract(df_data$member_since, "\\d+"))

# Aggregate the data by country and year
agg_data <- df_data %>%
  group_by(country, year) %>%
  summarize(num_service_providers = n_distinct(service_provider_name))

# Define the list of countries
countries <- c("USA", "India", "Pakistan", "Bangladesh", "United", "Philippines", "Egypt", "Canada", "Indonesia", "Ukraine")

# Extract only the rows with the specified countries
data_subset <- subset(agg_data, country %in% countries)

# Create a line plot with year on the x-axis, num_service_providers on the y-axis, and color by country
ggplot(data_subset, aes(x=year, y=num_service_providers, color=country)) +
  geom_line() +
  scale_x_continuous(limits=c(min(data_subset$year), max(data_subset$year))) +
  labs(x="Year", y="Number of Service Providers", color="Country") +
  ggtitle("Number of Service Providers by Year and Country")


