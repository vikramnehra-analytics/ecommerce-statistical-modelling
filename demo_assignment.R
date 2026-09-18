
install.packages(c("readxl", "dplyr", "ggplot2", "tidyr", "lubridate", "psych", "car", "effectsize", "writexl" ))


library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(psych)
library(car)
library(effectsize)
library(writexl)

getwd()

list.files()

#Data
 
retail <- read_csv("C:/Users/Abhinav Saxena/OneDrive/Desktop/Abhinav Projects/R Project/Online Retail.xlsx")

# Check the data
head(retail)
str(retail)
dim(retail)
names(retail)

#INITIAL DATA EXPLORATION

# Number of rows and columns
cat("Number of rows:", nrow(retail), "\n")
cat("Number of columns:", ncol(retail), "\n")

# Summary
summary(retail)

# Missing values
missing_values <- colSums(is.na(retail))
print(missing_values)

# Duplicate rows
duplicate_rows <- sum(duplicated(retail))
cat("Duplicate rows:", duplicate_rows, "\n")

#DATA TYPE CONVERSION

retail <- retail %>%
  mutate(
    InvoiceNo = as.character(InvoiceNo),
    StockCode = as.character(StockCode),
    Description = as.character(Description),
    Country = as.character(Country)
  )


# CHECK CANCELLATIONS
retain_clean

#DATA CLEANING

retail_clean <- retail %>%
  filter(
    !grepl("^C", InvoiceNo),
    Quantity > 0,
    UnitPrice > 0,
    !is.na(CustomerID)
  )


# Check cleaned dimensions
cat(
  "Rows after cleaning:",
  nrow(retail_clean),
  "\n"
)

#CREATE REVENUE VARIABLE

retail_clean <- retail_clean %>%
  mutate(
    Revenue = Quantity * UnitPrice
  )


# Inspect revenue
summary(retail_clean$Revenue)

#BASIC DESCRIPTIVE STATISTICS

revenue_statistics <- retail_clean %>%
  summarise(
    N = n(),
    Mean = mean(Revenue),
    Median = median(Revenue),
    SD = sd(Revenue),
    Minimum = min(Revenue),
    Q1 = quantile(Revenue, 0.25),
    Q3 = quantile(Revenue, 0.75),
    Maximum = max(Revenue)
  )

print(revenue_statistics)

#REVENUE HISTOGRAM

ggplot(retail_clean, aes(x = Revenue)) +
  geom_histogram(
    bins = 100
  ) +
  labs(
    title = "Distribution of Transaction Revenue",
    x = "Revenue (£)",
    y = "Frequency"
  ) +
  theme_minimal()

#REVENUE BOXPLOT

ggplot(retail_clean, aes(y = Revenue)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Transaction Revenue",
    y = "Revenue (£)"
  ) +
  theme_minimal()

#COUNTRY-LEVEL TRANSACTION SUMMARY

country_summary <- retail_clean %>%
  group_by(Country) %>%
  summarise(
    Transactions = n(),
    Revenue = sum(Revenue),
    MeanRevenue = mean(Revenue),
    MedianRevenue = median(Revenue),
    .groups = "drop"
  ) %>%
  arrange(desc(Revenue))

print(country_summary)

#TOP 10 COUNTRIES BY REVENUE

top_countries <- country_summary %>%
  slice_head(n = 10)

print(top_countries)

#VISUALISE TOP COUNTRIES

ggplot(
  top_countries,
  aes(
    x = reorder(Country, Revenue),
    y = Revenue
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Revenue",
    x = "Country",
    y = "Revenue (£)"
  ) +
  theme_minimal()

#CUSTOMER-LEVEL DATA

customer_data <- retail_clean %>%
  group_by(CustomerID, Country) %>%
  summarise(
    CustomerSpend = sum(Revenue),
    Transactions = n_distinct(InvoiceNo),
    Items = sum(Quantity),
    .groups = "drop"
  )

# Inspect
head(customer_data)

# Number of customers
cat(
  "Number of unique customers:",
  nrow(customer_data),
  "\n"
)

#CUSTOMER SUMMARY BY COUNTRY

customer_country_summary <- customer_data %>%
  group_by(Country) %>%
  summarise(
    Customers = n(),
    MeanSpend = mean(CustomerSpend),
    MedianSpend = median(CustomerSpend),
    SDSpend = sd(CustomerSpend),
    .groups = "drop"
  ) %>%
  arrange(desc(MeanSpend))

print(customer_country_summary)

#SELECT UK AND GERMANY

comparison <- customer_data %>%
  filter(
    Country %in% c(
      "United Kingdom",
      "Germany"
    )
  )

print(
  comparison %>%
    count(Country)
)

#CUSTOMER POPULATION VISUALISATION

customer_counts <- comparison %>%
  count(Country, name = "Customers")

ggplot(
  customer_counts,
  aes(
    x = Country,
    y = Customers
  )
) +
  geom_col() +
  labs(
    title = "Number of Customers: UK vs Germany",
    x = "Country",
    y = "Number of Customers"
  ) +
  theme_minimal()

# RAW CUSTOMER SPENDING VISUALISATION

ggplot(
  comparison,
  aes(
    x = Country,
    y = CustomerSpend
  )
) +
  geom_boxplot() +
  labs(
    title = "Customer Spending: UK vs Germany",
    x = "Country",
    y = "Customer Spending (£)"
  ) +
  theme_minimal()

#RAW CUSTOMER DESCRIPTIVE STATISTICS

raw_customer_stats <- comparison %>%
  group_by(Country) %>%
  summarise(
    N = n(),
    Mean = mean(CustomerSpend),
    Median = median(CustomerSpend),
    SD = sd(CustomerSpend),
    Minimum = min(CustomerSpend),
    Q1 = quantile(CustomerSpend, 0.25),
    Q3 = quantile(CustomerSpend, 0.75),
    Maximum = max(CustomerSpend),
    .groups = "drop"
  )

print(raw_customer_stats)


#STRATIFIED RANDOM SAMPLE

set.seed(105)

sample_data <- comparison %>%
  group_by(Country) %>%
  slice_sample(prop = 0.50) %>%
  ungroup()

# Check sample size
sample_counts <- sample_data %>%
  count(Country, name = "SampleSize")

print(sample_counts)

#LOG TRANSFORMATION

sample_data <- sample_data %>%
  mutate(
    log_spend = log1p(CustomerSpend)
  )

summary(sample_data$log_spend)


# DESCRIPTIVE STATISTICS AFTER TRANSFORMATION

log_statistics <- sample_data %>%
  group_by(Country) %>%
  summarise(
    N = n(),
    Mean = mean(log_spend),
    Median = median(log_spend),
    SD = sd(log_spend),
    Minimum = min(log_spend),
    Q1 = quantile(log_spend, 0.25),
    Q3 = quantile(log_spend, 0.75),
    Maximum = max(log_spend),
    .groups = "drop"
  )

print(log_statistics)

#LOG-SPENDING HISTOGRAM

ggplot(
  sample_data,
  aes(
    x = log_spend,
    fill = Country
  )
) +
  geom_histogram(
    bins = 30,
    alpha = 0.6,
    position = "identity"
  ) +
  labs(
    title = "Distribution of Log Customer Spending",
    x = "log(Customer Spending + 1)",
    y = "Frequency"
  ) +
  theme_minimal()

#LOG-SPENDING BOXPLOT

ggplot(
  sample_data,
  aes(
    x = Country,
    y = log_spend
  )
) +
  geom_boxplot() +
  labs(
    title = "Log Customer Spending by Country",
    x = "Country",
    y = "log(Customer Spending + 1)"
  ) +
  theme_minimal()

#CHECK NORMALITY VISUALLY - Q-Q PLOT

ggplot(
  sample_data,
  aes(
    sample = log_spend
  )
) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Country) +
  labs(
    title = "Q-Q Plots of Log Customer Spending",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()



uk_log <- sample_data %>%
  filter(Country == "United Kingdom") %>%
  pull(log_spend)

germany_log <- sample_data %>%
  filter(Country == "Germany") %>%
  pull(log_spend)


shapiro_uk <- shapiro.test(uk_log)
shapiro_germany <- shapiro.test(germany_log)

print(shapiro_uk)
print(shapiro_germany)

#VARIANCE COMPARISON

variance_summary <- sample_data %>%
  group_by(Country) %>%
  summarise(
    Variance = var(log_spend),
    SD = sd(log_spend),
    .groups = "drop"
  )

print(variance_summary)

# LEVENE'S TEST

levene_result <- leveneTest(
  log_spend ~ Country,
  data = sample_data
)

print(levene_result)

# HYPOTHESIS TEST


welch_result <- t.test(
  log_spend ~ Country,
  data = sample_data,
  var.equal = FALSE,
  conf.level = 0.95
)

print(welch_result)


# EFFECT SIZE


effect_result <- cohens_d(
  log_spend ~ Country,
  data = sample_data,
  pooled_sd = FALSE
)

print(effect_result)


# NON-PARAMETRIC ROBUSTNESS TEST
mann_whitney_result <- wilcox.test(
  log_spend ~ Country,
  data = sample_data,
  exact = FALSE,
  conf.int = TRUE
)

print(mann_whitney_result)

# CALCULATE GROUP MEANS

group_means <- sample_data %>%
  group_by(Country) %>%
  summarise(
    N = n(),
    MeanLogSpend = mean(log_spend),
    MedianLogSpend = median(log_spend),
    SDLogSpend = sd(log_spend),
    .groups = "drop"
  )

print(group_means)

# BACK-TRANSFORM GROUP MEANS


back_transformed <- group_means %>%
  mutate(
    ApproxSpendScale = exp(MeanLogSpend) - 1
  )

print(back_transformed)


# CALCULATE BUSINESS COMPARISON


uk_mean <- group_means %>%
  filter(Country == "United Kingdom") %>%
  pull(MeanLogSpend)

germany_mean <- group_means %>%
  filter(Country == "Germany") %>%
  pull(MeanLogSpend)

log_difference <- germany_mean - uk_mean

spending_ratio <- exp(log_difference)

cat(
  "Germany minus UK log-scale difference:",
  log_difference,
  "\n"
)

cat(
  "Approximate multiplicative spending ratio:",
  spending_ratio,
  "\n"
)

# MONTHLY REVENUE ANALYSIS


retail_clean <- retail_clean %>%
  mutate(
    InvoiceDate = as.POSIXct(InvoiceDate),
    Month = floor_date(InvoiceDate, unit = "month")
  )

monthly_revenue <- retail_clean %>%
  group_by(Month) %>%
  summarise(
    Revenue = sum(Revenue),
    Transactions = n(),
    .groups = "drop"
  )

print(monthly_revenue)

# MONTHLY REVENUE VISUALISATION


ggplot(
  monthly_revenue,
  aes(
    x = Month,
    y = Revenue
  )
) +
  geom_line() +
  labs(
    title = "Monthly Revenue Trend",
    x = "Month",
    y = "Revenue (£)"
  ) +
  theme_minimal()

# MONTHLY TRANSACTION VISUALISATION

ggplot(
  monthly_revenue,
  aes(
    x = Month,
    y = Transactions
  )
) +
  geom_line() +
  labs(
    title = "Monthly Transaction Trend",
    x = "Month",
    y = "Number of Transactions"
  ) +
  theme_minimal()

# TOP PRODUCTS BY REVENUE


product_summary <- retail_clean %>%
  group_by(StockCode, Description) %>%
  summarise(
    Revenue = sum(Revenue),
    Quantity = sum(Quantity),
    Transactions = n_distinct(InvoiceNo),
    .groups = "drop"
  ) %>%
  arrange(desc(Revenue))

top_products <- product_summary %>%
  slice_head(n = 10)

print(top_products)

#TOP PRODUCTS VISUALISATION


ggplot(
  top_products,
  aes(
    x = reorder(Description, Revenue),
    y = Revenue
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Products by Revenue",
    x = "Product",
    y = "Revenue (£)"
  ) +
  theme_minimal()


# FINAL STATISTICAL SUMMARY


cat("\n")
cat("============================================\n")
cat("FINAL STATISTICAL SUMMARY\n")
cat("============================================\n")

cat(
  "UK sample size:",
  sum(sample_data$Country == "United Kingdom"),
  "\n"
)

cat(
  "Germany sample size:",
  sum(sample_data$Country == "Germany"),
  "\n"
)

cat(
  "Welch t statistic:",
  unname(welch_result$statistic),
  "\n"
)

cat(
  "Degrees of freedom:",
  unname(welch_result$parameter),
  "\n"
)

cat(
  "p-value:",
  welch_result$p.value,
  "\n"
)

cat(
  "95% confidence interval:\n"
)

print(welch_result$conf.int)

cat(
  "Cohen's d:\n"
)

print(effect_result)

cat(
  "Mann-Whitney p-value:",
  mann_whitney_result$p.value,
  "\n"
)

