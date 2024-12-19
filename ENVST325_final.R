# Load necessary libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(PerformanceAnalytics)

# Load datasets
co2_data <- read.csv("/cloud/project/co2_emissions_bigdata.csv")
energy_data <- read.csv("/cloud/project/energy_sources_bigdata.csv")

# Rename columns for clarity
colnames(co2_data) <- c("Country", "Year", "CO2_Emissions_per_Capita")
colnames(energy_data) <- c("Country", "Energy_Group", "Group_Technology", "Technology", "Technology_Sub", 
                           "Producer_Type", "Year", "Electricity_Generation_GWh", 
                           "Installed_Capacity_MW")

# Select relevant columns from energy data
energy_data_clean <- energy_data %>%
  select(Country, Energy_Group, Technology, Year, Electricity_Generation_GWh)

# Aggregate electricity generation by country, year, and energy group
energy_summary <- energy_data_clean %>%
  group_by(Country, Year, Energy_Group) %>%
  summarise(Electricity_Generation_GWh = sum(Electricity_Generation_GWh, na.rm = TRUE), .groups = 'drop')

# Ensure Year columns are integers
co2_data$Year <- as.integer(co2_data$Year)
energy_summary$Year <- as.integer(energy_summary$Year)

# Merge datasets
merged_data <- co2_data %>%
  inner_join(energy_summary, by = c("Country", "Year"))

# Pivot data to wide format
pivoted_data <- merged_data %>%
  pivot_wider(names_from = Energy_Group, values_from = Electricity_Generation_GWh) %>%
  mutate(across(c(`Total Renewable`, `Total Non-Renewable`), ~ replace_na(., 0)))

## Models for Brazil
brazil_data <- pivoted_data %>% filter(Country == "Brazil")
energy_model_brazil <- lm(`Total Non-Renewable` ~ `Total Renewable`, data = brazil_data)
emission_model_brazil <- lm(CO2_Emissions_per_Capita ~ `Total Renewable`, data = brazil_data)

summary(energy_model_brazil)
#positive significant correlation (p<0.001), with R^2=0.48
#for every unit increase in renewable energy, non-renewable energy increases by 0.255 units

summary(emission_model_brazil)
#p=0.078, so marginally significant relationship between renewable energy and emissions, but R^2=0.13

# Assumption checks for Brazil (1 = energy, 2 = emissions)
brazil_residuals1 <- rstandard(energy_model_brazil)
brazil_fitted1 <- fitted.values(emission_model_brazil)

brazil_residuals2 <- rstandard(emission_model_brazil)
brazil_fitted2 <- fitted.values(emission_model_brazil)

# Residuals vs Fitted plot for Brazil (1 = energy, 2 = emissions)
residuals_vs_fitted_brazil1 <- data.frame(Fitted = brazil_fitted1, Residuals = brazil_residuals1)
ggplot(residuals_vs_fitted_brazil1, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for Brazil",
       x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
##the residuals do not appear to be randomly distributed, but we will proceed 

residuals_vs_fitted_brazil2 <- data.frame(Fitted = brazil_fitted2, Residuals = brazil_residuals2)
ggplot(residuals_vs_fitted_brazil2, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for Brazil",
       x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
##the residuals for the model do not appear to be randomly distributed, but we will proceed 

# QQ plot for Brazil (1 = energy, 2 = emissions)
qq_plot_brazil1 <- data.frame(Residuals = brazil_residuals1)
ggplot(qq_plot_brazil1, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for Brazil",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
#the standardized residuals follow the QQline fairly well

qq_plot_brazil2 <- data.frame(Residuals = brazil_residuals2)
ggplot(qq_plot_brazil2, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for Brazil",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
#the standardized residuals do not follow the QQline very well, particularly at the extremes
#regardless, we proceed

# Shapiro-Wilk test for Brazil (1 = energy, 2 = emissions)
shapiro_test_brazil1 <- shapiro.test(brazil_residuals1)
print(shapiro_test_brazil1)
#p-value = .257, which meets the assumption of normality 

shapiro_test_brazil2 <- shapiro.test(brazil_residuals2)
print(shapiro_test_brazil2)
#p-value = .005, which does not meet assumption of normality
#we proceed with analysis, but note that this is not met, analysis beyond the scope of the course needed

## Models for Canada
canada_data <- pivoted_data %>% filter(Country == "Canada")
energy_model_canada <- lm(`Total Non-Renewable` ~ `Total Renewable`, data = canada_data)
emission_model_canada <- lm(CO2_Emissions_per_Capita ~ `Total Renewable`, data = canada_data)

summary(energy_model_canada)
#a strong, positive, and significant relationship (p<0.001) between renewable and non-renewable energy (R^2=0.67)
#a unit increase in renewable energy leads to a 0.461-unit increase in non-renewable energy

summary(emission_model_canada)

# Assumption checks for Canada (1 = energy, 2 = emissions)
canada_residuals1 <- rstandard(energy_model_canada)
canada_fitted1 <- fitted.values(emission_model_canada)

canada_residuals2 <- rstandard(emission_model_canada)
canada_fitted2 <- fitted.values(emission_model_canada)

# Residuals vs Fitted plot for Canada (1 = energy, 2 = emissions)
residuals_vs_fitted_canada1 <- data.frame(Fitted = canada_fitted1, Residuals = canada_residuals1)
ggplot(residuals_vs_fitted_canada1, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for Canada",
       x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
#residuals are not randomly distributed, very clustered 
#regardless, we proceed

residuals_vs_fitted_canada2 <- data.frame(Fitted = canada_fitted2, Residuals = canada_residuals2)
ggplot(residuals_vs_fitted_canada2, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for Canada",
       x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
#residuals are not randomly distributed, but we proceed 

# QQ plot for Canada (1 = energy, 2 = emissions)
qq_plot_canada1 <- data.frame(Residuals = canada_residuals1)
ggplot(qq_plot_canada1, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for Canada",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
#residuals follow the QQline fairly well

qq_plot_canada2 <- data.frame(Residuals = canada_residuals2)
ggplot(qq_plot_canada2, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for Canada",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
#residuals generally follow the QQline, larger deviations at the extremes

# Shapiro-Wilk test for Canada (1 = energy, 2 = emissions)
shapiro_test_canada1 <- shapiro.test(canada_residuals1)
print(shapiro_test_canada1)
#p-value = .0056, which does not meet assumption of normality, but we note and proceed

shapiro_test_canada2 <- shapiro.test(canada_residuals2)
print(shapiro_test_canada2)
#p-value = .0063, which does not meet assumption of normality, but we note and proceed

## Models for China
china_data <- pivoted_data %>% filter(Country == "China")
energy_model_china <- lm(`Total Non-Renewable` ~ `Total Renewable`, data = china_data)
emission_model_china <- lm(CO2_Emissions_per_Capita ~ `Total Renewable`, data = china_data)

summary(energy_model_china)
#highly significant and strong positive relationship (p<0.001), with renewable energy contributing significantly to non-renewable energy consumption (R^2=0.91)
#a unit increase in renewable energy corresponds to a 2.201-unit increase in non-renewable energy

summary(emission_model_china)

# Assumption checks for China (1 = energy, 2 = emissions)
china_residuals1 <- rstandard(energy_model_china)
china_fitted1 <- fitted.values(emission_model_china)

china_residuals2 <- rstandard(emission_model_china)
china_fitted2 <- fitted.values(emission_model_china)

# Residuals vs Fitted plot for China (1 = energy, 2 = emissions)
residuals_vs_fitted_china1 <- data.frame(Fitted = china_fitted1, Residuals = china_residuals1)
ggplot(residuals_vs_fitted_china1, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for China",
       x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
#residuals appear to follow some sort of pattern

residuals_vs_fitted_china2 <- data.frame(Fitted = china_fitted2, Residuals = china_residuals2)
ggplot(residuals_vs_fitted_china2, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for China",
       x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
#residuals more closely follow a pattern

# QQ plot for China (1 = energy, 2 = emissions)
qq_plot_china1 <- data.frame(Residuals = china_residuals1)
ggplot(qq_plot_china1, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for China",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
#residuals follow the QQline very well

qq_plot_china2 <- data.frame(Residuals = china_residuals2)
ggplot(qq_plot_china2, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for China",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
#residuals still follow the QQline fairly well

# Shapiro-Wilk test for China (1 = energy, 2 = emissions)
shapiro_test_china1 <- shapiro.test(china_residuals1)
print(shapiro_test_china1)
#p-value = .965, so assumption of normality is met

shapiro_test_china2 <- shapiro.test(china_residuals2)
print(shapiro_test_china2)
#p-value = .021, so assumption of normality is NOT met

### Models for Germany
germany_data <- pivoted_data %>% filter(Country == "Germany")
energy_model_germany <- lm(`Total Non-Renewable` ~ `Total Renewable`, data = germany_data)
emission_model_germany <- lm(CO2_Emissions_per_Capita ~ `Total Renewable`, data = germany_data)

summary(energy_model_germany)
#no significant relationship between renewable and non-renewable energy (p=0.213), with a low R^2=0.07

summary(emission_model_germany)
#renewable energy use is significantly and negatively correlated with CO2 emissions per capita (p=0.003), with R^2=0.33
#a unit increase in renewable energy reduces emissions by a very small amount

# Assumption checks for Germany (1 = energy, 2 = emissions)
germany_residuals1 <- rstandard(energy_model_germany)
germany_fitted1 <- fitted.values(emission_model_germany)

germany_residuals2 <- rstandard(emission_model_germany)
germany_fitted2 <- fitted.values(emission_model_germany)

# Residuals vs Fitted plot for Germany (1 = energy, 2 = emissions)
residuals_vs_fitted_germany1 <- data.frame(Fitted = germany_fitted1, Residuals = germany_residuals1)
ggplot(residuals_vs_fitted_germany1, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for Germany",
       x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
#residuals do not appear randomly distributed, noted but will proceed

residuals_vs_fitted_germany2 <- data.frame(Fitted = germany_fitted2, Residuals = germany_residuals2)
ggplot(residuals_vs_fitted_germany2, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for Germany",
       x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
#again, residuals do not appear randomly distributed but will proceed

# QQ plot for Germany (1 = energy, 2 = emissions)
qq_plot_germany1 <- data.frame(Residuals = germany_residuals1)
ggplot(qq_plot_germany1, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for Germany",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
#residuals follow the QQline fairly well, except at the lower end

qq_plot_germany2 <- data.frame(Residuals = germany_residuals2)
ggplot(qq_plot_germany2, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for Germany",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
#again, residuals follow the QQline fairly well, except at the lower end

# Shapiro-Wilk test for Germany (1 = energy, 2 = emissions)
shapiro_test_germany1 <- shapiro.test(germany_residuals1)
print(shapiro_test_germany1)
#p-value = 1.5e-7, which is incredibly small, does NOT meet normality assumption but proceed

shapiro_test_germany2 <- shapiro.test(germany_residuals2)
print(shapiro_test_germany2)
#similarly, p-value = 6.7e-7 is incredibly small, but we continue

### Models for India
india_data <- pivoted_data %>% filter(Country == "India")
energy_model_india <- lm(`Total Non-Renewable` ~ `Total Renewable`, data = india_data)
emission_model_india <- lm(CO2_Emissions_per_Capita ~ `Total Renewable`, data = india_data)

summary(energy_model_india)
#very strong, positive, and significant relationship (p<0.001) exists between renewable and non-renewable energy (R^2 =0.93)
#a unit increase in renewable energy correlates with a 4.101-unit increase in non-renewable energy

summary(emission_model_india)
#CO2 emissions per capita increase significantly with renewable energy (p<0.001), explaining 41% of the variance (R^2)

# Assumption checks for India (1 = energy, 2 = emissions)
india_residuals1 <- rstandard(energy_model_india)
india_fitted1 <- fitted.values(emission_model_india)

india_residuals2 <- rstandard(emission_model_india)
india_fitted2 <- fitted.values(emission_model_india)

# Residuals vs Fitted plot for India (1 = energy, 2 = emissions)
residuals_vs_fitted_india1 <- data.frame(Fitted = india_fitted1, Residuals = india_residuals1)
ggplot(residuals_vs_fitted_india1, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for India",
       x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
#residuals appear sort of randomly distributed, some vague patterns

residuals_vs_fitted_india2 <- data.frame(Fitted = india_fitted2, Residuals = india_residuals2)
ggplot(residuals_vs_fitted_india2, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for India",
       x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
#residuals appear to follow a pattern, but we proceed

# QQ plot for India (1 = energy, 2 = emissions)
qq_plot_india1 <- data.frame(Residuals = india_residuals1)
ggplot(qq_plot_india1, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for India",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
#residuals generally follow the QQline

qq_plot_india2 <- data.frame(Residuals = india_residuals2)
ggplot(qq_plot_india2, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for India",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
#residuals follow the QQline quite well

# Shapiro-Wilk test for India (1 = energy, 2 = emissions)
shapiro_test_india1 <- shapiro.test(india_residuals1)
print(shapiro_test_india1)
#p-value = .551, which satisfies assumption of normality 

shapiro_test_india2 <- shapiro.test(india_residuals2)
print(shapiro_test_india2)
#p-value = 1.13e-5 is incredibly small and does not meet assumption of normality, but we proceed

### Models for Japan
japan_data <- pivoted_data %>% filter(Country == "Japan")
energy_model_japan <- lm(`Total Non-Renewable` ~ `Total Renewable`, data = japan_data)
emission_model_japan <- lm(CO2_Emissions_per_Capita ~ `Total Renewable`, data = japan_data)

summary(energy_model_japan)
#relationship between renewable and non-renewable energy is not significant (p=0.215), with a very low R^2=0.07

summary(emission_model_japan)
#renewable energy has a marginally non-significant negative impact on CO2 emissions per capita (p=0.064)
#model explains only 15% of the variance (R^2)

# Assumption checks for Japan (1 = energy, 2 = emissions)
japan_residuals1 <- rstandard(energy_model_japan)
japan_fitted1 <- fitted.values(emission_model_japan)

japan_residuals2 <- rstandard(emission_model_japan)
japan_fitted2 <- fitted.values(emission_model_japan)

# Residuals vs Fitted plot for Japan (1 = energy, 2 = emissions)
residuals_vs_fitted_japan1 <- data.frame(Fitted = japan_fitted1, Residuals = japan_residuals1)
ggplot(residuals_vs_fitted_japan1, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for Japan",
       x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
#residuals do not appear randoomly distributed, are clustered, but we proceed

residuals_vs_fitted_japan2 <- data.frame(Fitted = japan_fitted2, Residuals = japan_residuals2)
ggplot(residuals_vs_fitted_japan2, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for Japan",
       x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
#residuals appear a little more randomly distributed, but not by much 

# QQ plot for Japan (1 = energy, 2 = emissions)
qq_plot_japan1 <- data.frame(Residuals = japan_residuals1)
ggplot(qq_plot_japan1, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for Japan",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
#residuals follow the QQline fairly well

qq_plot_japan2 <- data.frame(Residuals = japan_residuals2)
ggplot(qq_plot_japan2, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for Japan",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
#again, residuals follow the QQline fairly well

# Shapiro-Wilk test for Japan (1 = energy, 2 = emissions)
shapiro_test_japan1 <- shapiro.test(japan_residuals1)
print(shapiro_test_japan1)
#p-value = 2.94e-6 is incredibly small, but we continue

shapiro_test_japan2 <- shapiro.test(japan_residuals2)
print(shapiro_test_japan2)
#p-value = .0001 is small and does not meet normality assumption

### Models for Norway
norway_data <- pivoted_data %>% filter(Country == "Norway")
energy_model_norway <- lm(`Total Non-Renewable` ~ `Total Renewable`, data = norway_data)
emission_model_norway <- lm(CO2_Emissions_per_Capita ~ `Total Renewable`, data = norway_data)

summary(energy_model_norway)
#renewable and non-renewable energy is marginally non-significant (p=0.062), with a low R^=0.15

summary(emission_model_norway)
#renewable energy has no significant impact on CO2 emissions per capita (p=0.397), with near-zero R^2

# Assumption checks for Norway (1 = energy, 2 = emissions)
norway_residuals1 <- rstandard(energy_model_norway)
norway_fitted1 <- fitted.values(emission_model_norway)

norway_residuals2 <- rstandard(emission_model_norway)
norway_fitted2 <- fitted.values(emission_model_norway)

# Residuals vs Fitted plot for Norway (1 = energy, 2 = emissions)
residuals_vs_fitted_norway1 <- data.frame(Fitted = norway_fitted1, Residuals = norway_residuals1)
ggplot(residuals_vs_fitted_norway1, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for Norway",
       x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
#residuals appear to be somewhat normally distributed, although they are clustered around one area of the graph

residuals_vs_fitted_norway2 <- data.frame(Fitted = norway_fitted2, Residuals = norway_residuals2)
ggplot(residuals_vs_fitted_norway2, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for Norway",
       x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
#residuals do not appear to be randomly distributed, but we proceed

# QQ plot for Norway (1 = energy, 2 = emissions)
qq_plot_norway1 <- data.frame(Residuals = norway_residuals1)
ggplot(qq_plot_norway1, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for Norway",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
#residuals follow QQline generally

qq_plot_norway2 <- data.frame(Residuals = norway_residuals2)
ggplot(qq_plot_norway2, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for Norway",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
#residuals follow QQline better

# Shapiro-Wilk test for Norway (1 = energy, 2 = emissions)
shapiro_test_norway1 <- shapiro.test(norway_residuals1)
print(shapiro_test_norway1)
#p-value = .105 meets assumption of normality 

shapiro_test_norway2 <- shapiro.test(norway_residuals2)
print(shapiro_test_norway2)
#p-value = .0123 does not meet assumption of normality, but we continue

### Models for USA
usa_data <- pivoted_data %>% filter(Country == "USA")
energy_model_usa <- lm(`Total Non-Renewable` ~ `Total Renewable`, data = usa_data)
emission_model_usa <- lm(CO2_Emissions_per_Capita ~ `Total Renewable`, data = usa_data)

summary(energy_model_usa)
#The relationship is marginally non-significant (p=0.07), with renewable energy weakly predicting 
#non-renewable energy (R^2=0.14)

summary(emission_model_usa)
#CO2 emissions per capita decrease significantly with renewable energy (p=0.001), explaining 39% of the 
#variance (R^2=0.39).

# Assumption checks for USA (1 = energy, 2 = emissions)
usa_residuals1 <- rstandard(energy_model_usa)
usa_fitted1 <- fitted.values(emission_model_usa)

usa_residuals2 <- rstandard(emission_model_usa)
usa_fitted2 <- fitted.values(emission_model_usa)

# Residuals vs Fitted plot for USA (1 = energy, 2 = emissions)
residuals_vs_fitted_usa1 <- data.frame(Fitted = usa_fitted1, Residuals = usa_residuals1)
ggplot(residuals_vs_fitted_usa1, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for USA",
       x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
#residuals do not appear to be randomly distributed but we continue

residuals_vs_fitted_usa2 <- data.frame(Fitted = usa_fitted2, Residuals = usa_residuals2)
ggplot(residuals_vs_fitted_usa2, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for USA",
       x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
#again, residuals do not appear randomly distributed

# QQ plot for USA (1 = energy, 2 = emissions)
qq_plot_usa1 <- data.frame(Residuals = usa_residuals1)
ggplot(qq_plot_usa1, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for USA",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
#residuals follow QQline well

qq_plot_usa2 <- data.frame(Residuals = usa_residuals2)
ggplot(qq_plot_usa2, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for USA",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
#again, residuals follow QQline well

# Shapiro-Wilk test for USA (1 = energy, 2 = emissions)
shapiro_test_usa1 <- shapiro.test(usa_residuals1)
print(shapiro_test_usa1)
#p-value = 1.48e-6 is incredibly small and does not meet assumption of normality but we continue

shapiro_test_usa2 <- shapiro.test(usa_residuals2)
print(shapiro_test_usa2)
#again, p-value = 4.89e-5 is incredibly small and does not meet assumption of normality


###### Visualization of energy generation over time for each country\
# Summarize renewable and non-renewable energy for Brazil
brazil_energy_proportion <- brazil_data %>%
  summarise(
    Renewable = sum(`Total Renewable`, na.rm = TRUE),
    `Non Renewable` = sum(`Total Non-Renewable`, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Energy_Type", values_to = "Generation") %>%
  mutate(Percentage = (Generation / sum(Generation)) * 100)  # Calculate percentage

# Create pie chart with percentage labels
ggplot(brazil_energy_proportion, aes(x = "", y = Generation, fill = Energy_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Energy Contribution in Brazil",
       fill = "Energy Type") +
  theme_void()


# Summarize renewable and non-renewable energy for Canada
canada_energy_proportion <- canada_data %>%
  summarise(
    Renewable = sum(`Total Renewable`, na.rm = TRUE),
    `Non Renewable` = sum(`Total Non-Renewable`, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Energy_Type", values_to = "Generation") %>%
  mutate(Percentage = (Generation / sum(Generation)) * 100)  # Calculate percentage

# Create pie chart with percentage labels
ggplot(canada_energy_proportion, aes(x = "", y = Generation, fill = Energy_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Energy Contribution in Canada",
       fill = "Energy Type") +
  theme_void()

# Summarize renewable and non-renewable energy for China
china_energy_proportion <- china_data %>%
  summarise(
    Renewable = sum(`Total Renewable`, na.rm = TRUE),
    `Non Renewable` = sum(`Total Non-Renewable`, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Energy_Type", values_to = "Generation") %>%
  mutate(Percentage = (Generation / sum(Generation)) * 100)  # Calculate percentage

# Create pie chart with percentage labels
ggplot(china_energy_proportion, aes(x = "", y = Generation, fill = Energy_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Energy Contribution in China",
       fill = "Energy Type") +
  theme_void()


# Summarize renewable and non-renewable energy for Germany
germany_energy_proportion <- germany_data %>%
  summarise(
    Renewable = sum(`Total Renewable`, na.rm = TRUE),
    `Non Renewable` = sum(`Total Non-Renewable`, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Energy_Type", values_to = "Generation") %>%
  mutate(Percentage = (Generation / sum(Generation)) * 100)  # Calculate percentage

# Create pie chart with percentage labels
ggplot(germany_energy_proportion, aes(x = "", y = Generation, fill = Energy_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Energy Contribution in Germany",
       fill = "Energy Type") +
  theme_void()

# Summarize renewable and non-renewable energy for India
india_energy_proportion <- india_data %>%
  summarise(
    Renewable = sum(`Total Renewable`, na.rm = TRUE),
    `Non Renewable` = sum(`Total Non-Renewable`, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Energy_Type", values_to = "Generation") %>%
  mutate(Percentage = (Generation / sum(Generation)) * 100)  # Calculate percentage

# Create pie chart with percentage labels
ggplot(india_energy_proportion, aes(x = "", y = Generation, fill = Energy_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Energy Contribution in India",
       fill = "Energy Type") +
  theme_void()


# Summarize renewable and non-renewable energy for Japan
japan_energy_proportion <- japan_data %>%
  summarise(
    Renewable = sum(`Total Renewable`, na.rm = TRUE),
    `Non Renewable` = sum(`Total Non-Renewable`, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Energy_Type", values_to = "Generation") %>%
  mutate(Percentage = (Generation / sum(Generation)) * 100)  # Calculate percentage

# Create pie chart with percentage labels
ggplot(japan_energy_proportion, aes(x = "", y = Generation, fill = Energy_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Energy Contribution in Japan",
       fill = "Energy Type") +
  theme_void()

# Summarize renewable and non-renewable energy for Norway
norway_energy_proportion <- norway_data %>%
  summarise(
    Renewable = sum(`Total Renewable`, na.rm = TRUE),
    `Non Renewable` = sum(`Total Non-Renewable`, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Energy_Type", values_to = "Generation") %>%
  mutate(Percentage = (Generation / sum(Generation)) * 100)  # Calculate percentage

# Create pie chart with percentage labels
ggplot(norway_energy_proportion, aes(x = "", y = Generation, fill = Energy_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Energy Contribution in Norway",
       fill = "Energy Type") +
  theme_void()


# Summarize renewable and non-renewable energy for USA
usa_energy_proportion <- usa_data %>%
  summarise(
    Renewable = sum(`Total Renewable`, na.rm = TRUE),
    `Non Renewable` = sum(`Total Non-Renewable`, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Energy_Type", values_to = "Generation") %>%
  mutate(Percentage = (Generation / sum(Generation)) * 100)  # Calculate percentage

# Create pie chart with percentage labels
ggplot(usa_energy_proportion, aes(x = "", y = Generation, fill = Energy_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Energy Contribution in USA",
       fill = "Energy Type") +
  theme_void()