
install.packages("rdrobust")
install.packages("stargazer")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
library(rdrobust)
library(stargazer)


# question 1 
full_df <- read_rds("C:/Users/mirac/Documents/GitHub/econ470_ma/hw4/data/output/final_ma_data.rds")

table(full_df$plan_type)
table(full_df$org_type)

q1 <- full_df %>%[;]
    filter(plan_type == c("HMO/HMOPOS","Local PPO", "MSA", "MSA Demo", "PSO (Federal Waiver of State License)", "PSO (State License)")) %>%
    group_by(year, county) %>%
    summarise(total_enrollment = n())

ggplot(q1, aes(x = as.factor(year), y = total_enrollment, fill = county)) +
    geom_boxplot() +
    labs(
        title = "Box and Whisker Plot of Total Enrollment Over Time",
        x = "Year",
        y = "Total Enrollment",
        fill = "County"
    ) +
    theme_minimal()

# question 2

# 2010
full_df %>%
    filter(year == 2010) %>%
    filter(Star_Rating != "NA") %>%
  ggplot(aes(x=as.factor(Star_Rating))) + 
  geom_bar() +
  labs(
    x="Star Rating",
    y="Count of Plans",
    title="Frequency Distribution of Star Ratings"
  ) + theme_bw()


# 2012
full_df %>%
    filter(year == 2012) %>%
    filter(Star_Rating != "NA") %>%
  ggplot(aes(x=as.factor(Star_Rating))) + 
  geom_bar() +
  labs(
    x="Star Rating",
    y="Count of Plans",
    title="Frequency Distribution of Star Ratings"
  ) + theme_bw()

  # 2015
full_df %>%
    filter(year == 2015) %>%
    filter(Star_Rating != "NA") %>%
  ggplot(aes(x=as.factor(Star_Rating))) + 
  geom_bar() +
  labs(
    x="Star Rating",
    y="Count of Plans",
    title="Frequency Distribution of Star Ratings"
  ) + theme_bw()

# simplified code
for (y in c(2010, 2012, 2025)) {
  full_df %>%
    filter(year == y) %>%
    filter(Star_Rating != "NA") %>%
  ggplot(aes(x=as.factor(Star_Rating))) + 
  geom_bar() +
  labs(
    x="Star Rating",
    y="Count of Plans",
    title=paste("Frequency Distribution of Star Ratings in ",y)
  ) + theme_bw()
}

# question 3

q3 <- full_df %>% 
filter(year >= 2010 & year <= 2015) %>%
group_by(year) %>%
summarise(avg_rate = mean(ma_rate, na.rm = TRUE))

ggplot(q3, aes(x = year, y = avg_rate)) +
    geom_line() +
    geom_point() +
    labs(
        title = "Average MA Rate Over Time",
        x = "Year",
        y = "Average MA Rate"
    ) +
    theme_minimal()

# question 4
q4 <- full_df %>% 
    filter(year >= 2010 & year <= 2015) %>%
    mutate(avg_share = avg_enrollment / avg_eligibles)

q4_b <- q4 %>%
    group_by(year) %>%
    summarise(avg = mean(avg_share, na.rm = TRUE))


ggplot(q4_b, aes(x = year, y = avg)) +
    geom_line() +
    geom_point() +
    labs(
        title = "Average Share of Enrollment to Eligible Population Over Time",
        x = "Year",
        y = "Average Share"
    ) +
    theme_minimal()


# question 5
df_10 <- full_df %>% filter(year == 2010)

df_10 %>% ggplot(aes(x=Star_Rating)) + 
  geom_bar(width=.025) + theme_bw() +
  labs(
    x="Running Variable",
    y="Number of Plans",
    title="Distribution of Star Rating"
  ) 

ls(df_10)

# overview of star rating
summary(df_10$partc_score)

# calculating average rating using underlying variables
df_10b <- df_10 %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,diabetes_cholscreen,
          glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
          mental_health,osteo_test,physical_monitor,primaryaccess,
          hospital_followup,depression_followup,nodelays,carequickly,
          overallrating_care,overallrating_plan,calltime,
          doctor_communicate,customer_service,osteo_manage,
          diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
          diabetes_chol,antidepressant,bloodpressure,ra_manage,
          copd_test,betablocker,bladder,falling,appeals_timely,
          appeals_review),
    na.rm=T)) %>%
  select(contractid, planid, fips, avg_enrollment, state, county, raw_rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate)

summary(df_10b$raw_rating)

# Group by Star_Rating and count the number of plans in each rating
star_rating_summary <- df_10 %>%
  group_by(Star_Rating) %>%
  summarise(plan_count = n()) %>%
  filter(Star_Rating %in% c(3, 3.5, 4, 4.5, 5))  # Filter for specific star ratings

# Print the summary table
print(star_rating_summary)


# question 6

# 3 vs 2.5 stars
df_10c <- df_10 %>%
  filter(Star_Rating %in% c(2.5, 3)) %>%
  filter(!is.na(avg_enrollment), !is.na(Star_Rating))

rd.est <- rdrobust(y = df_10c$avg_enrollment, x = df_10c$Star_Rating, c = 3, h = 0.125)
summary(rd.est)

# 3.5 vs 3 stars
df_10d <- df_10 %>%
  filter(Star_Rating %in% c(3, 3.5)) %>%
  filter(!is.na(avg_enrollment), !is.na(Star_Rating))

rd.est2 <- rdrobust(
  y = df_10d$avg_enrollment,
  x = df_10d$Star_Rating,
  c = 3.5,
  h = 0.125
)
summary(rd.est2)

# question 7
bandwidths <- c(0.1, 0.12, 0.13, 0.14, 0.15)

# Initialize lists to store results
results_3_vs_2_5 <- list()
results_3_5_vs_3 <- list()

# Loop over bandwidths for 3 vs 2.5 stars
for (h in bandwidths) {
  rd.est <- rdrobust(y = df_10c$avg_enrollment, x = df_10c$Star_Rating, c = 3, h = h)
  results_3_vs_2_5[[as.character(h)]] <- rd.est$coef[1]  # Store the treatment effect
}

# Loop over bandwidths for 3.5 vs 3 stars
for (h in bandwidths) {
  rd.est2 <- rdrobust(y = df_10d$avg_enrollment, x = df_10d$Star_Rating, c = 3.5, h = h)
  results_3_5_vs_3[[as.character(h)]] <- rd.est2$coef[1]  # Store the treatment effect
}

# Convert results to data frames for plotting
results_df_3_vs_2_5 <- data.frame(
  Bandwidth = bandwidths,
  Treatment_Effect = unlist(results_3_vs_2_5)
)

results_df_3_5_vs_3 <- data.frame(
  Bandwidth = bandwidths,
  Treatment_Effect = unlist(results_3_5_vs_3)
)

# Plot for 3 vs 2.5 stars
plot_3_vs_2_5 <- ggplot(results_df_3_vs_2_5, aes(x = Bandwidth, y = Treatment_Effect)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Sensitivity Analysis: 3 vs 2.5 Stars",
    x = "Bandwidth",
    y = "Treatment Effect"
  ) +
  theme_minimal()

# Plot for 3.5 vs 3 stars
plot_3_5_vs_3 <- ggplot(results_df_3_5_vs_3, aes(x = Bandwidth, y = Treatment_Effect)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Sensitivity Analysis: 3.5 vs 3 Stars",
    x = "Bandwidth",
    y = "Treatment Effect"
  ) +
  theme_minimal()

# question 9

# For 3 vs 2.5 stars
df_10c_characteristics <- df_10 %>%
  filter(Star_Rating %in% c(2.5, 3)) %>%
  filter(!is.na(parent_org), !is.na(partd))

characteristics_3_vs_2_5 <- df_10c_characteristics %>%
  group_by(Star_Rating, parent_org) %>%
  summarise(
    count_partd = sum(partd, na.rm = TRUE),
    total_plans = n(),
    partd_share = count_partd / total_plans
  )

print(characteristics_3_vs_2_5)

# For 3.5 vs 3 stars
df_10d_characteristics <- df_10 %>%
  filter(Star_Rating %in% c(3, 3.5)) %>%
  filter(!is.na(parent_org), !is.na(partd))

characteristics_3_5_vs_3 <- df_10d_characteristics %>%
  group_by(Star_Rating, parent_org) %>%
  summarise(
    count_partd = sum(partd, na.rm = TRUE),
    total_plans = n(),
    partd_share = count_partd / total_plans
  )

print(characteristics_3_5_vs_3)


save.image("C:/Users/mirac/Documents/GitHub/econ470_ma/hw4/submission1/results/hw_workspace4.Rdata")