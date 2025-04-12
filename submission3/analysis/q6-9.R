
install.packages("rdrobust")
install.packages("stargazer")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
library(rdrobust)
library(stargazer)



# question 5
df_10 <- full_df %>% filter(year == 2010) %>%
  filter(!is.na(avg_enrollment), !is.na(partc_score)) %>%
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
         bid, avg_ffscost, ma_rate, year) %>%
  mutate(mktshare = avg_enrollment / avg_eligibles)


star_rating_summary <- df_10 %>%
  group_by(Star_Rating) %>%
  summarise(number_of_plans = n()) %>%
  filter(Star_Rating >= 3) %>%  # Filter for ratings 3-star and above
  arrange(Star_Rating)



# question 6

# 3 vs 2.5 stars

# Function to estimate RD effect at a given cutoff
estimate_rd_effect <- function(data, cutoff, bw) {
  data_subset <- data %>% 
    filter(raw_rating >= cutoff - bw, raw_rating <= cutoff + bw) %>%
    mutate(
      treated = ifelse(raw_rating >= cutoff, 1, 0),  # Treatment indicator
      running_var = raw_rating - cutoff              # Centered running variable
    )
  
  # RD estimation (local linear regression)
rd_model <- rdrobust(
  y = data_subset$avg_enrollment,
  x = data_subset$running_var,
  c = 0,
  h = bw,  # Use `h` instead of `bw`
  kernel = "tri"
)
  
  return(rd_model$coef["Conventional", "tau.us"])
}

ls(df_10)

estimate_rd_effect <- function(data, cutoff, bw) {
  data_subset <- data %>% 
    filter(raw_rating >= cutoff - bw, raw_rating <= cutoff + bw) %>%
    mutate(
      treated = ifelse(raw_rating >= cutoff, 1, 0),
      running_var = raw_rating - cutoff
    )
  
  rd_model <- rdrobust(
    y = data_subset$avg_enrollment,
    x = data_subset$running_var,
    c = 0,
    h = bw,
    kernel = "tri"
  )
  print(rd_model)  # Debug: Print full model output
  return(summary(rd_model))
}

names(rd_model)  # List all components
head(rd_model$coef)  # View structure

df_subset <- df_10 %>% filter(raw_rating >= 2.625 & raw_rating <= 2.875)
nrow(df_subset)  # Check if there are enough observations

effect_3star <- estimate_rd_effect(df_10, cutoff = 2.75, bw = 0.125)
effect_35star <- estimate_rd_effect(df_10, cutoff = 3.25, bw = 0.125)


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

star30 <- lm(mkt_share ~ treat + score, 
  data=(ma.data.clean %>%
    filter(raw_rating>=(2.75-0.125), raw_rating<=(2.75-0.125),
    Star_rating %>% in c(2.5, 3)) %>%
  mutate(treat=Star_rating==3),
    score=raw_rating-2.75))

star35 <- lm(mkt_share ~ treat + score, 
  data=(ma.data.clean %>%
    filter(raw_rating>=(3.25-0.125), raw_rating<=(3.25+0.125),
    Star_rating %>% in c(3, 3.5)) %>%
  mutate(treat=Star_rating==3.5),
    score=raw_rating-3.25))

star40 <- lm(mkt_share ~ treat + score, 
  data=(ma.data.clean %>%
    filter(raw_rating>=(3.75-0.125), raw_rating<=(3.75+0.125),
    Star_rating %>% in c(3.5, 4)) %>%
  mutate(treat=Star_rating==4),
    score=raw_rating-3.75))

# Loop over bandwidths for 3 vs 2.5 stars
for (h in bandwidths) {
  coef.30 <- tidy(star30bw, conf.int=TRUE) %>%
    mutate(rating=30)
  
  star35bw <- lm(mkt_share ~ treat + score,
  data=(ma.data.clean %>%
    filter(raw_rating>=(3.25-h),
    raw_rating<=(3.25+h),
    Star_rating %>% in c(3, 3.5)) %>%
  mutate(treat=Star_rating==3.5),
    score=raw_rating-3.25

coef.35 <- tidy(star35bw, conf.int=TRUE) %>% mutate(rating=35)

star40bw <- lm(mkt_share ~ treat + score,
  data=(ma.data.clean %>%
    filter(raw_rating>=(3.75-h),
    raw_rating<=(3.75+h),
    Star_rating %>% in c(3.5, 4)) %>%
  mutate(treat=Star_rating==4),
    score=raw_rating-3.75)

coef.40 <- tidy(star40bw, conf.int=TRUE) %>% mutate(rating=40)

est.collect <- rbind(coef.30, coef.35, coef.40) %>%
  mutate(bandwidth=h)

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
  filter((raw_rating >= 2.75-.125 & Star_Rating == 2.5) | (raw_rating <= 2.75+.125 & Star_Rating == 3)) %>%
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

# question 8 - alt
kd.running30 <- df_10 %>%
filter((raw_rating >= 2.75-.125 & Star_Rating == 2.5) | (raw_rating <= 2.75+.125 & Star_Rating == 3)) %>%
ggplot(aes(x=raw_rating)) +
geom_density() +
geom_vline(xintercept=2.75, linetype="dashed") +
labs(x="Running Variable", y="Number of Plans") +
scale_x_continuous(breaks=seq(2.6, 2.9, 0.05), limits=c(2.6, 2.9)) + theme_bw()

kd.running35 <- df_10 %>%
filter((raw_rating >= 3.25-.125 & Star_Rating == 3) | (raw_rating <= 3.25+.125 & Star_Rating == 3.5)) %>%
ggplot(aes(x=raw_rating)) +
geom_density() +
geom_vline(xintercept=3.25, linetype="dashed") +
labs(x="Running Variable", y="Number of Plans") +
scale_x_continuous(breaks=seq(3.10, 3.40, 0.05), limits=c(3.10, 3.40)) + theme_bw()

# question 9 - alt

partd_plot <- df_10 %>%
  ggplot(aes(x = raw_rating, fill = factor())) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 3.25, linetype = "dashed") +
  labs(x = "Running Variable", y = "Density", 
       title = "Part D Status Distribution Around 3.25 Cutoff",
       fill = "Above Cutoff") +
  scale_x_continuous(breaks = seq(3.10, 3.40, 0.05), limits = c(3.10, 3.40)) +
  theme_bw()



save.image("C:/Users/mirac/Documents/GitHub/econ470_ma/hw4/submission3/results/hw_workspace4.Rdata")