library(tidyverse)
library(estimatr)
library(lme4)
library(lmerTest)

options(scipen = 999)

df <- read.csv("polymarket_data.csv")

df <- df %>%
  mutate(
    log_volume = log(volume),
    is_prime = as.integer(str_detect(title, "House Election Winner|Primary Winner"))
    )

# DESCRIPTIVE STATISTICS
# Table 1: Summary statistics
summary_vars <- df %>%
  select(volume, log_volume, actor_polyarchy, subject_polyarchy,
         actor_libdem, entropy, proximity_5050, market_age_days, is_prime) %>%
  pivot_longer(everything(), names_to = "variable") %>%
  group_by(variable) %>%
  summarise(
    N = sum(!is.na(value)),
    Mean = mean(value, na.rm = TRUE),
    Median = median(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    Min = min(value, na.rm = TRUE),
    Max = max(value, na.rm = TRUE)
  )
print(summary_vars)

# Table 2: Cross-tabulation event_type x democracy level
df <- df %>%
  mutate(dem_level = case_when(
    is.na(subject_polyarchy) ~ "N/A",
    subject_polyarchy < 0.4 ~ "Autocracy",
    subject_polyarchy < 0.7 ~ "Hybrid",
    TRUE ~ "Democracy"
  ))
table(df$event_type, df$dem_level)

# Table 3: Volume by event type
df %>%
  group_by(event_type) %>%
  summarise(
    N = n(),
    Mean_logvol = mean(log_volume, na.rm = TRUE),
    Median_logvol = median(log_volume, na.rm = TRUE),
    SD_logvol = sd(log_volume, na.rm = TRUE),
    Median_raw = median(volume, na.rm = TRUE),
    Mean_entropy = mean(entropy, na.rm = TRUE),
    Mean_polyarchy = mean(subject_polyarchy, na.rm = TRUE)
  )

# Table 4: Correlation matrix
cor(df[, c("log_volume", "actor_polyarchy", "subject_polyarchy",
           "entropy", "market_age_days")], use = "pairwise.complete.obs")


## STAGE 1: AGGREGATED MODELS (H1)
# Model 1: baseline
m1_ols <- lm(log_volume ~ actor_polyarchy + event_type + entropy + market_age_days + is_prime,
             data = df)
summary(m1_ols)

m1_cr <- lm_robust(log_volume ~ actor_polyarchy + event_type + entropy + market_age_days + is_prime,
                data = df, clusters = country_actor, se_type = "CR2")
summary(m1_cr)

# Model 2: democracy*event_type (H1)
m2_ols <- lm(log_volume ~ event_type:actor_polyarchy + entropy + market_age_days + is_prime,
             data = df)
summary(m2_ols)

m2_cr <- lm_robust(log_volume ~ event_type:actor_polyarchy + entropy + market_age_days + is_prime,
                data = df, clusters = country_actor, se_type = "CR2")
summary(m2_cr)

# Model 3: democracy*entropy
m3_ols <- lm(log_volume ~ actor_polyarchy * entropy + event_type + market_age_days + is_prime,
             data = df)
summary(m3_ols)

m3_cr <- lm_robust(log_volume ~ actor_polyarchy * entropy + event_type + market_age_days + is_prime,
                data = df, clusters = country_actor, se_type = "CR2")
summary(m3_cr)


# STAGE 2: SUBSAMPLE MODELS (H2a, H2b)
## Elections
elec_ols <- lm(log_volume ~ actor_polyarchy * entropy + market_age_days + is_prime,
               data = df[df$event_type == "Elections", ])
summary(elec_ols)

elec_cr <- lm_robust(log_volume ~ actor_polyarchy * entropy + market_age_days + is_prime,
                    data = df[df$event_type == "Elections", ], clusters = country_actor, se_type = "CR0")
summary(elec_cr)

## Leadership Changes
lead_ols <- lm(log_volume ~ actor_polyarchy * entropy + market_age_days,
               data = df[df$event_type == "Leadership Changes", ])
summary(lead_ols)

lead_cr <- lm_robust(log_volume ~ actor_polyarchy * entropy + market_age_days,
                    data = df[df$event_type == "Leadership Changes", ], clusters = country_actor, se_type = "CR2")
summary(lead_cr)

## Domestic Politics
dom_ols <- lm(log_volume ~ actor_polyarchy * entropy + market_age_days,
              data = df[df$event_type == "Domestic Policy", ])
summary(dom_ols)

dom_cr  <- lm_robust(log_volume ~ actor_polyarchy * entropy + market_age_days,
                    data = df[df$event_type == "Domestic Policy", ], clusters = country_actor, se_type = "CR2")
summary(dom_cr)

## International Affairs
intl_ols <- lm(log_volume ~ actor_polyarchy * entropy + market_age_days,
                data = df[df$event_type == "Intl Conflicts", ])
summary(intl_ols)

intl_cr <- lm_robust(log_volume ~ actor_polyarchy * entropy + market_age_days,
                    data = df[df$event_type == "Intl Conflicts", ], clusters = country_actor, se_type = "CR2")
summary (intl_cr)


######################
## ROBUSTNESS CHECK ##
# Linear Mixed Model with random effect on a country
m1_lmer <- lmer(log_volume ~ actor_polyarchy + event_type + entropy + market_age_days + is_prime + (1|country_actor),
                data = df)
summary(m1_lmer)

m2_lmer <- lmer(log_volume ~ event_type:actor_polyarchy + entropy + market_age_days + is_prime + (1|country_actor),
                data = df)
summary(m2_lmer)

m3_lmer <- lmer(log_volume ~ actor_polyarchy * entropy + event_type + market_age_days + is_prime + (1|country_actor),
                data = df)
summary(m3_lmer)

elec_lmer <- lmer(log_volume ~ actor_polyarchy * entropy + market_age_days + is_prime + (1|country_actor),
                  data = df[df$event_type == "Elections", ])
summary(elec_lmer)

lead_lmer <- lmer(log_volume ~ actor_polyarchy * entropy + market_age_days + (1|country_actor),
                  data = df[df$event_type == "Leadership Changes", ])
summary(lead_lmer)

dom_lmer <- lmer(log_volume ~ actor_polyarchy * entropy + market_age_days + (1|country_actor),
                 data = df[df$event_type == "Domestic Policy", ])
summary(dom_lmer)

intl_lmer <- lmer(log_volume ~ actor_polyarchy * entropy + market_age_days + (1|country_actor),
                  data = df[df$event_type == "Intl Conflicts", ])
summary(intl_lmer)

#OLS subject-based democracy level
# Model 1: baseline
subject_m1_ols <- lm(log_volume ~ subject_polyarchy + event_type + entropy + market_age_days + is_prime,
             data = df)
summary(subject_m1_ols)

subject_m1_cr <- lm_robust(log_volume ~ subject_polyarchy + event_type + entropy + market_age_days + is_prime,
                   data = df, clusters = country_actor, se_type = "CR2")
summary(subject_m1_cr)

# Model 2: democracy*event_type (H1)
subject_m2_ols <- lm(log_volume ~ event_type:subject_polyarchy + entropy + market_age_days + is_prime,
             data = df)
summary(subject_m2_ols)

subject_m2_cr <- lm_robust(log_volume ~ event_type:subject_polyarchy + entropy + market_age_days + is_prime,
                   data = df, clusters = country_actor, se_type = "CR2")
summary(subject_m2_cr)

# Model 3: democracy*entropy
subject_m3_ols <- lm(log_volume ~ subject_polyarchy * entropy + event_type + market_age_days + is_prime,
             data = df)
summary(subject_m3_ols)

subject_m3_cr <- lm_robust(log_volume ~ subject_polyarchy * entropy + event_type + market_age_days + is_prime,
                   data = df, clusters = country_actor, se_type = "CR2")
summary(subject_m3_cr)

## Elections
subject_elec_ols <- lm(log_volume ~ subject_polyarchy * entropy + market_age_days + is_prime,
               data = df[df$event_type == "Elections", ])
summary(subject_elec_ols)

subject_elec_cr <- lm_robust(log_volume ~ subject_polyarchy * entropy + market_age_days + is_prime,
                     data = df[df$event_type == "Elections", ], clusters = country_actor, se_type = "CR0")
summary(subject_elec_cr)

## Leadership Changes
subject_lead_ols <- lm(log_volume ~ subject_polyarchy * entropy + market_age_days,
               data = df[df$event_type == "Leadership Changes", ])
summary(subject_lead_ols)

subject_lead_cr <- lm_robust(log_volume ~ subject_polyarchy * entropy + market_age_days,
                     data = df[df$event_type == "Leadership Changes", ], clusters = country_actor, se_type = "CR2")
summary(subject_lead_cr)

## Domestic Politics
subject_dom_ols <- lm(log_volume ~ subject_polyarchy * entropy + market_age_days,
              data = df[df$event_type == "Domestic Policy", ])
summary(subject_dom_ols)

subject_dom_cr  <- lm_robust(log_volume ~ subject_polyarchy * entropy + market_age_days,
                     data = df[df$event_type == "Domestic Policy", ], clusters = country_actor, se_type = "CR2")
summary(subject_dom_cr)

## International Affairs
subject_intl_ols <- lm(log_volume ~ subject_polyarchy * entropy + market_age_days,
               data = df[df$event_type == "Intl Conflicts", ])
summary(subject_intl_ols)

subject_intl_cr <- lm_robust(log_volume ~ subject_polyarchy * entropy + market_age_days,
                     data = df[df$event_type == "Intl Conflicts", ], clusters = country_actor, se_type = "CR2")
summary (subject_intl_cr)


