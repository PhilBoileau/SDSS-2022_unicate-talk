################################################################################
# Plots for Presentation
################################################################################

# load required libraries
library(here)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

# generate a plot to provide some intuition on VIP #############################

# generate a dummy dataset
set.seed(12312)
n <- 200
treatment <- c(rep(0, n / 2), rep(1, n / 2))
biomarker <- rnorm(n)
err <- rnorm(n, mean = 0, sd = 0.2)
outcome <- treatment + 0.5 * biomarker + biomarker * treatment + err
trial_tbl <- tibble(
  treatment = treatment,
  biomarker = biomarker,
  outcome = outcome
) %>%
  mutate(
    treatment = if_else(treatment == 0, "Control", "Treatment"),
    treatment = factor(treatment, levels = c("Control", "Treatment"))
  )

# plot the data
trial_tbl %>%
  ggplot(aes(x = biomarker, y = outcome, colour = treatment)) +
    geom_point(alpha = 0.5) +
    xlab("Biomarker") +
    ylab("Outcome") +
    scale_colour_viridis_d(name = "Group Assignment", option = "E", end = 0.8) +
    theme_classic()

ggsave(
  filename = "simulated-trial-data.png",
  path = here("plots"),
  dpi = "retina",
  width = 6,
  height = 4,
  scale = 1.2
)

# predict the difference in outcome under treatment and control

# fit a linear model on the original data
lm_mod <- lm("outcome ~ treatment + biomarker * treatment")

# predict the counterfactual outcomes
all_cont_tbl <- trial_tbl %>% mutate(treatment = 0)
all_treat_tbl <- trial_tbl %>% mutate(treatment = 1)
trial_tbl$outcome_cont <- predict(lm_mod, newdata = all_cont_tbl)
trial_tbl$outcome_treat <- predict(lm_mod, newdata = all_treat_tbl)

# compute the counterfactual differnces
trial_tbl <- trial_tbl %>% mutate(diff = outcome_treat - outcome_cont - 0.6)

# plot the predicted outcomes
trial_tbl %>%
  select(biomarker, outcome_cont, outcome_treat) %>%
  pivot_longer(cols = c(outcome_cont, outcome_treat),
               names_to = "outcome_type") %>%
  mutate(outcome_type = if_else(outcome_type == "outcome_cont",
                                "Control Assignment Outcome",
                                "Treatment Assignment Outcome")) %>%
  ggplot(aes(x = biomarker, y = value, colour = outcome_type)) +
    geom_smooth(se = FALSE, method = "lm") +
    xlab("Biomarker") +
    ylab("Predicted Outcome") +
    scale_colour_viridis_d(name = "", option = "E", end = 0.8) +
    theme_classic()

ggsave(
  filename = "simulated-trial-predicted-outcomes.png",
  path = here("plots"),
  dpi = "retina",
  width = 6,
  height = 4,
  scale = 1.2
)

# plot the predicted outcomes with predicted difference
trial_tbl %>%
  select(biomarker, outcome_cont, outcome_treat, diff) %>%
  pivot_longer(cols = c(outcome_cont, outcome_treat, diff),
               names_to = "outcome_type") %>%
  mutate(outcome_type = if_else(outcome_type == "outcome_cont",
                                "Control Assignment Outcome",
                          if_else(outcome_type == "diff",
                                  "Predicted Outcome Difference",
                                  "Treatment Assignment Outcome")),
         outcome_type = factor(outcome_type,
                               levels = c("Control Assignment Outcome",
                                          "Predicted Outcome Difference",
                                          "Treatment Assignment Outcome"))
  ) %>%
  ggplot(aes(x = biomarker, y = value, colour = outcome_type)) +
    geom_smooth(se = FALSE, method = "lm") +
    xlab("Biomarker") +
    ylab("Predicted Outcome") +
    scale_colour_viridis_d(name = "", option = "E", end = 0.8) +
    theme_classic()

ggsave(
  filename = "simulated-trial-predicted-outcome-differences.png",
  path = here("plots"),
  dpi = "retina",
  width = 6,
  height = 4,
  scale = 1.2
)
