#'
#' Analysis for go_fish v2
#'


rm(list = ls())
setwd("/Users/erikbrockbank/web/elc-lab/go_fish_v2/analysis/")

library(tidyverse)
library(viridis)
library(patchwork)

library(lme4)
library(emmeans)


# GLOBALS ======================================================================
# Data files
SUMMARY_DATA = "01_go_fish_v2_meta.csv"
TRIAL_DATA = "02_go_fish_v2_trials.csv"
EVAL_DATA = "03_go_fish_v2_evaluation.csv"
MEMORY_DATA = "04_go_fish_v2_memory.csv"
EXPLANATION_DATA_CODED = "explanation_coded.csv"


label_width = 12
RULESET_LOOKUP = c("target" = "Target", "distractor" = "Distractor",
                   "abstract_color" = "Abstract (color)", "abstract_shape" = "Abstract (shape)",
                   "misc" = "All other rules", "rand" = "Random")

RULETEXT_LOOKUP = c(
  "If a lure combination has a pointy shape on the bottom, it will catch fish." = str_wrap("Target (100%)", label_width),
  "If a lure combination has a yellow shape or a diamond on the bottom, it will catch fish." = str_wrap("Distractor (100%)", label_width),
  "If a lure combination has a top lure with bright colors that are more visible under water (red or yellow), it will catch fish." = str_wrap("Abstract Color (75%)", label_width),
  "If a lure combination has a rounded top shape that resembles a fish's body, it will catch fish." = str_wrap("Abstract Shape (75%)", label_width),
  "There is no pattern to which lure combinations catch fish: the results are random, but there are approximately equal numbers that catch fish and don’t." = str_wrap("Random", label_width),
  "If a lure combination has a red shape on the bottom, it will catch fish." = str_wrap("Misc (25%)", label_width),
  "If a lure combination has a blue shape, it will catch fish." = str_wrap("Misc (50%)", label_width),
  "If a lure combination has a purple dot on at least one of the lures, it will catch fish." = str_wrap("Misc (75%)", label_width)
)


RULESET_LABELS = c(str_wrap("Misc (25%)", label_width), str_wrap("Misc (50%)", label_width), str_wrap("Misc (75%)", label_width),
                   str_wrap("Random", label_width), str_wrap("Abstract Color (75%)", label_width), str_wrap("Abstract Shape (75%)", label_width),
                   str_wrap("Distractor (100%)", label_width), str_wrap("Target (100%)", label_width))
RULESET_LABELS = c(str_wrap("Target (100%)", label_width), str_wrap("Distractor (100%)", label_width),
                   str_wrap("Abstract Color (75%)", label_width), str_wrap("Abstract Shape (75%)", label_width),
                   str_wrap("Random", label_width), str_wrap("Misc (75%)", label_width),
                   str_wrap("Misc (50%)", label_width), str_wrap("Misc (25%)", label_width))


CODED_MEASURE_FILTER = c("shape_total", "color_total", "purple_dot_total")
coded_measure_width = 10
CODED_MEASURE_LOOKUP = c("mechanism_total" = str_wrap("Mechanism", width = coded_measure_width),
                         # "shape_total" = str_wrap("Shape (total)", width = coded_measure_width),
                         "shape_abstract_total" = str_wrap("Shape (abstract)", width = coded_measure_width),
                         "shape_concrete_total" = str_wrap("Shape (concrete)", width = coded_measure_width),
                         # "color_total" = str_wrap("Color (total)", width = coded_measure_width),
                         "color_abstract_total" = str_wrap("Color (abstract)", width = coded_measure_width),
                         "color_concrete_total" = str_wrap("Color (concrete)", width = coded_measure_width),
                         # "purple_dot_total" = str_wrap("Purple dot (total)", width = coded_measure_width),
                         "purple_dot_abstract_total" = str_wrap("Purple dot (abstract)", width = coded_measure_width),
                         "purple_dot_concrete_total" = str_wrap("Purple dot (concrete)", width = coded_measure_width)
)
CODED_MEASURE_LEVELS = c(str_wrap("Mechanism", width = coded_measure_width),
                         # str_wrap("Shape (total)", width = coded_measure_width),
                         str_wrap("Shape (abstract)", width = coded_measure_width), str_wrap("Shape (concrete)", width = coded_measure_width),
                         # str_wrap("Color (total)", width = coded_measure_width),
                         str_wrap("Color (abstract)", width = coded_measure_width), str_wrap("Color (concrete)", width = coded_measure_width),
                         # str_wrap("Purple dot (total)", width = coded_measure_width),
                         str_wrap("Purple dot (abstract)", width = coded_measure_width), str_wrap("Purple dot (concrete)", width = coded_measure_width))






# ANALYSIS FUNCTIONS ===========================================================
# Read in and process summary data file
read_summary_data = function(filepath) {
  # Read in data at filepath and add/modify columns as needed
  summary_data = read_csv(filepath)
  summary_data = summary_data %>%
    mutate(condition = ifelse(is_control == TRUE, "Describe", "Explain"),
           experiment_completion_time = (expt_end_ts - expt_start_ts) / 1000)
  return(summary_data)
}


# Read in and process data from explain/describe trials
read_trial_data = function(filepath) {
  trial_data = read_csv(filepath)
  trial_data = trial_data %>%
    mutate(
      condition = ifelse(is_control == TRUE, "Describe", "Explain"),
      input_correct = (input_prediction_catches_fish == prediction_catches_fish))
  trial_data$input_evidence_response = str_replace_all(
    trial_data$input_evidence_response, "\n" , "[newline]")
  return(trial_data)
}


# Read in and process data from rule evaluation task
read_evaluation_data = function(filepath) {
  evaluation_data = read_csv(filepath)
  evaluation_data = evaluation_data %>%
    mutate(condition = ifelse(is_control == TRUE, "Describe", "Explain"))
  return(evaluation_data)
}


# Read in and process data from memory task at end of experiment
read_memory_data = function(filepath) {
  memory_data = read_csv(filepath)
  memory_data = memory_data %>%
    mutate(condition = ifelse(is_control == TRUE, "Describe", "Explain"),
           memory_correct = (memory_shape_in_expt == input_shape_in_expt))
  return(memory_data)
}

# Read in coded explanation/description data
read_coded_explanation_data = function(filename) {
  explanation_free_resp_coded = read_csv(filename)
  return(explanation_free_resp_coded)
}


# Summarize evaluation data across participants and conditions for target and non-target rules
get_evaluation_summary = function(evaluation_data) {
  evaluation_data %>%
    # mutate(ruleset = RULESET_LOOKUP[category]) %>%
    mutate(ruleset = RULETEXT_LOOKUP[rule_text]) %>%
    group_by(condition, ruleset, subjID) %>%
    summarize(mean_subj_rating = mean(input_rule_rating),
              rules = n()) %>%
    group_by(ruleset, condition) %>%
    summarize(mean_rating = mean(mean_subj_rating),
              subjects = n(),
              sd_rating = sd(mean_subj_rating),
              se_rating = sd_rating / sqrt(n()),
              ci_lower = mean_rating - se_rating,
              ci_upper = mean_rating + se_rating)
}


# Summarize experiment completion time data
get_time_summary = function(time_data) {
  time_data %>%
    group_by(condition) %>%
    summarize(mean_task_time = mean(experiment_completion_time),
              subjects = n(),
              se_task_time = sd(experiment_completion_time) / sqrt(subjects),
              ci_lower = mean_task_time - se_task_time,
              ci_upper = mean_task_time + se_task_time)
}


# Summarize average trial completion time by participant
get_trial_time_subj_summary = function(trial_data) {
  trial_data %>%
    mutate(trial_completion_time = (trial_n_end_ts - trial_n_start_ts) / 1000) %>%
    group_by(condition, subjID) %>%
    summarize(mean_trial_completion = mean(trial_completion_time),
              trials = n(),
              se_trial_completion = sd(trial_completion_time) / sqrt(trials),
              ci_lower = mean_trial_completion - se_trial_completion,
              ci_upper = mean_trial_completion + se_trial_completion)
}


# Get summary of time spent on trials in each condition across participants
get_trial_time_summary = function(trial_time_subj_summary) {
  trial_time_subj_summary %>%
    group_by(condition) %>%
    # These column names kept the same as those in `get_time_summary` for easier graphing
    summarize(mean_task_time = mean(mean_trial_completion),
              subjects = n(),
              se_trial_time = sd(mean_trial_completion) / sqrt(subjects),
              ci_lower = mean_task_time - se_trial_time,
              ci_upper = mean_task_time + se_trial_time)
}


# Summarize memory performance data by participant
get_memory_subj_summary = function(memory_data) {
  memory_subj_accuracy = memory_data %>%
    group_by(condition, subjID) %>%
    summarize(subj_accuracy = sum(memory_correct) / n())
  return(memory_subj_accuracy)
}


# Summarize memory performance across participants by condition
get_memory_summary = function(memory_subject_summary) {
  memory_summary = memory_subject_summary %>%
    group_by(condition) %>%
    summarize(mean_memory_accuracy = mean(subj_accuracy),
              subjects = n(),
              se_memory_accuracy = sd(subj_accuracy) / sqrt(n()),
              ci_lower = mean_memory_accuracy - se_memory_accuracy,
              ci_upper = mean_memory_accuracy + se_memory_accuracy)
  return(memory_summary)
}

# Summarize explanation/description coded data
get_explanation_coded_subj_summary = function(explanation_data) {
  explanation_summary = explanation_data %>%
    group_by(Condition, Subject) %>%
    summarize(mechanism_total = sum(`FINAL - total mechanisms`, na.rm = T),
              shape_total = sum(`FINAL - total shape references`, na.rm = T),
              shape_abstract_total = sum(`FINAL - abstract shape references`, na.rm = T),
              shape_concrete_total = sum(`FINAL - concrete shape references`, na.rm = T),
              color_total = sum(`FINAL - total color references`, na.rm = T),
              color_abstract_total = sum(`FINAL - abstract color references`, na.rm = T),
              color_concrete_total = sum(`FINAL - concrete color references`, na.rm = T),
              purple_dot_total = sum(`FINAL - total purple dot references`, na.rm = T),
              purple_dot_abstract_total = sum(`FINAL - abstract purple dot references`, na.rm = T),
              purple_dot_concrete_total = sum(`FINAL - concrete purple dot references`, na.rm = T)) %>%
    gather(
      key = "measure",
      value = "subject_total",
      -Condition,
      -Subject
    )
  return(explanation_summary)
}

# Summarize coded explanation/description results across participants by condition
get_explanation_coded_summary = function(explanation_subject_summary) {
  explanation_summary = explanation_subject_summary %>%
    filter(!measure %in% CODED_MEASURE_FILTER) %>%
    mutate(measure = factor(CODED_MEASURE_LOOKUP[measure], levels = CODED_MEASURE_LEVELS)) %>%
    group_by(Condition, measure) %>%
    summarize(subjects = n(),
              measure_mean = mean(subject_total),
              measure_se = sd(subject_total) / sqrt(subjects)
    )
  return(explanation_summary)
}


# Auxiliary function for printing out t test statistics
report_t_summary = function(t_test) {
  paste("Mean 1:", round(t_test$estimate[1], 2), "||",
        "Mean 2:", round(t_test$estimate[2], 2), "||",
        "t (", t_test$parameter, ") =", round(t_test$statistic, 2), ",",
        "p =", round(t_test$p.value, 3),
        sep = " ")
}

# Auxiliary function for printing out wilcoxon signed-rank test statistics
report_wilcox_summary = function(wilcox_test) {
  z_val = qnorm(wilcox_test$p.value / 2)
  paste("z =", round(z_val, 2), ",",
        "p =", round(wilcox_test$p.value, 3),
        sep = " ")
}





# PLOTTING FUNCTIONS ===========================================================
# Generic plot theme for use in most graphs
individ_plot_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 24),
  axis.title.y = element_text(face = "bold", size = 24),
  axis.title.x = element_text(face = "bold", size = 20),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 16, face = "bold"),
  axis.text.x = element_text(size = 16, face = "bold"),
  # legend text
  legend.text = element_text(size = 18, face = "bold"),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom",
  legend.key = element_rect(colour = "transparent", fill = "transparent")
)

# Bar chart of average evaluation ratings across conditions on rule evaluation task
plot_evaluation_results = function(evaluation_summary) {
  evaluation_summary %>%
    ggplot(aes(x = ruleset, y = mean_rating,
               color = condition, fill = condition)) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.5, alpha = 0.5) +
    geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.5, preserve = "single"),
      width = 0.2) +
    labs(y = "Mean evaluation rating") +
    scale_x_discrete(name = element_blank(),
                     breaks = RULESET_LABELS,
                     limits = RULESET_LABELS) +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        # Change defaults to be blue/green instead of yellow/purple
                        begin = 0.25,
                        end = 0.75) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       # Change defaults to be blue/green instead of yellow/purple
                       begin = 0.25,
                       end = 0.75) +
    scale_y_continuous(breaks = seq(0, 100, by = 10),
                       labels = seq(0, 100, by = 10),
                       limits = c(0, 100)) +
    individ_plot_theme
}

# Bar chart of average memory accuracy across conditions
plot_memory_data = function(memory_summary) {
  memory_summary %>%
    ggplot(aes(x = condition, y = mean_memory_accuracy,
               color = condition, fill = condition)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.25) +
    geom_hline(yintercept = 0.5, linetype = "dashed", size = 1) +
    ylim(c(0, 1)) +
    labs(x = "", y = "Mean memory accuracy") +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        # Change defaults to be blue/green instead of yellow/purple
                        begin = 0.25,
                        end = 0.75) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       # Change defaults to be blue/green instead of yellow/purple
                       begin = 0.25,
                       end = 0.75) +
    individ_plot_theme +
    theme(axis.text.x = element_blank())
}

# Bar chart of experiment completion time or avg. trial time
plot_time_data = function(time_summary, individ_data, ylab, title) {
  time_summary %>%
    ggplot(aes(x = condition, y = mean_task_time,
               color = condition, fill = condition)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.25) +
    geom_point(data = individ_data, aes(x = condition, y = individ_task_time, color = condition),
               alpha = 0.5) +
    labs(x = "", y = ylab) +
    ggtitle(title) +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        # Change defaults to be blue/green instead of yellow/purple
                        begin = 0.25,
                        end = 0.75) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       # Change defaults to be blue/green instead of yellow/purple
                       begin = 0.25,
                       end = 0.75) +
    individ_plot_theme +
    theme(axis.text.x = element_blank(),
          plot.title = element_text(size = 32, face = "bold"))
}

# Bar chart of counts of feature references in coded explanations/descriptions, by condition
plot_coded_explanation_data = function(explanation_coded_summary) {
  explanation_coded_summary %>%
    ggplot(aes(x = measure, y = measure_mean,
               color = Condition, fill = Condition)) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"),
             width = 0.5, alpha = 0.5) +
    geom_errorbar(aes(ymin = measure_mean - measure_se, ymax = measure_mean + measure_se),
                  position = position_dodge(width = 0.5, preserve = "single"),
                  width = 0.25) +
    ggtitle("Explanation and description measures") +
    labs(x = "", y = "Mean number of references") +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        # Change defaults to be blue/green instead of yellow/purple
                        begin = 0.25,
                        end = 0.75) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       # Change defaults to be blue/green instead of yellow/purple
                       begin = 0.25,
                       end = 0.75) +
    individ_plot_theme

}


# DATA INITIALIZATION ==========================================================

# Read in data
summary_data = read_summary_data(SUMMARY_DATA)
trial_data = read_trial_data(TRIAL_DATA)
evaluation_data = read_evaluation_data(EVAL_DATA)
memory_data = read_memory_data(MEMORY_DATA)

# Coded explanation / description data
explanation_coded_data = read_coded_explanation_data(EXPLANATION_DATA_CODED)
explanation_coded_summary_subjects = get_explanation_coded_subj_summary(explanation_coded_data)
explanation_coded_summary = get_explanation_coded_summary(explanation_coded_summary_subjects)



# Summarize participant count before catch trials
summary_data %>%
  group_by(condition) %>%
  summarize(participants = n())


# Check for repeat users
summary_data %>%
  group_by(sona_survey_code) %>%
  summarize(completions = n()) %>%
  filter(completions > 1)

# Remove users that fail catch trial evaluation
CATCH_RULE = "If a lure combination has a red shape on the bottom, it will catch fish."
CATCH_RULE_CUTOFF = 80

catch_eval = evaluation_data %>%
  filter(rule_text == CATCH_RULE)

catch_eval %>%
  ggplot(aes(x = condition, y = input_rule_rating, color = condition)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_hline(yintercept = CATCH_RULE_CUTOFF, linetype = "dashed", color = "black", size = 1) +
  labs(x = "", y = "Catch trial rule evaluation") +
  individ_plot_theme +
  theme(legend.position = "none")

# Select participants that should be removed
catch_users = evaluation_data %>%
  filter(rule_text == CATCH_RULE & input_rule_rating > 80)
table(catch_users$condition)
unique(catch_users$subjID)


sd(summary_data$experiment_completion_time)
COMPLETION_TIME_CUTOFF = mean(summary_data$experiment_completion_time) + 5 * sd(summary_data$experiment_completion_time)
# COMPLETION_TIME_CUTOFF = 10000
# COMPLETION_TIME_CUTOFF = 2000


completion_time_users = summary_data %>%
  filter(experiment_completion_time > COMPLETION_TIME_CUTOFF) %>%
  select(subjID)

CATCH_USERS = c(catch_users$subjID, completion_time_users$subjID)

summary_data = summary_data %>%
  filter(!subjID %in% CATCH_USERS)
trial_data = trial_data %>%
  filter(!subjID %in% CATCH_USERS)
evaluation_data = evaluation_data %>%
  filter(!subjID %in% CATCH_USERS)
memory_data = memory_data %>%
  filter(!subjID %in% CATCH_USERS)

# NB: catch users filtered out of coded explanation/description data at earlier stage
# In order to generate accurate stats about coder agreement



# Summarize participant count after catch trials
summary_data %>%
  group_by(condition) %>%
  summarize(participants = n())


# Summarize data
evaluation_summary = get_evaluation_summary(evaluation_data)
completion_time_summary = get_time_summary(summary_data)
trial_time_subject_summary = get_trial_time_subj_summary(trial_data)
trial_time_summary = get_trial_time_summary(trial_time_subject_summary)
memory_subject_summary = get_memory_subj_summary(memory_data)
memory_summary = get_memory_summary(memory_subject_summary)





### SUMMARY

summary_data %>%
  group_by(condition) %>%
  summarize(N = n()) %>%
  rename("Condition" = condition)


### GENERATION ANALYSIS ========================================================

# Are there any condition differences in prediction accuracy?

prediction_summary = trial_data %>%
  group_by(condition, trial_index) %>%
  summarize(mean_accuracy = sum(input_correct) / n())

# NB: this plot not included in manuscript
prediction_summary %>%
  # filter(trial_index > 4) %>%
  ggplot(aes(x = trial_index, y = mean_accuracy, color = condition)) +
  geom_line() +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(x = "Trial index", y = "Accuracy") +
  # ggtitle("Prediction accuracy in each round") +
  scale_color_viridis(discrete = T,
                      name = element_blank()) +
  individ_plot_theme



### EVALUATION ANALYSIS ========================================================

evaluation_summary$condition = factor(evaluation_summary$condition,
                                      levels = c("Explain", "Describe"))

plot_evaluation_results(evaluation_summary)


# Wilcoxon signed-rank test showing that target rule is different from all other rules across both groups
eval_summary_other_rules = evaluation_data %>%
  filter(category != "target") %>%
  group_by(condition, subjID) %>%
  summarize(mean_subj_rating = mean(input_rule_rating))

eval_difference = evaluation_data %>%
  group_by(subjID, condition) %>%
  filter(category == "target") %>%
  inner_join(., eval_summary_other_rules, by = "subjID") %>%
  mutate(diff = input_rule_rating - mean_subj_rating) %>%
  select(subjID, condition.x, diff)

wil_exp = wilcox.test(eval_difference$diff[eval_difference$condition.x == "Explain"], exact = F)
wil_des = wilcox.test(eval_difference$diff[eval_difference$condition.x == "Describe"], exact = F)
report_wilcox_summary(wil_exp) # Explainers
report_wilcox_summary(wil_des) # Describers

# Target rule comparison
report_t_summary(
  t.test(
    evaluation_data$input_rule_rating[evaluation_data$condition == "Explain" &
                                        evaluation_data$category == "target"],
    evaluation_data$input_rule_rating[evaluation_data$condition == "Describe" &
                                        evaluation_data$category == "target"],
    var.equal = T))

# Distractor comparison
report_t_summary(
  t.test(
    evaluation_data$input_rule_rating[evaluation_data$condition == "Explain" &
                                        evaluation_data$category == "distractor"],
    evaluation_data$input_rule_rating[evaluation_data$condition == "Describe" &
                                        evaluation_data$category == "distractor"],
    var.equal = T))

# Abstract: color comparison
report_t_summary(
  t.test(
    evaluation_data$input_rule_rating[evaluation_data$condition == "Explain" &
                                        evaluation_data$category == "abstract_color"],
    evaluation_data$input_rule_rating[evaluation_data$condition == "Describe" &
                                        evaluation_data$category == "abstract_color"],
    var.equal = T))

# Abstract: shape comparison
report_t_summary(
  t.test(
    evaluation_data$input_rule_rating[evaluation_data$condition == "Explain" &
                                        evaluation_data$category == "abstract_shape"],
    evaluation_data$input_rule_rating[evaluation_data$condition == "Describe" &
                                        evaluation_data$category == "abstract_shape"],
    var.equal = T))



# EVALUATION MIXED EFFECTS ANALYSIS ============================================

ruleset_factors = c(
  "If a lure combination has a pointy shape on the bottom, it will catch fish." = "target",
  "If a lure combination has a yellow shape or a diamond on the bottom, it will catch fish." = "distractor",
  "If a lure combination has a top lure with bright colors that are more visible under water (red or yellow), it will catch fish." = "abstract_color",
  "If a lure combination has a rounded top shape that resembles a fish's body, it will catch fish." = "abstract_shape",
  "There is no pattern to which lure combinations catch fish: the results are random, but there are approximately equal numbers that catch fish and don’t." = "random",
  "If a lure combination has a red shape on the bottom, it will catch fish." = "misc25",
  "If a lure combination has a blue shape, it will catch fish." = "misc50",
  "If a lure combination has a purple dot on at least one of the lures, it will catch fish." = "misc75"
)

evaluation_data = evaluation_data %>%
  mutate(rule_factor = factor(ruleset_factors[rule_text]))
evaluation_data$condition = factor(evaluation_data$condition,
                                      levels = c("Explain", "Describe"))

# glimpse(evaluation_data)


m0 = lmer(formula = input_rule_rating ~ rule_factor + (1 | subjID),
          data = evaluation_data,
          REML = F)
m1 = lmer(formula = input_rule_rating ~ rule_factor + condition + (1 | subjID),
          data = evaluation_data,
          REML = F)
anova(m1, m0)

# Pairwise: difference between conditions for target and distractor
emmeans(m1,
        specs = pairwise ~ rule_factor + condition,
        adjust = "none")
# No main effects of condition for target or distractor
# contrast                                          estimate   SE   df t.ratio p.value
# distractor Explain - distractor Describe             1.251 2.22  166   0.565 0.5731
# target Explain - target Describe                     1.251 2.22  166   0.565 0.5731




# CONTENT ANALYSIS: EXPLANATIONS / DESCRIPTIONS ==============================

# Plot results
explanation_coded_summary$Condition = factor(explanation_coded_summary$Condition,
                                             levels = c("Explain", "Describe"))
plot_coded_explanation_data(explanation_coded_summary)


# Check mechanism effect: were explainers more likely to provide a mechanistic account?
t_mech = t.test(explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "mechanism_total" &
                                                                   explanation_coded_summary_subjects$Condition == "Explain"],
                explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "mechanism_total" &
                                                                   explanation_coded_summary_subjects$Condition == "Describe"],
                var.equal = T)
report_t_summary(t_mech) # Means are avg. number of references

# Analysis (copied from Williams & Lombrozo, 2010)
# First, do abstract feature references show a main effect of condition?
# i.e. do explainers make more abstract references?
abstract_data = explanation_coded_summary_subjects %>%
  filter(measure %in% c("shape_abstract_total", "color_abstract_total", "purple_dot_abstract_total"))
anova_abstract = aov(data = abstract_data, subject_total ~ Condition + measure)
summary(anova_abstract)

# Do concrete feature references show a main effect of condition?
# i.e. do control participants make more concrete references?
concrete_data = explanation_coded_summary_subjects %>%
  filter(measure %in% c("shape_concrete_total", "color_concrete_total", "purple_dot_concrete_total"))
anova_concrete = aov(data = concrete_data, subject_total ~ Condition + measure)
summary(anova_concrete)

# ANOVAs suggest main effect of condition on number of concrete and abstract features
# Below t-tests confirm direction/significance for each feature individually
# Shape
t_shape_abs = t.test(explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "shape_abstract_total" &
                                                                        explanation_coded_summary_subjects$Condition == "Explain"],
                     explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "shape_abstract_total" &
                                                                        explanation_coded_summary_subjects$Condition == "Describe"],
                     var.equal = T)
report_t_summary(t_shape_abs) # Means are avg. number of references

t_shape_conc = t.test(explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "shape_concrete_total" &
                                                                         explanation_coded_summary_subjects$Condition == "Explain"],
                      explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "shape_concrete_total" &
                                                                         explanation_coded_summary_subjects$Condition == "Describe"],
                      var.equal = T)
report_t_summary(t_shape_conc) # Means are avg. number of references

# Color
t_color_abs = t.test(explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "color_abstract_total" &
                                                                        explanation_coded_summary_subjects$Condition == "Explain"],
                     explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "color_abstract_total" &
                                                                        explanation_coded_summary_subjects$Condition == "Describe"],
                     var.equal = T)
report_t_summary(t_color_abs) # Means are avg. number of references

t_color_conc = t.test(explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "color_concrete_total" &
                                                                         explanation_coded_summary_subjects$Condition == "Explain"],
                      explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "color_concrete_total" &
                                                                         explanation_coded_summary_subjects$Condition == "Describe"],
                      var.equal = T)
report_t_summary(t_color_conc) # Means are avg. number of references

# Purple dot
t_dot_abs = t.test(explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "purple_dot_abstract_total" &
                                                                      explanation_coded_summary_subjects$Condition == "Explain"],
                   explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "purple_dot_abstract_total" &
                                                                      explanation_coded_summary_subjects$Condition == "Describe"],
                   var.equal = T)
report_t_summary(t_dot_abs) # Means are avg. number of references

t_dot_conc = t.test(explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "purple_dot_concrete_total" &
                                                                       explanation_coded_summary_subjects$Condition == "Explain"],
                    explanation_coded_summary_subjects$subject_total[explanation_coded_summary_subjects$measure == "purple_dot_concrete_total" &
                                                                       explanation_coded_summary_subjects$Condition == "Describe"],
                    var.equal = T)
report_t_summary(t_dot_conc) # Means are avg. number of references



# CONTENT ANALYSIS: EXPLANATIONS / DESCRIPTIOSN MIXED EFFECTS ==================

# Make data long form
explanation_coded_long = explanation_coded_data %>%
  gather(measure, references, `FINAL - total mechanisms`:`FINAL - concrete purple dot references`)
explanation_coded_long$measure = factor(explanation_coded_long$measure)
explanation_coded_long$Condition = factor(explanation_coded_long$Condition,
                                          levels = c("Explain", "Describe"))
glimpse(explanation_coded_long)


# NB: each of these takes 90-120s to fit
m0 = lmer(formula = references ~ measure + (1 + measure | Subject), # measure effect varies across subjects
          data = explanation_coded_long, REML = F)
m1 = lmer(formula = references ~ measure * Condition + (1 + measure | Subject),
          data = explanation_coded_long, REML = F)

# NB: the models above may produce singular fit, but simpler model comparison here generates nearly identical results
# m0 = lmer(formula = references ~ measure + (1 | Subject), # measure effect varies across subjects
#           data = explanation_coded_long, REML = F)
# m1 = lmer(formula = references ~ measure * Condition + (1 | Subject),
#           data = explanation_coded_long, REML = F)


# Effect of including the condition interaction
anova(m1, m0)
# X^2(10) = 183.5, p < 0.001


# 2. Pairwise: emmeans comparisons of abstract v. concrete references in each condition
emmeans(m1, specs = pairwise ~ measure * Condition)

# $contrasts
#  contrast                                                                                              estimate     SE  df z.ratio p.value
#  (FINAL - abstract color references Explain) - (FINAL - abstract color references Describe)             0.06762 0.0247 Inf   2.738 0.4001
#  (FINAL - abstract purple dot references Explain) - (FINAL - abstract purple dot references Describe)   0.00846 0.0235 Inf   0.359 1.0000
#  (FINAL - abstract shape references Explain) - (FINAL - abstract shape references Describe)             0.28916 0.0539 Inf   5.361 <.0001
#  (FINAL - concrete color references Explain) - (FINAL - concrete color references Describe)            -1.29648 0.1024 Inf -12.655 <.0001
#  (FINAL - concrete purple dot references Explain) - (FINAL - concrete purple dot references Describe)  -0.69594 0.0540 Inf -12.878 <.0001
#  (FINAL - concrete shape references Explain) - (FINAL - concrete shape references Describe)            -1.57992 0.0834 Inf -18.941 <.0001
#  (FINAL - total color references Explain) - (FINAL - total color references Describe)                  -1.22886 0.1057 Inf -11.630 <.0001

#  (FINAL - total mechanisms Explain) - (FINAL - total mechanisms Describe)                               0.42138 0.0528 Inf   7.976 <.0001

#  (FINAL - total purple dot references Explain) - (FINAL - total purple dot references Describe)        -0.68748 0.0533 Inf -12.901 <.0001
#  (FINAL - total shape references Explain) - (FINAL - total shape references Describe)                  -1.29076 0.0840 Inf -15.358 <.0001




# COVARIATE ANALYSIS: MEMORY ===================================================

# 1. Memory performance compared to chance
# NB: not doing binomial test here because we are looking at accuracy percentages for N subjects
t_mem_chance_exp = t.test(memory_subject_summary$subj_accuracy[memory_subject_summary$condition == "Explain"],
                          mu = 0.5,
                          equal.var = T)

t_mem_chance_desc = t.test(memory_subject_summary$subj_accuracy[memory_subject_summary$condition == "Describe"],
                           mu = 0.5,
                           equal.var = T)

t_mem_chance_exp
t_mem_chance_desc


# 2. Memory accuracy across conditions
plot_memory_data(memory_summary) # NB: this plot not included in manuscript

t.test(memory_subject_summary$subj_accuracy[memory_subject_summary$condition == "Describe"],
       memory_subject_summary$subj_accuracy[memory_subject_summary$condition == "Explain"],
       var.equal = T)



# COVARIATE ANALYSIS: TIME =====================================================

# 1. Overall time on task across conditions
# NB: this generates the plot but we display below with patchwork
individ_completion = summary_data %>%
  select(subjID, condition, experiment_completion_time) %>%
  # filter(experiment_completion_time <= 10000) %>%
  rename("individ_task_time" = experiment_completion_time)

time_on_task = plot_time_data(completion_time_summary, individ_completion, ylab = "Seconds", title = "Mean time on experiment")

# Do the two conditions spend significantly different amounts of time on the experiment?
t.test(summary_data$experiment_completion_time[summary_data$condition == "Describe"],
       summary_data$experiment_completion_time[summary_data$condition == "Explain"],
       var.equal = T) # Means are seconds on task


# 2. Time on evidence trials across conditions
individ_trials = trial_time_subject_summary %>%
  select(subjID, condition, mean_trial_completion) %>%
  # filter(mean_trial_completion <= 1000) %>%
  rename("individ_task_time" = mean_trial_completion)
# NB: this generates the plot but we display below with patchwork
time_on_trials = plot_time_data(trial_time_summary, individ_trials, ylab = "Seconds", title = "Mean time on trials")

# Do the two conditions spend significantly different amounts of time on each trial?
t.test(trial_time_subject_summary$mean_trial_completion[trial_time_subject_summary$condition == "Describe"],
       trial_time_subject_summary$mean_trial_completion[trial_time_subject_summary$condition == "Explain"],
       var.equal = T) # Means are seconds on trials

# Plot graphs from 1. and 2. above side by side with patchwork
time_on_task + time_on_trials # NB: this plot not included in manuscript



# APPENDIX =====================================================================



