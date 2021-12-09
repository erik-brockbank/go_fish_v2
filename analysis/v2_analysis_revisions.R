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

EXCLUDE = TRUE # toggle this to confirm results with or without exclusions


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


# DATA INITIALIZATION ====

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
  filter(rule_text == CATCH_RULE & input_rule_rating > CATCH_RULE_CUTOFF)
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

glimpse(summary_data)
if (EXCLUDE) {
  summary_data = summary_data %>%
    filter(!subjID %in% CATCH_USERS)
  trial_data = trial_data %>%
    filter(!subjID %in% CATCH_USERS)
  evaluation_data = evaluation_data %>%
    filter(!subjID %in% CATCH_USERS)
  memory_data = memory_data %>%
    filter(!subjID %in% CATCH_USERS)
}


# NB: catch users filtered out of coded explanation/description data at earlier stage
# In order to generate accurate stats about coder agreement


# Summarize data
evaluation_summary = get_evaluation_summary(evaluation_data)
memory_subject_summary = get_memory_subj_summary(memory_data)
memory_summary = get_memory_summary(memory_subject_summary)



# CONDITION SUMMARY
summary_data %>%
  group_by(condition) %>%
  summarize(N = n()) %>%
  rename("Condition" = condition)



# ANALYSIS: EVALUATION ====

# Figures
evaluation_summary$condition = factor(evaluation_summary$condition,
                                      levels = c("Explain", "Describe"))
plot_evaluation_results(evaluation_summary)


# Analysis
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

# Mixed effects analysis
# glimpse(evaluation_data)
m0 = lmer(formula = input_rule_rating ~ rule_factor + (1|subjID),
          data = evaluation_data,
          REML = F)
m1.main = lmer(formula = input_rule_rating ~ rule_factor + condition + (1|subjID),
               data = evaluation_data,
               REML = F)
m1.int = lmer(formula = input_rule_rating ~ rule_factor * condition + (1|subjID),
              data = evaluation_data,
              REML = F)

anova(m1.int, m1.main, m0, test = 'LRT')
# summary(m1)
# Pairwise: difference between conditions for target and distractor
emmeans(m1.int, specs = pairwise ~ rule_factor * condition)

# No significant differences between conditions
# contrast                                          estimate   SE   df t.ratio p.value
# target Describe - target Explain                     3.745 4.26 1113   0.879 1.0000
# distractor Describe - distractor Explain            -2.182 4.26 1113  -0.512 1.0000
# abstract_color Describe - abstract_color Explain    -2.262 4.26 1113  -0.531 1.0000
# abstract_shape Describe - abstract_shape Explain    -5.180 4.26 1113  -1.215 0.9980
# random Describe - random Explain                     2.402 4.26 1113   0.563 1.0000
# misc25 Describe - misc25 Explain                    -5.614 4.26 1113  -1.317 0.9953
# misc50 Describe - misc50 Explain                    -1.074 4.26 1113  -0.252 1.0000
# misc75 Describe - misc75 Explain                     0.153 4.26 1113   0.036 1.0000


# Individual rule t-tests
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



# ANALYSIS: EXPLANATION / DESCRIPTION CONTENT ====

# Figures
explanation_coded_summary$Condition = factor(explanation_coded_summary$Condition,
                                             levels = c("Explain", "Describe"))
plot_coded_explanation_data(explanation_coded_summary)


# Analysis
# Do explainers reference *more* features?
explanation_coded_data = explanation_coded_data %>%
  rowwise() %>%
  mutate(total_refs = sum(`FINAL - total shape references`, `FINAL - total color references`, `FINAL - total purple dot references`),
         total_abstract = sum(`FINAL - abstract shape references`, `FINAL - abstract color references`, `FINAL - abstract purple dot references`),
         total_concrete = sum(`FINAL - concrete shape references`, `FINAL - concrete color references`, `FINAL - concrete purple dot references`),
         total_mechanisms = `FINAL - total mechanisms`)
# glimpse(explanation_coded_data)

m0.total = glmer(formula = total_refs ~ (1 | Subject) + (1 | `Trial index`),
                 data = explanation_coded_data,
                 family = "poisson")
m1.total = glmer(formula = total_refs ~ Condition + (1 | Subject) + (1 | `Trial index`),
                 data = explanation_coded_data,
                 family = "poisson")
anova(m1.total, m0.total, test = 'LRT')
emmeans(m1.total, pairwise ~ Condition)
exp(1.334)
exp(-0.223)


# Interaction between condition and type
glimpse(explanation_coded_data)
explanation_coded_long = explanation_coded_data %>%
  select(Subject, Condition, `Trial index`, total_abstract, total_concrete, total_mechanisms) %>%
  pivot_longer(.,
               cols = total_abstract:total_mechanisms,
               names_to = "ref_type",
               names_prefix = "total_",
               values_to = "Count")
# sanity check the above
hist(explanation_coded_long$Count[explanation_coded_long$ref_type == "mechanisms"])
hist(explanation_coded_long$Count[explanation_coded_long$ref_type == "abstract"])
hist(explanation_coded_long$Count[explanation_coded_long$ref_type == "concrete"])


m0 = glmer(formula = Count ~ (1 | Subject) + (1 | `Trial index`),
           data = explanation_coded_long,
           family = "poisson")
m1.cond = glmer(formula = Count ~ Condition + (1 | Subject) + (1 | `Trial index`),
                data = explanation_coded_long,
                family = "poisson")
m1.main = glmer(formula = Count ~ Condition + ref_type + (1 | Subject) + (1 | `Trial index`),
                data = explanation_coded_long,
                family = "poisson")
m1.int = glmer(formula = Count ~ Condition * ref_type + (1 | Subject) + (1 | `Trial index`),
               data = explanation_coded_long,
               family = "poisson")

anova(m0, m1.cond, m1.main, m1.int, test = 'LRT')
emmeans(m1.int, pairwise ~ Condition * ref_type)

# $emmeans
# Condition ref_type   emmean     SE  df asymp.LCL asymp.UCL
# Describe  abstract   -2.616 0.1697 Inf     -2.95    -2.284
# Explain   abstract   -0.901 0.0909 Inf     -1.08    -0.723
# Describe  concrete    1.340 0.0628 Inf      1.22     1.463
# Explain   concrete   -0.839 0.0894 Inf     -1.01    -0.663
# Describe  mechanisms -3.284 0.2299 Inf     -3.73    -2.833
# Explain   mechanisms -0.861 0.0899 Inf     -1.04    -0.684

# $contrasts
# contrast                                 estimate     SE  df z.ratio p.value
# Describe abstract - Explain abstract      -1.7149 0.1868 Inf  -9.181 <.0001
# Describe concrete - Explain concrete       2.1785 0.0988 Inf  22.044 <.0001
# Describe mechanisms - Explain mechanisms  -2.4235 0.2424 Inf  -9.997 <.0001




