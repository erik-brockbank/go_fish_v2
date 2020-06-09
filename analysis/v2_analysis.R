#' 
#' Analysis for go_fish v2
#' 


rm(list = ls())
setwd("~/web/go_fish_v2/analysis/")

library(tidyverse)
library(viridis)
library(patchwork)


# GLOBALS ======================================================================
# Data files
SUMMARY_DATA = "01_go_fish_v2_meta.csv"
TRIAL_DATA = "02_go_fish_v2_trials.csv"
EVAL_DATA = "03_go_fish_v2_evaluation.csv"
MEMORY_DATA = "04_go_fish_v2_memory.csv"




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


# Summarize evaluation data across participants and conditions for target and non-target rules
get_evaluation_summary = function(evaluation_data) {
  # Average rating across participants on target rule
  eval_summary_target_rule = evaluation_data %>%
    filter(category == "target") %>% # for target rule, summarize across participants
    group_by(condition) %>%
    summarize(ruleset = "Target",
              mean_rating = mean(input_rule_rating),
              subjects = n(),
              se_rating = sd(input_rule_rating) / sqrt(n()),
              ci_lower = mean_rating - se_rating,
              ci_upper = mean_rating + se_rating)
  
  # Average rating across participants on distractor rule
  eval_summary_distractor_rule = evaluation_data %>%
    filter(category == "distractor") %>% # for distractor rule, summarize across participants
    group_by(condition) %>%
    summarize(ruleset = "Distractor",
              mean_rating = mean(input_rule_rating),
              subjects = n(),
              se_rating = sd(input_rule_rating) / sqrt(n()),
              ci_lower = mean_rating - se_rating,
              ci_upper = mean_rating + se_rating)
  
  eval_summary_abstract_color = evaluation_data %>%
    filter(category == "abstract_color") %>% # for abstract color rule, summarize across participants
    group_by(condition) %>%
    summarize(ruleset = "Abstract (color)",
              mean_rating = mean(input_rule_rating),
              subjects = n(),
              se_rating = sd(input_rule_rating) / sqrt(n()),
              ci_lower = mean_rating - se_rating,
              ci_upper = mean_rating + se_rating)
  
  eval_summary_abstract_shape = evaluation_data %>%
    filter(category == "abstract_shape") %>% # for abstract shape rule, summarize across participants
    group_by(condition) %>%
    summarize(ruleset = "Abstract (shape)",
              mean_rating = mean(input_rule_rating),
              subjects = n(),
              se_rating = sd(input_rule_rating) / sqrt(n()),
              ci_lower = mean_rating - se_rating,
              ci_upper = mean_rating + se_rating)
    
  
  # Average of each participant's average across non-target rules
  eval_summary_other_rules = evaluation_data %>%
    filter(category %in% c("misc", "rand")) %>% # for all other rules, summarize average of participant avgs
    group_by(condition, subjID) %>%
    summarize(mean_subj_rating = mean(input_rule_rating),
              rules = n()) %>%
    group_by(condition) %>%
    summarize(ruleset = "All other rules",
              mean_rating = mean(mean_subj_rating),
              subjects = n(),
              se_rating = sd(mean_subj_rating) / sqrt(n()),
              ci_lower = mean_rating - se_rating,
              ci_upper = mean_rating + se_rating)
  
  eval_summary = rbind(eval_summary_target_rule, eval_summary_distractor_rule, 
                       eval_summary_abstract_color, eval_summary_abstract_shape,
                       eval_summary_other_rules)
  return(eval_summary)
}


# Summarize experiment completion time data
get_time_summary = function(time_data) {
  time_summary = time_data %>%
    group_by(condition) %>%
    summarize(mean_task_time = mean(experiment_completion_time),
              subjects = n(),
              se_task_time = sd(experiment_completion_time) / sqrt(subjects),
              ci_lower = mean_task_time - se_task_time,
              ci_upper = mean_task_time + se_task_time)
  return(time_summary)
}


# Summarize average trial completion time by participant
get_trial_time_subj_summary = function(trial_data) {
  trial_subject_summary = trial_data %>%
    mutate(trial_completion_time = (trial_n_end_ts - trial_n_start_ts) / 1000) %>%
    group_by(condition, subjID) %>%
    summarize(mean_trial_completion = mean(trial_completion_time),
              trials = n(),
              se_trial_completion = sd(trial_completion_time) / sqrt(trials),
              ci_lower = mean_trial_completion - se_trial_completion,
              ci_upper = mean_trial_completion + se_trial_completion)
  return(trial_subject_summary)
}


# Get summary of time spent on trials in each condition across participants
get_trial_time_summary = function(trial_time_subj_summary) {
  trial_time_summary = trial_time_subj_summary %>%
    group_by(condition) %>%
    # These column names kept the same as those in `get_time_summary` for easier graphing
    summarize(mean_task_time = mean(mean_trial_completion),
              subjects = n(),
              se_trial_time = sd(mean_trial_completion) / sqrt(subjects),
              ci_lower = mean_task_time - se_trial_time,
              ci_upper = mean_task_time + se_trial_time)
  return(trial_time_summary)
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
    scale_x_discrete(name = element_blank()) +
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
    scale_y_continuous(breaks = seq(0, 100, by = 10)) +
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
plot_time_data = function(time_summary, ylab, ymax, title) {
  time_summary %>%
    ggplot(aes(x = condition, y = mean_task_time, 
               color = condition, fill = condition)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.25) +
    ylim(0, ymax) +
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


# DATA INITIALIZATION ==========================================================
# Read in data
summary_data = read_summary_data(SUMMARY_DATA)
trial_data = read_trial_data(TRIAL_DATA)
evaluation_data = read_evaluation_data(EVAL_DATA)
memory_data = read_memory_data(MEMORY_DATA)

# Summarize data
evaluation_summary = get_evaluation_summary(evaluation_data)

completion_time_summary = get_time_summary(summary_data)
trial_time_subject_summary = get_trial_time_subj_summary(trial_data)
trial_time_summary = get_trial_time_summary(trial_time_subject_summary)
memory_subject_summary = get_memory_subj_summary(memory_data)
memory_summary = get_memory_summary(memory_subject_summary)




# DATA ANALYSIS ================================================================

### GENERATION


### EVALUATION
plot_evaluation_results(evaluation_summary)

# TODO decide which analyses from v1 analysis/cog sci write-up to include here





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
plot_memory_data(memory_summary)

t.test(memory_subject_summary$subj_accuracy[memory_subject_summary$condition == "Describe"],
       memory_subject_summary$subj_accuracy[memory_subject_summary$condition == "Explain"],
       var.equal = T)



# COVARIATE ANALYSIS: TIME =====================================================

# 1. Overall time on task across conditions
# NB: this generates the plot but we display below with patchwork
time_on_task = plot_time_data(completion_time_summary, ylab = "Seconds", ymax = 1000, title = "Mean time on experiment")

t.test(summary_data$experiment_completion_time[summary_data$condition == "Describe"],
       summary_data$experiment_completion_time[summary_data$condition == "Explain"],
       var.equal = T) # Means are seconds on task


# 2. Time on evidence trials across conditions
# NB: this generates the plot but we display below with patchwork
time_on_trials = plot_time_data(trial_time_summary, ylab = "Seconds", ymax = 80, title = "Mean time on trials")

t.test(trial_time_subject_summary$mean_trial_completion[trial_time_subject_summary$condition == "Describe"],
       trial_time_subject_summary$mean_trial_completion[trial_time_subject_summary$condition == "Explain"],
       var.equal = T) # Means are seconds on trials

# Plot graphs from 1. and 2. above side by side with patchwork
time_on_task + time_on_trials







