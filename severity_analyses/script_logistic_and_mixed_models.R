#---------------------------------------------------------------------------------------------------------------
# packages
library(tidyverse)
library(broom)
library(janitor)
library(car)
library(logistf)
library(emmeans)
library(DHARMa)
library(lme4)

#---------------------------------------------------------------------------------------------------------------
# main dataset
dataset_all_states <- data.table::fread("dataset_all_states.csv")

dataset_all_states <- dataset_all_states %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(age_cat = forcats::fct_relevel(age_cat, "15-65", "<5", "5-9", "10-14", ">65"),
         race = forcats::fct_relevel(race, "Branca/amarela", "Preta/parda/indígena", "ignorada/não info"),
         state_code = forcats::fct_relevel(state_code,"SP"),
         year = as.factor(as.character(year)),
         month = as.factor(as.character(month)))
str(dataset_all_states)

# dataset for notification rate
m_not_rate_all_states <- data.table::fread("monthly_notification_rate.csv")

m_not_rate_all_states <- m_not_rate_all_states %>% 
  mutate(year = as.factor(as.character(year)), 
         month = as.factor(as.character(month)),
         state_code = as.factor(state_code)) %>% 
  group_by(state_code) %>%
  mutate(z_rate = scale(notification_rate)) %>%
  ungroup()
str(m_not_rate_all_states)

m_not_rate_all_states %>%
  group_by(state_code) %>%
  summarise(
    media = round(mean(z_rate, na.rm = TRUE),4),
    sd = sd(z_rate, na.rm = TRUE),
    n_total = n()
  )

#---------------------------------------------------------------------------------------------------------------
# functions
#---------------------------------------------------------------------------------------------------------------

# relative frequencies and unajusted ORs
summarize_predictors <- function(data, vars, outcome = "den_sevc",
                                 yes_level = "sim", no_level = "não",
                                 digits_freq = 2, digits_or = 2, digits_p = 4) {
  
  
  make_freq_table <- function(level_value) {
    data %>%
      filter(.data[[outcome]] == level_value) %>%
      select(all_of(c(outcome, vars))) %>%
      pivot_longer(
        cols = all_of(vars),
        names_to = "predictor",
        values_to = "value"
      ) %>%
      count(predictor, value, .data[[outcome]], name = "n") %>%
      group_by(predictor, .data[[outcome]]) %>%
      mutate(freq_rel = round(n / sum(n), digits_freq)) %>%
      ungroup()
  }
  
  df_freq_yes <- make_freq_table(yes_level)
  df_freq_no  <- make_freq_table(no_level)
  
  fit_univariate_glm <- function(v) {
    f <- as.formula(paste(outcome, "~", v))
    m <- glm(f, data = data, family = binomial)
    
    coefs <- coef(m)
    
    est <- exp(coefs)[-1]
    ci  <- suppressWarnings(confint.default(m))[-1, , drop = FALSE]
    p   <- summary(m)$coefficients[-1, 4]
    
    data.frame(
      variable = v,
      term = names(est),
      OR = round(est, digits_or),
      IC95_low = round(exp(ci[, 1]), digits_or),
      IC95_high = round(exp(ci[, 2]), digits_or),
      p_value = round(p, digits_p),
      row.names = NULL
    )
  }
  
  or_glm <- lapply(vars, fit_univariate_glm) %>%
    bind_rows()
  
  list(
    freq_yes = df_freq_yes,
    freq_no = df_freq_no,
    or_glm = or_glm
  )
}

# adjusted ORs
fit_multiple_logistic_model <- function(data, outcome, predictors) {
  
  formula_model <- as.formula(
    paste(outcome, "~", paste(predictors, collapse = " + "))
  )
  
  mm1 <- glm(
    formula_model,
    family = binomial(),
    data = data
  )
  
  or_ci <- broom::tidy(mm1, exponentiate = TRUE, conf.int = TRUE) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::rename(
      OR = estimate,
      IC_low = conf.low,
      IC_up = conf.high,
      p_value = p.value
    )
  
  list(
    model = mm1,
    summary = summary(mm1),
    or_ci = or_ci
  )
}

# model diagnostic
plot_glm_diagnostics <- function(model) {
  
  
  scatter.smooth(
    statmod::qresid(model) ~ boot::logit(fitted(model)),
    las = 1,
    ylab = "resíduo quantílico",
    xlab = "valores ajustados (logito)"
  )
  
  scatter.smooth(
    residuals(model, type = "deviance") ~ predict(model, type = "link"),
    las = 1,
    ylab = "resíduo deviance",
    xlab = "valores ajustados (logito)"
  )
  
  arm::binnedplot(
    x = fitted(model),
    y = residuals(model, type = "pearson")
  )
  
  topmodels::qqrplot(model)
  
  
  car::influenceIndexPlot(
    model = model,
    vars = "hat"
  )
  
  car::influenceIndexPlot(
    model = model,
    vars = "Cook"
  )
  
  invisible(
    list(
      fitted = fitted(model),
      link = predict(model, type = "link"),
      qresid = statmod::qresid(model),
      deviance_resid = residuals(model, type = "deviance"),
      pearson_resid = residuals(model, type = "pearson"),
      hat = hatvalues(model),
      cooks_distance = cooks.distance(model)
    )
  )
}

#---------------------------------------------------------------------------------------------------------------
# SP
#---------------------------------------------------------------------------------------------------------------
# unajusted OR
dataset_SP <- dataset_all_states %>% 
  dplyr::filter(state_code == "SP")
summary(dataset_SP)

#-------------------------------------------------------------
vars <- setdiff(names(dataset_SP), c("den_sevc", "age", 
                                     "state_code", "notification_date", "month"))

#-------------------------------------------------------------
result_SP <- summarize_predictors(
  data = dataset_SP,
  vars = vars,
  outcome = "den_sevc",
  yes_level = "sim",
  no_level = "não"
)

result_SP$freq_yes
result_SP$freq_no
result_SP$or_glm

#-------------------------------------------------------------
# adjusted OR
result_mm_SP <- fit_multiple_logistic_model(data = dataset_SP, 
                                            outcome = "den_sevc",
                                            predictors = vars)
result_mm_SP$or_ci
#-------------------------------------------------------------
plot_glm_diagnostics(result_mm_SP$model)

#-------------------------------------------------------------
em <- emmeans(result_mm_SP$model, ~ serotype_infection)
contrast(em, method = "revpairwise", adjust = "tukey", 
         type = "response", infer = c(TRUE, TRUE))

#-------------------------------------------------------------
# adjusted OR with monthly notification rate as contextual covariate
dataset_SP2 <- left_join(dataset_SP, m_not_rate_all_states,
                         by = c("month", "year", "state_code"))
str(dataset_SP2)

# model
mm1 <- glmer(
  den_sevc ~ serotype_infection + comorbidities + sex + age_cat + race +
    z_rate + (1 | year:month),
  data = dataset_SP2,
  family = binomial,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  ))

summary(mm1)
performance::model_performance(mm1)

# diagnostic
res <- simulateResiduals(fittedModel = mm1)
plot(res)

testDispersion(res)
car::vif(mm1)

res_group <- recalculateResiduals(res, group = dataset_SP2$notification_date)
testTemporalAutocorrelation(res_group, time = unique(dataset_SP2$notification_date))

dataset_SP2 %>%
  mutate(res = residuals(res)) %>%
  group_by(notification_date) %>%
  summarise(mean_res = mean(res)) %>%
  ggplot(aes(x = notification_date, y = mean_res)) +
  geom_line() +
  geom_smooth()

# results
coef_tab <- summary(mm1)$coefficients[-1,]
result <- data.frame(
  term = rownames(coef_tab),
  beta = coef_tab[, "Estimate"],
  se = coef_tab[, "Std. Error"],
  z = coef_tab[, "z value"],
  OR = round(exp(coef_tab[, "Estimate"]),4),
  CIinf = round(exp(coef_tab[, "Estimate"] - 1.96 * coef_tab[, "Std. Error"]),4),
  CIsup = round(exp(coef_tab[, "Estimate"] + 1.96 * coef_tab[, "Std. Error"]),4),
  p_value = round(coef_tab[, "Pr(>|z|)"],5),
  row.names = NULL
)
result

writexl::write_xlsx(result, "MM_SP.xlsx")


#---------------------------------------------------------------------------------------------------------------
# PR
#---------------------------------------------------------------------------------------------------------------

# unajusted OR
dataset_PR <- dataset_all_states %>% 
  dplyr::filter(state_code == "PR")
summary(dataset_PR)

result_PR <- summarize_predictors(
  data = dataset_PR,
  vars = vars,
  outcome = "den_sevc",
  yes_level = "sim",
  no_level = "não"
)

result_PR$freq_yes
result_PR$freq_no
result_PR$or_glm

#-------------------------------------------------------------
# adjusted OR
result_mm_PR <- fit_multiple_logistic_model(data = dataset_PR,
                                            outcome = "den_sevc",
                                            predictors = vars)
result_mm_PR$or_ci
#-------------------------------------------------------------
plot_glm_diagnostics(result_mm_PR$model)

#-------------------------------------------------------------
em <- emmeans(result_mm_PR$model, ~ serotype_infection)
contrast(em, method = "revpairwise", adjust = "tukey", 
         type = "response", infer = c(TRUE, TRUE))

#-------------------------------------------------------------
# adjusted OR with monthly notification rate as covariate
dataset_PR2 <- left_join(dataset_PR, m_not_rate_all_states,
                         by = c("month", "year", "state_code"))

# model
mm1 <- glmer(
  den_sevc ~ serotype_infection + comorbidities + sex + age_cat + race +
    z_rate + (1 | year:month),
  data = dataset_PR2,
  family = binomial,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  ))

summary(mm1)
performance::model_performance(mm1)

# diagnostic
res <- simulateResiduals(fittedModel = mm1)
plot(res)

testDispersion(res)
car::vif(mm1)

res_group <- recalculateResiduals(res, group = dataset_PR2$notification_date)
testTemporalAutocorrelation(res_group, time = unique(dataset_PR2$notification_date))

dataset_PR2 %>%
  mutate(res = residuals(res)) %>%
  group_by(notification_date) %>%
  summarise(mean_res = mean(res)) %>%
  ggplot(aes(x = notification_date, y = mean_res)) +
  geom_line() +
  geom_smooth()

# results
coef_tab <- summary(mm1)$coefficients[-1,]
result <- data.frame(
  term = rownames(coef_tab),
  beta = coef_tab[, "Estimate"],
  se = coef_tab[, "Std. Error"],
  z = coef_tab[, "z value"],
  OR = round(exp(coef_tab[, "Estimate"]),4),
  CIinf = round(exp(coef_tab[, "Estimate"] - 1.96 * coef_tab[, "Std. Error"]),4),
  CIsup = round(exp(coef_tab[, "Estimate"] + 1.96 * coef_tab[, "Std. Error"]),4),
  p_value = round(coef_tab[, "Pr(>|z|)"],5),
  row.names = NULL
)
result

writexl::write_xlsx(result, "MM_PR.xlsx")

#---------------------------------------------------------------------------------------------------------------
# MS
#---------------------------------------------------------------------------------------------------------------

# unajusted OR
dataset_MS <- dataset_all_states %>% 
  dplyr::filter(state_code == "MS")
summary(dataset_MS)

result_MS <- summarize_predictors(
  data = dataset_MS,
  vars = vars,
  outcome = "den_sevc",
  yes_level = "sim",
  no_level = "não"
)

result_MS$freq_yes
result_MS$freq_no
result_MS$or_glm

#-------------------------------------------------------------
# adjusted OR
result_mm_MS <- fit_multiple_logistic_model(data = dataset_MS,
                                            outcome = "den_sevc",
                                            predictors = vars)
result_mm_MS$or_ci
#-------------------------------------------------------------
plot_glm_diagnostics(result_mm_MS$model)

#-------------------------------------------------------------
em <- emmeans(result_mm_MS$model, ~ serotype_infection)
contrast(em, method = "revpairwise", adjust = "tukey", 
         type = "response", infer = c(TRUE, TRUE))

#-------------------------------------------------------------
# adjusted OR with monthly notification rate as covariate
dataset_MS2 <- left_join(dataset_MS, m_not_rate_all_states,
                         by = c("month", "year", "state_code"))

# model
mm1 <- glmer(
  den_sevc ~ serotype_infection + comorbidities + sex + age_cat + race +
    z_rate + (1 | year:month),
  data = dataset_MS2,
  family = binomial,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  ))

summary(mm1)
performance::model_performance(mm1)

# diagnostic
res <- simulateResiduals(fittedModel = mm1)
plot(res)

testDispersion(res)
car::vif(mm1)

res_group <- recalculateResiduals(res, group = dataset_MS2$notification_date)
testTemporalAutocorrelation(res_group, time = unique(dataset_MS2$notification_date))

dataset_MS2 %>%
  mutate(res = residuals(res)) %>%
  group_by(notification_date) %>%
  summarise(mean_res = mean(res)) %>%
  ggplot(aes(x = notification_date, y = mean_res)) +
  geom_line() +
  geom_smooth()

# results
coef_tab <- summary(mm1)$coefficients[-1,]
result <- data.frame(
  term = rownames(coef_tab),
  beta = coef_tab[, "Estimate"],
  se = coef_tab[, "Std. Error"],
  z = coef_tab[, "z value"],
  OR = round(exp(coef_tab[, "Estimate"]),4),
  CIinf = round(exp(coef_tab[, "Estimate"] - 1.96 * coef_tab[, "Std. Error"]),4),
  CIsup = round(exp(coef_tab[, "Estimate"] + 1.96 * coef_tab[, "Std. Error"]),4),
  p_value = round(coef_tab[, "Pr(>|z|)"],5),
  row.names = NULL
)
result

writexl::write_xlsx(result, "MM_MS.xlsx")

#---------------------------------------------------------------------------------------------------------------
# MG
#---------------------------------------------------------------------------------------------------------------

# unajusted OR
dataset_MG <- dataset_all_states %>% 
  dplyr::filter(state_code == "MG")
summary(dataset_MG)

result_MG <- summarize_predictors(
  data = dataset_MG,
  vars = vars,
  outcome = "den_sevc",
  yes_level = "sim",
  no_level = "não"
)

result_MG$freq_yes
result_MG$freq_no
result_MG$or_glm

#-------------------------------------------------------------
# adjusted OR
result_mm_MG <- fit_multiple_logistic_model(data = dataset_MG,
                                            outcome = "den_sevc",
                                            predictors = vars)
result_mm_MG$or_ci
#-------------------------------------------------------------
plot_glm_diagnostics(result_mm_MG$model)

#-------------------------------------------------------------
em <- emmeans(result_mm_MG$model, ~ serotype_infection)
contrast(em, method = "revpairwise", adjust = "tukey", 
         type = "response", infer = c(TRUE, TRUE))

#-------------------------------------------------------------
# adjusted OR with monthly notification rate as covariate
dataset_MG2 <- left_join(dataset_MG, m_not_rate_all_states,
                         by = c("month", "year", "state_code"))

# model
mm1 <- glmer(
  den_sevc ~ serotype_infection + comorbidities + sex + age_cat + race +
    z_rate + (1 | year:month),
  data = dataset_MG2,
  family = binomial,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  ))

summary(mm1)
performance::model_performance(mm1)

# diagnostic
res <- simulateResiduals(fittedModel = mm1)
plot(res)

testDispersion(res)
car::vif(mm1)

res_group <- recalculateResiduals(res, group = dataset_MG2$notification_date)
testTemporalAutocorrelation(res_group, time = unique(dataset_MG2$notification_date))

dataset_MG2 %>%
  mutate(res = residuals(res)) %>%
  group_by(notification_date) %>%
  summarise(mean_res = mean(res)) %>%
  ggplot(aes(x = notification_date, y = mean_res)) +
  geom_line() +
  geom_smooth()

# results
coef_tab <- summary(mm1)$coefficients[-1,]
result <- data.frame(
  term = rownames(coef_tab),
  beta = coef_tab[, "Estimate"],
  se = coef_tab[, "Std. Error"],
  z = coef_tab[, "z value"],
  OR = round(exp(coef_tab[, "Estimate"]),4),
  CIinf = round(exp(coef_tab[, "Estimate"] - 1.96 * coef_tab[, "Std. Error"]),4),
  CIsup = round(exp(coef_tab[, "Estimate"] + 1.96 * coef_tab[, "Std. Error"]),4),
  p_value = round(coef_tab[, "Pr(>|z|)"],5),
  row.names = NULL
)
result

writexl::write_xlsx(result, "MM_MG.xlsx")

#---------------------------------------------------------------------------------------------------------------
# 2 years or less
#---------------------------------------------------------------------------------------------------------------

# unajusted OR
dataset_2y <- dataset_all_states %>%
  dplyr::filter(age <= 2) 
summary(dataset_2y)

vars <- setdiff(names(dataset_2y), c("den_sevc", "age",
                                     "age_cat", "notification_date", "month"))

result_2y <- summarize_predictors(
  data = dataset_2y,
  vars = vars,
  outcome = "den_sevc",
  yes_level = "sim",
  no_level = "não"
)

result_2y$freq_yes
result_2y$freq_no
result_2y$or_glm

#-------------------------------------------------------------
# adjusted OR
result_mm_2y <- fit_multiple_logistic_model(data = dataset_2y,
                                            outcome = "den_sevc",
                                            predictors = vars)
result_mm_2y$or_ci

#-------------------------------------------------------------
plot_glm_diagnostics(result_mm_2y$model)

#-------------------------------------------------------------
# adjusted OR with monthly notification rate as contextual covariate

dataset_2y2 <- left_join(dataset_2y, m_not_rate_all_states,
                         by = c("month", "year", "state_code"))


mm1 <- glmer(
  den_sevc ~ serotype_infection + comorbidities + sex + race + 
    z_rate + (1 | state_code/year:month),
  data = dataset_2y2,
  family = binomial,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  ))

summary(mm1)
performance::model_performance(mm1)

# diagnostic
res <- simulateResiduals(fittedModel = mm1)
plot(res)

testDispersion(res)
car::vif(mm1)

res_group <- recalculateResiduals(res, group = dataset_2y2$notification_date)
testTemporalAutocorrelation(res_group, time = unique(dataset_2y2$notification_date))

dataset_2y2 %>%
  mutate(res = residuals(res)) %>%
  group_by(notification_date) %>%
  summarise(mean_res = mean(res)) %>%
  ggplot(aes(x = notification_date, y = mean_res)) +
  geom_line() +
  geom_smooth()

# results
coef_tab <- summary(mm1)$coefficients[-1,]
result <- data.frame(
  term = rownames(coef_tab),
  beta = coef_tab[, "Estimate"],
  se = coef_tab[, "Std. Error"],
  z = coef_tab[, "z value"],
  OR = round(exp(coef_tab[, "Estimate"]),4),
  CIinf = round(exp(coef_tab[, "Estimate"] - 1.96 * coef_tab[, "Std. Error"]),4),
  CIsup = round(exp(coef_tab[, "Estimate"] + 1.96 * coef_tab[, "Std. Error"]),4),
  p_value = round(coef_tab[, "Pr(>|z|)"],5),
  row.names = NULL
)
result

writexl::write_xlsx(result, "MM_2y2.xlsx")

#---------------------------------------------------------------------------------------------------------------
# 1 year or less
#---------------------------------------------------------------------------------------------------------------

# unajusted OR
dataset_1y <- dataset_all_states %>%
  dplyr::filter(age <= 1) 
summary(dataset_1y)

result_1y <- summarize_predictors(
  data = dataset_1y,
  vars = vars,
  outcome = "den_sevc",
  yes_level = "sim",
  no_level = "não"
)

result_1y$freq_yes
result_1y$freq_no
result_1y$or_glm

#-------------------------------------------------------------
# adjusted OR
result_mm_1y <- fit_multiple_logistic_model(data = dataset_1y,
                                            outcome = "den_sevc",
                                            predictors = vars)
result_mm_1y$or_ci
#-------------------------------------------------------------
plot_glm_diagnostics(result_mm_1y$model)

#-------------------------------------------------------------
# adjusted OR with monthly notification rate as covariate
dataset_1y2 <- left_join(dataset_1y, m_not_rate_all_states,
                         by = c("month", "year", "state_code"))

mm1 <- glmer(
  den_sevc ~ serotype_infection + comorbidities + sex + race + 
    z_rate + (1 | state_code/year:month),
  data = dataset_1y2,
  family = binomial,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  ))

summary(mm1)
performance::model_performance(mm1)

# diagnostic
res <- simulateResiduals(fittedModel = mm1)
plot(res)

testDispersion(res)
car::vif(mm1)

res_group <- recalculateResiduals(res, group = dataset_1y2$notification_date)
testTemporalAutocorrelation(res_group, time = unique(dataset_1y2$notification_date))

dataset_1y2 %>%
  mutate(res = residuals(res)) %>%
  group_by(notification_date) %>%
  summarise(mean_res = mean(res)) %>%
  ggplot(aes(x = notification_date, y = mean_res)) +
  geom_line() +
  geom_smooth()

# results
coef_tab <- summary(mm1)$coefficients[-1,]
result <- data.frame(
  term = rownames(coef_tab),
  beta = coef_tab[, "Estimate"],
  se = coef_tab[, "Std. Error"],
  z = coef_tab[, "z value"],
  OR = round(exp(coef_tab[, "Estimate"]),4),
  CIinf = round(exp(coef_tab[, "Estimate"] - 1.96 * coef_tab[, "Std. Error"]),4),
  CIsup = round(exp(coef_tab[, "Estimate"] + 1.96 * coef_tab[, "Std. Error"]),4),
  p_value = round(coef_tab[, "Pr(>|z|)"],5),
  row.names = NULL
)
result

writexl::write_xlsx(result, "MM_1y2.xlsx")

#---------------------------------------------------------------------------------------------------------------
# 0 year
#---------------------------------------------------------------------------------------------------------------

# unajusted OR
dataset_0y <- dataset_all_states %>%
  dplyr::filter(age < 1) 
summary(dataset_0y)

result_0y <- summarize_predictors(
  data = dataset_0y,
  vars = vars,
  outcome = "den_sevc",
  yes_level = "sim",
  no_level = "não"
)

result_0y$freq_yes
result_0y$freq_no
result_0y$or_glm

#-------------------------------------------------------------
# adjusted OR
result_mm_0y <- fit_multiple_logistic_model(data = dataset_0y,
                                            outcome = "den_sevc",
                                            predictors = vars)
result_mm_0y$or_ci
#-------------------------------------------------------------
plot_glm_diagnostics(result_mm_0y$model)

#-------------------------------------------------------------
# adjusted OR with monthly notification rate as covariate
dataset_0y2 <- left_join(dataset_0y, m_not_rate_all_states,
                         by = c("month", "year", "state_code"))

mm1 <- glmer(
  den_sevc ~ serotype_infection + comorbidities + sex + race + 
    z_rate + (1 | state_code/year:month),
  data = dataset_0y2,
  family = binomial,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  ))

summary(mm1)
performance::model_performance(mm1)

# diagnostic
res <- simulateResiduals(fittedModel = mm1)
plot(res)

testDispersion(res)
car::vif(mm1)

res_group <- recalculateResiduals(res, group = dataset_0y2$notification_date)
testTemporalAutocorrelation(res_group, time = unique(dataset_0y2$notification_date))

dataset_0y2 %>%
  mutate(res = residuals(res)) %>%
  group_by(notification_date) %>%
  summarise(mean_res = mean(res)) %>%
  ggplot(aes(x = notification_date, y = mean_res)) +
  geom_line() +
  geom_smooth()

# results
coef_tab <- summary(mm1)$coefficients[-1,]
result <- data.frame(
  term = rownames(coef_tab),
  beta = coef_tab[, "Estimate"],
  se = coef_tab[, "Std. Error"],
  z = coef_tab[, "z value"],
  OR = round(exp(coef_tab[, "Estimate"]),4),
  CIinf = round(exp(coef_tab[, "Estimate"] - 1.96 * coef_tab[, "Std. Error"]),4),
  CIsup = round(exp(coef_tab[, "Estimate"] + 1.96 * coef_tab[, "Std. Error"]),4),
  p_value = round(coef_tab[, "Pr(>|z|)"],5),
  row.names = NULL
)
result

writexl::write_xlsx(result, "MM_0y2.xlsx")
