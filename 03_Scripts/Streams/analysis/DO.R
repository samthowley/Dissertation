#call in data#######

DO<-chem%>%
  select(Date, ID, DO, pH, SpC, Temp_DO, depth, Q)%>%
  mutate(day=as.Date(Date))%>%
  left_join(master_metabolism)%>%
  mutate(
    Basin=case_when(ID=='5'~'5',ID=='5a'~'5',ID=='15'~'15',
                    ID=='3'~'6',ID=='7'~'7',ID=='6'~'6',ID=='6a'~'6',
                    ID=='9'~'9', ID=='13'~'13')
  )%>%left_join(wetland_cover)%>%  left_join(watershed_inundation)%>%
  select(-AREA, -Basin)%>%
  distinct(Date, ID, .keep_all = T)%>%
  left_join(gw_corrected_metabolism)%>%
  drop_na()%>%
  distinct(day, ID, .keep_all = T)

#global rf#############
rf.df<-DO%>%
  select(DO,
         SpC,Temp_DO, depth, Q, wetland.perc,
         watershed.inundation)

set.seed(123)

n <- nrow(rf.df)
train_idx <- sample(seq_len(n), size = floor(0.8 * n))

rf_train <- rf.df[train_idx, ]
rf_test  <- rf.df[-train_idx, ]

k <- 5
folds <- sample(rep(1:k, length.out = nrow(rf_train)))

mtry_vals <- 2:(ncol(rf_train) - 1)

cv_results <- expand.grid(
  mtry = mtry_vals,
  fold = 1:k
)

cv_results$RMSE <- NA

for (i in seq_len(nrow(cv_results))) {

  m <- cv_results$mtry[i]
  f <- cv_results$fold[i]

  train_fold <- rf_train[folds != f, ]
  test_fold  <- rf_train[folds == f, ]

  rf_fit <- randomForest(
    DO ~ .,
    data = train_fold,
    ntree = 500,
    mtry = m,
    importance = FALSE
  )

  preds <- predict(rf_fit, test_fold)
  cv_results$RMSE[i] <- sqrt(mean((preds - test_fold$DO)^2))
}

cv_summary <- cv_results %>%
  group_by(mtry) %>%
  summarise(mean_RMSE = mean(RMSE))

best_mtry <- cv_summary$mtry[which.min(cv_summary$mean_RMSE)]
best_mtry


rf_final <- randomForest(
  DO ~ .,
  data = rf_train,
  ntree = 1000,
  mtry = best_mtry,
  importance = TRUE
)

print(rf_final)


ggplot(pred_df, aes(x = obs, y = pred)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(intercept = 0, slope = 1, linewidth = 1) +
  coord_equal() +
  theme_bw() +
  labs(
    x = "Observed internal DO",
    y = "Predicted internal DO",
    title = "Random forest: predicted vs observed",
    subtitle = paste0("RMSE = ", round(rmse, 3), " | R\u00B2 = ", round(r2, 3))
  )

varImpPlot(rf_final)
importance(rf_final)



#by site##########

# 1) Daily aggregation
rf.df <- DO %>%
  group_by(ID, day) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  select(
    ID,
    DO,
    Q,
    Temp_DO,
    watershed.inundation,
    depth,
    SpC
  ) %>%
  drop_na()

# 2) Define flow state using a GLOBAL mean Q (change if you want site-specific)
Q_bar <- mean(rf.df$Q, na.rm = TRUE)

rf.df <- rf.df %>%
  mutate(
    flow.state = case_when(
      Q >  Q_bar ~ "highflow",
      Q <= Q_bar ~ "baseflow"
    ),
    flow.state = factor(flow.state, levels = c("baseflow", "highflow"))
  )

set.seed(123)

min_n <- 30
k <- 5
repeats <- 10

site_ids <- sort(unique(rf.df$ID))

perf_list <- list()
imp_list  <- list()

for (s in site_ids) {

  message("Running site: ", s)

  site_df <- rf.df %>% filter(ID == s)

  if (nrow(site_df) < min_n) {
    message("  Skipped (n < ", min_n, ")")
    next
  }

  rep_perf <- vector("list", repeats)
  rep_imp  <- vector("list", repeats)

  for (r in seq_len(repeats)) {

    n <- nrow(site_df)
    train_idx <- sample(seq_len(n), size = floor(0.8 * n))

    # keep ID out of model
    train <- site_df[train_idx, ] %>% select(-ID)
    test  <- site_df[-train_idx, ] %>% select(-ID)

    # CV folds inside training set
    folds <- sample(rep(1:k, length.out = nrow(train)))
    mtry_vals <- 2:(ncol(train) - 1)  # excludes response

    rmse_by_mtry <- numeric(length(mtry_vals))

    for (j in seq_along(mtry_vals)) {

      m <- mtry_vals[j]
      rmse_fold <- numeric(k)

      for (f in 1:k) {
        tr <- train[folds != f, ]
        te <- train[folds == f, ]

        rf_fit <- randomForest(
          DO ~ .,
          data = tr,
          ntree = 500,
          mtry = m
        )

        preds <- predict(rf_fit, te)
        rmse_fold[f] <- sqrt(mean((preds - te$DO)^2, na.rm = TRUE))
      }

      rmse_by_mtry[j] <- mean(rmse_fold, na.rm = TRUE)
    }

    best_mtry <- mtry_vals[which.min(rmse_by_mtry)]

    # Final model
    rf_final <- randomForest(
      DO ~ .,
      data = train,
      ntree = 500,
      mtry = best_mtry,
      importance = TRUE
    )

    pred <- as.numeric(predict(rf_final, test))

    rmse <- sqrt(mean((pred - test$DO)^2, na.rm = TRUE))

    # guard against NA when variance is zero
    r2 <- suppressWarnings(cor(pred, test$DO, use = "complete.obs")^2)
    if (is.na(r2)) r2 <- 0

    rep_perf[[r]] <- data.frame(
      ID = s,
      n = nrow(site_df),
      rep = r,
      RMSE = rmse,
      R2 = r2,
      mtry = best_mtry
    )

    imp <- importance(rf_final)[, "%IncMSE"]
    rep_imp[[r]] <- data.frame(
      ID = s,
      rep = r,
      variable = names(imp),
      importance = as.numeric(imp)
    )
  }

  perf_list[[as.character(s)]] <- bind_rows(rep_perf)
  imp_list[[as.character(s)]]  <- bind_rows(rep_imp)
}

site_perf_rep <- bind_rows(perf_list)
site_imp_rep  <- bind_rows(imp_list)

# Summarize performance with uncertainty
site_perf_summary <- site_perf_rep %>%
  group_by(ID, n) %>%
  summarise(
    RMSE_mean = mean(RMSE, na.rm = TRUE),
    RMSE_sd   = sd(RMSE, na.rm = TRUE),
    R2_mean   = mean(R2, na.rm = TRUE),
    R2_sd     = sd(R2, na.rm = TRUE),
    mtry_mode = as.integer(names(sort(table(mtry), decreasing = TRUE))[1]),
    .groups = "drop"
  )

site_perf_summary


# --- NRMSE FIX: normalize by SD of the RESPONSE (DO), by site ---
sd_by_site <- rf.df %>%
  group_by(ID) %>%
  summarise(sd_y = sd(DO, na.rm = TRUE), .groups = "drop")

site_perf_rep <- site_perf_rep %>%
  left_join(sd_by_site, by = "ID") %>%
  mutate(NRMSE = ifelse(is.finite(sd_y) & sd_y > 0, RMSE / sd_y, NA_real_))

# Normalize importance within each site+rep so it sums to 1 per model run
site_imp_norm <- site_imp_rep %>%
  group_by(ID, rep) %>%
  mutate(rel_importance = importance / sum(importance, na.rm = TRUE)) %>%
  ungroup()

imp_summary <- site_imp_norm %>%
  group_by(variable) %>%
  summarise(
    mean_imp = mean(rel_importance, na.rm = TRUE),
    sd_imp   = sd(rel_importance, na.rm = TRUE),
    n_sites  = n_distinct(ID),
    .groups = "drop"
  )

imp_summary

best_by_site <- site_imp_norm %>%
  group_by(ID) %>%
  slice_max(order_by = importance, n = 1, with_ties = FALSE) %>%
  ungroup()
best_by_site

site_top_driver <- site_imp_norm %>%
  group_by(ID) %>%
  slice_max(rel_importance, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(variable)
site_top_driver


site_codominant <- site_imp_norm %>%
  group_by(ID) %>%
  mutate(max_imp = max(rel_importance)) %>%
  filter(rel_importance >= 0.9 * max_imp) %>%
  ungroup() %>%
  arrange(ID, desc(rel_importance))
site_codominant


plot_df <- site_imp_norm %>%
  group_by(ID, variable) %>%
  summarise(rel_imp = mean(rel_importance, na.rm = TRUE), .groups = "drop") %>%
  group_by(ID) %>%                                # re-normalize just in case
  mutate(rel_imp = rel_imp / sum(rel_imp, na.rm = TRUE)) %>%
  ungroup()

ggplot(plot_df, aes(x = ID, y = rel_imp, fill = variable)) +
  geom_col(width = 0.85) +
  theme_bw() +
  labs(x = "Site", y = "Relative importance") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  ggtitle("Relative Importance of Factors Influencing the Internal Pathway")


#lmm#############

lm.df<-DO%>%
  group_by(ID, day) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")%>%
  select(
    DO,
    Q,
    Temp_DO,
    watershed.inundation,
    depth,
    SpC,
    ID,
  ) %>%
  drop_na()

cont_vars <- c("Q", "depth", "Temp_DO", "watershed.inundation", "SpC")

lm_df <- lm.df %>%
  mutate(across(all_of(cont_vars), scale))


lmm_final <- lmer(
  DO ~ Q + Temp_DO + depth + watershed.inundation + SpC +
    (1 | ID),
  data = lm_df,
  REML = FALSE)
summary(lmm_final)
vif(lm_check)

r2_nakagawa(lmm_global)

lmm_drop <- drop1(lmm_global, test = "Chisq")
lmm_drop

r2_parts <- partR2(
  lmm_final,
  partvars = c("Temp_DO", "Q"),
  data = lm_df,
  R2_type = "marginal",
  nboot = 1000
)
summary(r2_parts)


#By Site############

nested_df <- lm_df %>%
  group_by(ID) %>%
  nest()

get_site_R2 <- function(df) {

  # Must have enough rows to fit reliably
  if (nrow(df) < 20) return(tibble())

  # Fit per-site LM (NO random effect inside a single site)
  mod <- lm(DO ~ Temp_DO + Q + depth + watershed.inundation + SpC, data = df)

  # total R2 for this site
  R2_total <- summary(mod)$r.squared

  # fitted values
  yhat <- fitted(mod)

  preds <- c("Temp_DO", "Q", "depth", "watershed.inundation", "SpC")

  # "inclusive" proxy via structure coefficient: cor(yhat, x)^2 * R2_total
  inclusive <- map_dbl(preds, ~ {
    x <- df[[.x]]
    if (sd(x, na.rm = TRUE) == 0) return(0)
    cor(yhat, x, use = "pairwise.complete.obs")^2 * R2_total
  })

  # unique contribution: drop-term ΔR2
  unique <- map_dbl(preds, ~ {
    mod_drop <- update(mod, paste0(". ~ . - ", .x))
    max(R2_total - summary(mod_drop)$r.squared, 0)
  })

  tibble(
    variable = preds,
    unique_R2 = unique,
    inclusive_R2 = inclusive,
    shared_R2 = pmax(inclusive_R2 - unique_R2, 0),
    R2_total = R2_total
  )
}


site_R2_df <- nested_df %>%
  mutate(R2 = map(data, get_site_R2)) %>%
  unnest(R2)

plot_long <- site_R2_df %>%
  dplyr::select(ID, variable, unique_R2, shared_R2) %>%
  tidyr::pivot_longer(
    cols = c(unique_R2, shared_R2),
    names_to = "component",
    values_to = "rel_imp"
  ) %>%
  dplyr::mutate(component = dplyr::case_when(
    component == "unique_R2" ~ "Unique",
    component == "shared_R2" ~ "Shared (collinear)",
    TRUE ~ component
  ))


ggplot(plot_long, aes(x = variable, y = rel_imp, fill = component)) +
  geom_col(width = 0.85, color = "black") +
  facet_wrap(~ ID) +
  theme_bw() +
  labs(x = "Predictor", y = "Explained variance (R²)", fill = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  ggtitle("Predictive Power by Site",
          subtitle = "Unique vs shared (collinear) contributions")


#beta weights
get_site_betas <- function(df) {

  # must have enough data
  if (nrow(df) < 20) return(tibble())

  # variables to standardize
  preds <- c("Temp_DO", "Q", "depth", "watershed.inundation", "SpC")

  # standardize within site
  df_std <- df %>%
    mutate(
      DO = scale(DO)[,1],
      across(all_of(preds), ~ scale(.x)[,1])
    )

  # fit per-site model
  mod <- lm(
    DO ~ Temp_DO + Q + depth + watershed.inundation + SpC,
    data = df_std
  )

  # extract standardized betas
  tibble(
    variable = names(coef(mod))[-1],  # drop intercept
    beta = coef(mod)[-1]
  )
}
id_order <- c("15", "5", "5a", "3", "6", "13", "7", "9")

site_beta_df <- lm_df %>%
  group_by(ID) %>%
  nest() %>%
  mutate(betas = purrr::map(data, get_site_betas)) %>%
  unnest(betas)%>%
  mutate(ID = factor(ID, levels = id_order))


ggplot(site_beta_df%>%filter(variable %in% c('Temp_DO', "watershed.inundation")), aes(x = variable, y = beta, fill = beta > 0)) +
  geom_col(color = "black", width = 0.8) +
  facet_wrap(~ ID, scales='free') +
  scale_fill_manual(
    values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),
    labels = c("Negative", "Positive")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(
    x = "Predictor",
    y = "Standardized beta (β)",
    fill = "Effect direction",
    title = "Standardized beta weights by site",
    subtitle = "Predictors standardized within site"
  )



#figures#########
left_join(int.ext, DO)%>%left_join(temperature)%>%
  ggplot(aes(x=DO, y=avg.external))+
  geom_point(aes(colour = Temp_PT))+
  #lm.common+
  facet_wrap(~ID, scales='free')
test<-left_join(int.ext, DO)%>%left_join(temperature)
