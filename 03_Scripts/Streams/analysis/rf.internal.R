#entire dataset#####
source("03_Scripts/Streams/analysis/data for analysis.R")

master <- int.ext %>%
  select(Date, ID, avg.internal, Basin, day, NEP_corrected) %>%
  left_join(chem, by = c("Date", "ID")) %>%
  left_join(watershed_inundation, by = c("day", "Basin")) %>%
  left_join(wetland_cover, by = c('Basin')) %>%
  distinct(ID, Date, .keep_all = TRUE)%>%
  fill(avg.internal, Basin, avg.internal, K600, NEP_corrected, .direction = 'down')%>%
  drop_na()


# corr_mat <- cor(master%>%select(-Date, -ID, -AREA, -avg.internal, -avg.internal, -Basin, -day),
#                 use = "pairwise.complete.obs")
# corrplot(corr_mat,
#          method = "color",
#          type = "upper",
#          tl.col = "black",
#          tl.srt = 45,
#          addCoef.col = "black",
#          number.cex = 0.7)

#global############

rf.df<-master%>%
  select(
    avg.internal,
    Q,
    Temp_DO,
    watershed.inundation,
    depth,
    SpC, wetland.perc)%>%mutate(flow.state=case_when(
      Q>mean(Q, na.rm=T)~"highflow",
      Q<=mean(Q, na.rm=T)~"baseflow"
    )) %>%
  drop_na()

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
    avg.internal ~ .,
    data = train_fold,
    ntree = 1000,
    mtry = m,
    importance = FALSE
  )

  preds <- predict(rf_fit, test_fold)
  cv_results$RMSE[i] <- sqrt(mean((preds - test_fold$avg.internal)^2))
}

cv_summary <- cv_results %>%
  group_by(mtry) %>%
  summarise(mean_RMSE = mean(RMSE))

best_mtry <- cv_summary$mtry[which.min(cv_summary$mean_RMSE)]
best_mtry


rf_final <- randomForest(
  avg.internal ~ .,
  data = rf_train,
  ntree = 1000,
  mtry = best_mtry,
  importance = TRUE
)

print(rf_final)

pred <- predict(rf_final, rf_test)

plot(
  rf_test$avg.internal,
  pred,
  xlab = "Internal CO₂ Flux",
  ylab = "Internal CO₂ Flux",
  pch = 16
)
abline(0, 1, col = "red", lwd = 2)

rmse <- sqrt(mean((pred - rf_test$avg.internal)^2))
r2 <- cor(pred, rf_test$avg.internal)^2

rmse
r2

varImpPlot(rf_final)
importance(rf_final)

pred_df <- tibble(
  obs  = rf_test$avg.internal,
  pred = as.numeric(pred)
)

rmse <- sqrt(mean((pred_df$pred - pred_df$obs)^2))
r2   <- cor(pred_df$pred, pred_df$obs)^2

ggplot(pred_df, aes(x = obs, y = pred)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(intercept = 0, slope = 1, linewidth = 1) +
  coord_equal() +
  theme_bw() +
  labs(
    x = "Observed internal CO\u2082 flux",
    y = "Predicted internal CO\u2082 flux",
    title = "Random forest: predicted vs observed",
    subtitle = paste0("RMSE = ", round(rmse, 3), " | R\u00B2 = ", round(r2, 3))
  )

#by site##########

# 1) Daily aggregation
rf.df <- master %>%
  group_by(ID, day) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  select(
    ID,
    avg.internal,
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
          avg.internal ~ .,
          data = tr,
          ntree = 500,
          mtry = m
        )

        preds <- predict(rf_fit, te)
        rmse_fold[f] <- sqrt(mean((preds - te$avg.internal)^2, na.rm = TRUE))
      }

      rmse_by_mtry[j] <- mean(rmse_fold, na.rm = TRUE)
    }

    best_mtry <- mtry_vals[which.min(rmse_by_mtry)]

    # Final model
    rf_final <- randomForest(
      avg.internal ~ .,
      data = train,
      ntree = 500,
      mtry = best_mtry,
      importance = TRUE
    )

    pred <- as.numeric(predict(rf_final, test))

    rmse <- sqrt(mean((pred - test$avg.internal)^2, na.rm = TRUE))

    # guard against NA when variance is zero
    r2 <- suppressWarnings(cor(pred, test$avg.internal, use = "complete.obs")^2)
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


# --- NRMSE FIX: normalize by SD of the RESPONSE (avg.internal), by site ---
sd_by_site <- rf.df %>%
  group_by(ID) %>%
  summarise(sd_y = sd(avg.internal, na.rm = TRUE), .groups = "drop")

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


