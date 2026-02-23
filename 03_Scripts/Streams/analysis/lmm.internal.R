source("03_Scripts/Streams/analysis/data for analysis.R")

master <- int.ext %>%
  select(Date, ID, avg.internal, Basin, day, NEP_corrected) %>%
  left_join(chem, by = c("Date", "ID")) %>%
  left_join(watershed_inundation, by = c("day", "Basin")) %>%
  left_join(wetland_cover, by = c('Basin')) %>%
  distinct(ID, Date, .keep_all = TRUE)%>%
  fill(avg.internal, Basin, avg.internal, K600, NEP_corrected, .direction = 'down')%>%
  drop_na()


lm.df<-master%>%
  group_by(ID, day) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")%>%
  select(
    avg.internal,
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
  avg.internal ~ Q + Temp_DO + depth + watershed.inundation + SpC +
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
  mod <- lm(avg.internal ~ Temp_DO + Q + depth + watershed.inundation + SpC, data = df)

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
  facet_wrap(~ ID, scales='free') +
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
      avg.internal = scale(avg.internal)[,1],
      across(all_of(preds), ~ scale(.x)[,1])
    )

  # fit per-site model
  mod <- lm(avg.internal ~ Temp_DO + Q + depth + watershed.inundation + SpC, data = df_std)


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





