# Power simulation for the Mesoudi, Whiten & Dunbar (2006) Experiment 2 replication.
# The report runs this code live, piece by piece, using the "## ---- label ----" markers.
# It also runs on its own with Rscript power_sim.R (about a minute).

## ---- setup ----
library(tidyverse)
set.seed(240) # the same random numbers on every render

# SciMinds colors: a blue ramp, darker = bigger effect
sciminds_ramp <- c("0.5" = "#A6D0EA", "0.75" = "#58A6D6", "1" = "#2A7CBE")
# monospace figure text, and a lighter panel so the palest blue stays visible
theme_set(theme_gray(base_family = "mono") +
            theme(panel.background = element_rect(fill = "grey97")))

# Mesoudi's Experiment 2 data, pinned to one version so it never changes
mesoudi_url <- paste0(
  "https://raw.githubusercontent.com/",
  "amesoudi/mesoudi_whiten_dunbar_2006/",
  "a27a5c3024d55c47d17b30e6cdea7dd28c24e9fc/",
  "mesoudi2006_exp2_data.csv")
mesoudi <- read_csv(mesoudi_url, show_col_types = FALSE) |>
  rename(score = correct_recall) # propositions recalled correctly, out of 14

## ---- chain-difference ----
# each chain's social minus non-social difference, plus the four pairs
chain_difference <- function(data) {
  data |>
    group_by(chain, material) |>
    summarize(score = mean(score), .groups = "drop") |>
    pivot_wider(names_from = material, values_from = score) |>
    mutate(difference = (gossip + social) / 2 - (individual + physical) / 2,
           gossip_vs_individual = gossip - individual,
           gossip_vs_physical = gossip - physical,
           social_vs_individual = social - individual,
           social_vs_physical = social - physical)
}

real_differences <- chain_difference(mesoudi)$difference

## ---- sign-flip ----
sign_flip_p <- function(differences, n_flips = 5000) {
  n <- length(differences)
  # each row is one round of coin flips, a random + or - for every chain
  signs <- matrix(sample(c(-1, 1), n_flips * n, replace = TRUE), nrow = n_flips)
  # multiplying gives every round's mean difference at once, which is much
  # faster than a loop that flips one round at a time
  flipped_means <- (signs %*% differences) / n
  # how often a flipped mean is as far from zero as the real one; the tiny
  # 1e-9 allowance keeps rounding error from missing exact ties
  mean(abs(flipped_means) >= abs(mean(differences)) - 1e-9)
}

real_p <- sign_flip_p(real_differences)

## ---- bootstrap-effect ----
boot_means <- map_dbl(1:10000, \(i) mean(sample(real_differences, replace = TRUE)))

boot_ci <- quantile(boot_means, c(0.025, 0.975)) # the range of plausible effects
boot_low_share <- boot_ci[1] / mean(real_differences) # the low end as a share of his effect

## ---- mesoudi-effects ----
# readable test names, in the order the tables and figure show them
test_labels <- c(difference = "social vs non-social",
                 gossip_vs_individual = "gossip vs individual",
                 gossip_vs_physical = "gossip vs physical",
                 social_vs_individual = "social vs individual",
                 social_vs_physical = "social vs physical")

# Mesoudi's effect for each test: the mean and SD of his 10 chain
# differences, and the cutoff it is tested at (0.01 for the pairs, his
# Bonferroni level)
mesoudi_effects <- chain_difference(mesoudi) |>
  pivot_longer(difference:social_vs_physical,
               names_to = "test", values_to = "chain_difference") |>
  mutate(test = factor(test, levels = names(test_labels),
                       labels = test_labels)) |>
  group_by(test) |>
  summarize(mean = mean(chain_difference), sd = sd(chain_difference)) |>
  mutate(alpha = if_else(test == "social vs non-social", 0.05, 0.01))

knitr::kable(mesoudi_effects, digits = 2,
             col.names = c("test", "mean difference", "SD", "cutoff"))

## ---- one-experiment ----
# scorer noise, as a share of the true SD, that makes measured and true
# differences correlate at r = 0.90
scorer_noise <- sqrt(1 / 0.90^2 - 1)

# one simulated study with n_chains chains: a p-value for each test. Each
# test gets its own simulated chains, which doesn't change any one test's
# power
one_experiment <- function(n_chains, effect) {
  map_dbl(1:nrow(mesoudi_effects), \(i) {
    this_mean <- effect * mesoudi_effects$mean[i]
    this_sd <- mesoudi_effects$sd[i]
    true <- rnorm(n_chains, mean = this_mean, sd = this_sd)
    measured <- true + rnorm(n_chains, mean = 0, sd = this_sd * scorer_noise)
    # 1,000 flips keeps the render to about a minute
    sign_flip_p(measured, n_flips = 1000)
  })
}

# power: the share of simulated studies that come out significant, for
# each test
estimate_power <- function(n_chains, effect, n_sims = 1000) {
  # one row per test, one column per simulated study, so each row is
  # compared with its own test's cutoff
  p_values <- replicate(n_sims, one_experiment(n_chains, effect))
  tibble(test = mesoudi_effects$test,
         power = rowMeans(p_values < mesoudi_effects$alpha))
}

## ---- no-effect ----
# with no effect, each test should come out significant at about its cutoff
estimate_power(n_chains = 12, effect = 0, n_sims = 2000) |>
  mutate(cutoff = mesoudi_effects$alpha) |>
  knitr::kable(digits = 3,
               col.names = c("test", "share significant", "cutoff"))

## ---- power-sims ----
# 4 to 48 chains in steps of 4, so the reading orders balance
power <- expand_grid(n_chains = seq(4, 48, by = 4),
                     effect = c(1, 0.75, 0.5)) |>
  mutate(result = map2(n_chains, effect, \(n, e) estimate_power(n, e))) |>
  unnest(result)

## ---- power-curve ----
ggplot(power, aes(x = n_chains, y = power, color = factor(effect))) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey70") +
  geom_line(linewidth = 0.8) +
  facet_wrap(~test, nrow = 1, labeller = label_wrap_gen(width = 10)) +
  scale_x_continuous(breaks = c(12, 24, 36, 48)) +
  scale_color_manual(values = sciminds_ramp,
                     labels = c("50%", "75%", "100%")) +
  labs(x = "number of chains", y = "power",
       color = "true effect\n(share of\nMesoudi's)")

## ---- chains-needed ----
# the fewest chains that reach 80% power, and the power at 12 chains
chains_table <- power |>
  group_by(test, effect) |>
  summarize(chains_for_80 = if (any(power >= 0.8))
                              as.character(min(n_chains[power >= 0.8]))
                            else "> 48",
            power_at_12 = power[n_chains == 12],
            .groups = "drop") |>
  arrange(test, desc(effect))

knitr::kable(chains_table, digits = 2,
             col.names = c("test", "share of Mesoudi's effect",
                           "chains for 80% power", "power at 12 chains"))

# planned test at 12 chains and 75% of the effect
power_at_12 <- power |>
  filter(test == "social vs non-social", effect == 0.75, n_chains == 12) |>
  pull(power)

# and at 50% of the effect
power_at_12_half <- power |>
  filter(test == "social vs non-social", effect == 0.5, n_chains == 12) |>
  pull(power)
