# Power simulation for the Mesoudi, Whiten & Dunbar (2006) Experiment 2 replication.
# Run once from the writeup folder with Rscript power/power_sim.R (about 15 minutes).
# Results are saved to power/results.rds and power/*.png, which the report loads.
# The report shows this code piece by piece, using the "## ---- label ----" markers.

## ---- setup ----
library(tidyverse)
set.seed(227) # the same random numbers on every run

# SciMinds colors: a blue ramp (darker = bigger effect) and the accent blue
sciminds_ramp <- c("0.5" = "#A6D0EA", "0.75" = "#58A6D6", "1" = "#2A7CBE")
sciminds_blue <- "#4868A8"
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

## ---- rates ----
# what each reader received from the person before them
mesoudi <- mesoudi |>
  arrange(chain, material, generation) |>
  group_by(chain, material) |>
  mutate(previous = lag(score)) |>
  ungroup()

rates <- mesoudi |>
  group_by(material) |>
  summarize(
    # how much of the original the first reader recalls
    start_rate = mean(score[generation == 1]) / 14,
    # how much of what they received later readers pass on
    keep_rate = sum(score[generation > 1]) / sum(previous[generation > 1]))

knitr::kable(rates, digits = 2)

## ---- chain-offset ----
# how much stronger or weaker each chain's first reader is than average
chain_offsets <- mesoudi |>
  filter(generation == 1) |>
  left_join(rates, by = "material") |>
  group_by(chain) |>
  summarize(offset = mean(score / 14 - start_rate))

knitr::kable(chain_offsets, digits = 2)

# how much chains differ from each other
chain_sd <- sd(chain_offsets$offset)

## ---- consistency-calibration ----
# how far each real reader landed from what the keep rate predicts
handoffs <- mesoudi |>
  filter(generation > 1) |>
  left_join(rates, by = "material") |>
  mutate(expected = previous * keep_rate,
         miss = score - expected)

# how much real scores vary around what the keep rate predicts
real_spread <- mean(handoffs$miss^2)

# the spread from chance alone, if every reader kept the same share
chance_spread <- mean(handoffs$previous * handoffs$keep_rate * (1 - handoffs$keep_rate))

# simulate a reader for each real handoff and measure the spread
simulated_spread <- function(consistency) {
  # each reader gets their own keep rate
  reader_keep <- rbeta(nrow(handoffs),
                       handoffs$keep_rate * consistency,
                       (1 - handoffs$keep_rate) * consistency)
  # each proposition they received survives or is lost
  simulated_score <- rbinom(nrow(handoffs), size = handoffs$previous, prob = reader_keep)
  mean((simulated_score - handoffs$expected)^2)
}

# keep the consistency whose spread matches the real one
calibration <- tibble(consistency = seq(0.5, 10, by = 0.25)) |>
  mutate(spread = map_dbl(consistency, \(k) mean(replicate(200, simulated_spread(k)))))

consistency <- calibration$consistency[which.min(abs(calibration$spread - real_spread))]

consistency_calibration_plot <- ggplot(calibration, aes(x = consistency, y = spread)) +
  geom_line(color = sciminds_blue) +
  geom_hline(yintercept = real_spread, linetype = "dashed") +     # real data
  geom_hline(yintercept = chance_spread, linetype = "dotted") +   # chance alone
  geom_vline(xintercept = consistency, color = "grey60") +
  labs(x = "consistency (higher = readers more alike)",
       y = "spread of simulated scores")
ggsave("power/consistency_calibration.png", consistency_calibration_plot, width = 6, height = 4)

## ---- generator ----
# shrink the gap between materials; 1 is Mesoudi's effect
scale_rates <- function(effect) {
  rates |>
    mutate(start_rate = mean(start_rate) + effect * (start_rate - mean(start_rate)),
           keep_rate = mean(keep_rate) + effect * (keep_rate - mean(keep_rate)))
}

# the four reading orders (1 = gossip, 2 = social, 3 = individual, 4 = physical)
paragraphs <- c("gossip", "social", "individual", "physical")
williams <- list(c(1, 2, 4, 3),  # gossip, social, physical, individual
                 c(2, 3, 1, 4),  # social, individual, gossip, physical
                 c(3, 4, 2, 1),  # individual, physical, social, gossip
                 c(4, 1, 3, 2))  # physical, gossip, individual, social
latin_square <- map(1:4, \(o) tibble(order = o,
                                     material = paragraphs[williams[[o]]],
                                     position = 1:4)) |>
  bind_rows()

make_study <- function(n_chains, effect = 1, scorer_error = 0) {
  # each chain's four paragraphs, with their rates and the chain's offset
  study <- expand_grid(chain = 1:n_chains, material = rates$material) |>
    left_join(scale_rates(effect), by = "material") |>
    group_by(chain) |>
    mutate(offset = rnorm(1, mean = 0, sd = chain_sd)) |>
    ungroup()

  generations <- list()
  for (g in 1:4) {
    if (g == 1) {
      # the first reader recalls from all 14 original propositions
      prob <- pmin(pmax(study$start_rate + study$offset, 0), 1)
      true_score <- rbinom(nrow(study), size = 14, prob = prob)
    } else {
      # later readers keep a share of what they received
      reader_keep <- rbeta(nrow(study), study$keep_rate * consistency,
                           (1 - study$keep_rate) * consistency)
      true_score <- rbinom(nrow(study), size = previous, prob = reader_keep)
    }
    # the scorer misjudges some propositions in either direction
    score <- rbinom(nrow(study), size = true_score, prob = 1 - scorer_error) +
      rbinom(nrow(study), size = 14 - true_score, prob = scorer_error)
    generations[[g]] <- study |> mutate(generation = g, true_score = true_score, score = score)
    previous <- true_score # the next reader gets what was actually written
  }

  bind_rows(generations) |>
    mutate(order = (chain - 1) %% 4 + 1) |> # each chain gets the next order
    left_join(latin_square, by = c("order", "material")) |>
    select(chain, generation, material, position, true_score, score)
}

make_study(n_chains = 10) |> head(8) |> knitr::kable(digits = 2)

## ---- simulated-vs-real ----
simulated <- map(1:500, \(i) make_study(n_chains = 10)) |> bind_rows()

# simulated means next to Mesoudi's real means
simulated_means <- simulated |>
  group_by(material, generation) |>
  summarize(score = mean(score), .groups = "drop")

# Mesoudi's means, with error bars of 1 SE across his 10 chains
real_means <- mesoudi |>
  group_by(material, generation) |>
  summarize(se = sd(score) / sqrt(n()), score = mean(score), .groups = "drop")

simulated_vs_real_plot <- ggplot(mapping = aes(x = generation, y = score, color = material)) +
  geom_line(data = simulated_means) +
  geom_errorbar(data = real_means, aes(ymin = score - se, ymax = score + se), width = 0.1) +
  geom_point(data = real_means, size = 2) +
  labs(y = "mean correct recall (out of 14)")

# the social minus non-social gap at each effect setting
social <- c("gossip", "social")
effect_dial <- tibble(effect = c(1, 0.75, 0.5, 0)) |>
  mutate(difference = map_dbl(effect, \(e) {
    d <- map(1:500, \(i) make_study(n_chains = 10, effect = e)) |> bind_rows()
    mean(d$score[d$material %in% social]) - mean(d$score[!d$material %in% social])
  }))
ggsave("power/simulated_vs_real_means.png", simulated_vs_real_plot, width = 6, height = 4)

## ---- scorer-error ----
# one large simulated dataset to score
big <- make_study(n_chains = 1000)

# how well a scorer with this error rate agrees with the truth
scorer_r <- function(error) {
  scored <- rbinom(nrow(big), size = big$true_score, prob = 1 - error) +
    rbinom(nrow(big), size = 14 - big$true_score, prob = error)
  cor(big$true_score, scored)
}

# keep the error rate that gives r = 0.90
scorer_fit <- tibble(error = seq(0, 0.2, by = 0.0025)) |>
  mutate(r = map_dbl(error, scorer_r))
scorer_error <- scorer_fit$error[which.min(abs(scorer_fit$r - 0.90))]

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

## ---- power-sims ----
# one simulated study: a p-value for each of the five tests
run_one_study <- function(n_chains, effect, scorer_error) {
  d <- chain_difference(make_study(n_chains, effect, scorer_error))
  tibble(planned = sign_flip_p(d$difference),
         gossip_vs_individual = sign_flip_p(d$gossip_vs_individual),
         gossip_vs_physical = sign_flip_p(d$gossip_vs_physical),
         social_vs_individual = sign_flip_p(d$social_vs_individual),
         social_vs_physical = sign_flip_p(d$social_vs_physical))
}

# power: the share of 1,000 studies that come out significant
# (0.05 for the planned test, 0.01 for the four pairs)
estimate_power <- function(n_chains, effect, scorer_error, n_sims = 1000) {
  map(1:n_sims, \(i) run_one_study(n_chains, effect, scorer_error)) |>
    bind_rows() |>
    summarize(planned = mean(planned < 0.05),
              gossip_vs_individual = mean(gossip_vs_individual < 0.01),
              gossip_vs_physical = mean(gossip_vs_physical < 0.01),
              social_vs_individual = mean(social_vs_individual < 0.01),
              social_vs_physical = mean(social_vs_physical < 0.01))
}

# every chain count and effect size for one scorer; map2 (not map2_dbl)
# because each cell gives five powers, one per test
power_for_scorer <- function(scorer_error) {
  expand_grid(n_chains = seq(4, 48, by = 4),
              effect = c(1, 0.75, 0.5)) |>
    mutate(result = map2(n_chains, effect, \(n, e) estimate_power(n, e, scorer_error)))
}

# both scorers (about 15 minutes), then one row per test
power <- bind_rows(power_for_scorer(0) |> mutate(scorer = "perfect scoring"),
                   power_for_scorer(scorer_error) |> mutate(scorer = "scorer at r = 0.90")) |>
  unnest(result) |>
  pivot_longer(planned:social_vs_physical, names_to = "test", values_to = "power") |>
  mutate(test = factor(test, levels = c("planned", "gossip_vs_individual",
                                        "gossip_vs_physical", "social_vs_individual",
                                        "social_vs_physical")))

## ---- power-curve ----
# readable panel labels
test_labels <- c(planned = "social vs\nnon-social",
                 gossip_vs_individual = "gossip vs\nindividual",
                 gossip_vs_physical = "gossip vs\nphysical",
                 social_vs_individual = "social vs\nindividual",
                 social_vs_physical = "social vs\nphysical",
                 "perfect scoring" = "perfect\nscoring",
                 "scorer at r = 0.90" = "scorer at\nr = 0.90")

power_plot <- ggplot(power, aes(x = n_chains, y = power, color = factor(effect))) +
  geom_hline(yintercept = c(0.8, 0.9, 0.95), linetype = "dashed", color = "grey70") +
  geom_line(linewidth = 0.8) +
  facet_grid(scorer ~ test, labeller = as_labeller(test_labels)) +
  scale_x_continuous(breaks = c(12, 24, 36, 48)) +
  scale_color_manual(values = sciminds_ramp,
                     labels = c("0.5" = "50%", "0.75" = "75%", "1" = "100%")) +
  labs(x = "number of chains", y = "power",
       color = "true effect\n(share of\nMesoudi's)")
ggsave("power/power_curve.png", power_plot, width = 9, height = 5)

## ---- chains-needed ----
# the fewest chains that reach each power target
chains_needed <- function(target) {
  power |>
    group_by(scorer, test, effect) |>
    summarize(chains = if (any(power >= target))
                         as.character(min(n_chains[power >= target]))
                       else "> 48",
              .groups = "drop") |>
    mutate(target = paste0(target * 100, "% power"))
}

chains_table <- map(c(0.8, 0.9, 0.95), chains_needed) |>
  bind_rows() |>
  pivot_wider(names_from = target, values_from = chains) |>
  arrange(scorer, test, desc(effect))
chains_table

## ---- save-results ----
# save what the report needs
saveRDS(list(consistency = consistency,
             scorer_error = scorer_error,
             effect_dial = effect_dial,
             power = power,
             chains_table = chains_table),
        "power/results.rds")
