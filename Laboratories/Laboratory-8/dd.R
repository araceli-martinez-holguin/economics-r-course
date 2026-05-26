###############################################################################
#                                                                             #
#   CASE STUDY: Microfinance Impact Evaluation in Bangladesh                  #
#   Based on Pitt & Khandker (1998) and Khandker (2006)                       #
#                                                                             #
#   Research Question:                                                        #
#   Does participation in microfinance programs increase per capita           #
#   household expenditure? Does the gender of the participant matter?         #
#                                                                             #
#   Data: Khandker Bangladesh Panel, 1991–1998 (hh_9198.dta)                  #
#   ~1,800 households across 29 thanas (districts)                            #
#                                                                             #
#   Identification Strategy:                                                  #
#   - ELIGIBILITY RULE: Households owning < 0.5 acres of land are             #
#     eligible to participate in microfinance programs.                       #
#   - GENDER CONSTRAINT: Men participate only in men's groups;                #
#     women only in women's groups.                                           #
#   - PANEL STRUCTURE: Two survey rounds (1991–92 baseline, 1998–99           #
#     follow-up) allow Difference-in-Differences estimation.                  #
#   - VILLAGE FIXED EFFECTS: Control for selection of program villages.       #
#                                                                             #
#   Exercises:                                                                #
#   1. Difference-in-Differences (DiD) — cross-section + panel                #
#   2. Propensity Score Matching + DiD (PSM-DiD)                              #
#                                                                             #
###############################################################################

# ── Required packages ─────────────────────────────────────────────────────────
# Install if needed:
# install.packages(c("tidyverse", "haven", "fixest", "MatchIt",
#                    "WeightIt", "car", "cobalt", "modelsummary"))

library(tidyverse)       # Data wrangling and ggplot2 visualizations
library(haven)           # Read Stata .dta files (replaces foreign::read.dta)
library(fixest)          # Fast OLS + panel FE with clustered SEs
library(car)             # Variance Inflation Factor (VIF) for collinearity
library(MatchIt)         # Propensity Score Matching
library(WeightIt)        # IPW weighting (optional complement to MatchIt)
library(cobalt)          # Covariate balance diagnostics after matching
library(modelsummary)    # Publication-quality regression tables

# Set your working directory to wherever hh_9198.dta lives
setwd("~/Documents/Cursos-R/Laboratorio-8")

###############################################################################
# STEP 1 — LOAD AND INSPECT THE DATA
###############################################################################
#
# The dataset contains two observations per household (nh):
#   year == 0  →  baseline survey  (1991–92)
#   year == 1  →  follow-up survey (1998–99)
#
# Key variables:
#   exptot  — total household expenditure (outcome)
#   dfmfd   — female microfinance participation (1 = yes, in that survey year)
#   dmmfd   — male microfinance participation
#   hhland  — land owned (in decimals; 1 acre = 100 decimals)
#   sexhead, agehead, educhead — household head characteristics
#   vaccess, pcirr, rice, wheat, milk, oil — village/household controls
#
# WHY A PANEL?
# A single cross-section (Pitt & Khandker 1998 used 1991–92 only) cannot
# separate pre-existing differences from program effects. The 1998–99 panel
# allows us to difference out time-invariant household characteristics,
# removing a key source of selection bias.
# ─────────────────────────────────────────────────────────────────────────────

hh <- read_dta("hh_9198.dta")

# Quick look at the structure
glimpse(hh)
table(hh$year)   # Should show two rounds


###############################################################################
# STEP 2 — FEATURE ENGINEERING
###############################################################################
#
# We build three key variables before estimation:
#
# (a) lexptot  — log(1 + exptot)
#     The outcome is expenditure, which is right-skewed. Taking logs
#     reduces skewness, stabilises variance, and makes the DiD coefficient
#     interpretable as an approximate percentage change.
#     We add 1 before logging to avoid log(0) for households with exptot = 0.
#
# (b) lnland   — log(1 + hhland/100)
#     Land owned (in acres) is used as a CONTROL and as the eligibility
#     instrument (< 0.5 acres → eligible). Taking logs handles its
#     skewed distribution.
#
# (c) dfmfd98  — EVER-TREATED indicator (time-invariant)
#     dfmfd records whether a household's female member participated
#     IN THAT YEAR. For DiD we need a stable group indicator.
#     A household is "treated" if it had female participation in 1998–99.
#     We compute this by: flagging participation in year == 1, then
#     taking the max (i.e., OR) across both years within each household.
#     This is the between-group variable in the DiD design.
#
# (d) dfmfdyr  = dfmfd98 × year  — THE DiD INTERACTION TERM
#     This is the key regressor. It equals 1 only for treated households
#     in the follow-up year. Its coefficient is the ATT estimate.
# ─────────────────────────────────────────────────────────────────────────────

hh <- hh %>%
  mutate(
    lexptot = log(1 + exptot),           # log outcome (see (a) above)
    lnland  = log(1 + hhland / 100)      # log land in acres (see (b) above)
  ) %>%
  
  # --- Female participation flag in 1998 only ---
  mutate(dfmfd1 = if_else(dfmfd == 1 & year == 1, 1L, 0L)) %>%
  
  # --- Ever-treated: did ANYONE in the household participate in 1998? ---
  # group_by(nh) ensures max() is computed within each household panel
  group_by(nh) %>%
  mutate(dfmfd98 = max(dfmfd1)) %>%      # 1 if female member participated in 1998
  ungroup() %>%
  
  # --- DiD interaction term ---
  mutate(dfmfdyr = dfmfd98 * year) %>%   # = 1 only for treated hh in 1998
  
  # --- Repeat for male participants (for sensitivity / Exercise 2) ---
  mutate(dmmfd1  = if_else(dmmfd == 1 & year == 1, 1L, 0L)) %>%
  group_by(nh) %>%
  mutate(dmmfd98 = max(dmmfd1)) %>%
  ungroup() %>%
  mutate(dmmfdyr = dmmfd98 * year)

# Verify group sizes (should match the ~60% participation rate in targeted villages)
hh %>%
  filter(year == 0) %>%                  # baseline only, one obs per household
  count(dfmfd98) %>%
  mutate(pct = n / sum(n) * 100)


###############################################################################
# EXERCISE 1: DIFFERENCE-IN-DIFFERENCES (DiD)
###############################################################################
#
# THE DiD LOGIC (in terms of this study):
#
#   We compare how expenditure changed from 1991 to 1998 for:
#     - TREATED group: households whose female members joined microfinance
#     - CONTROL group: households whose female members did NOT join
#
#   The DiD estimator is:
#     δ = (Ȳ_treated,1998 - Ȳ_treated,1991) - (Ȳ_control,1998 - Ȳ_control,1991)
#
#   This difference removes:
#     ✓ Time-invariant household characteristics (e.g., ability, social capital)
#     ✓ Aggregate shocks common to all households (e.g., macroeconomic trends)
#
#   KEY ASSUMPTION (Parallel Trends):
#     In the absence of microfinance, treated and control households would
#     have had the same trend in expenditure from 1991 to 1998.
#     We check this visually with the plot below (we only have 2 periods,
#     so a formal pre-trend test is not feasible here).
#
# ─────────────────────────────────────────────────────────────────────────────

# ── 1A. Basic DiD (OLS, no fixed effects) ─────────────────────────────────────
#
#   Model: lexptot = α + β₁·year + β₂·dfmfd98 + β₃·(dfmfd98×year) + ε
#
#   Where:
#     β₁  (year)     = common time trend (what happened to EVERYONE from 1991→1998)
#     β₂  (dfmfd98)  = pre-existing level difference between groups (selection)
#     β₃  (dfmfdyr)  = DiD estimator = causal effect of female microfinance
#
#   This replicates the cross-section spirit of Pitt & Khandker (1998) but
#   uses the panel to difference out time-invariant factors.

did_basic <- lm(lexptot ~ year + dfmfd98 + dfmfdyr, data = hh)
summary(did_basic)

# ── 1B. DiD with Household Fixed Effects (Panel DiD) ─────────────────────────
#
#   Model: lexptot_it = α_i + β₁·year_t + β₃·(dfmfd98_i × year_t) + ε_it
#
#   The household fixed effect α_i (| nh) absorbs ALL time-invariant
#   household characteristics — including dfmfd98 itself (which is why it
#   disappears from the output). This directly maps to Khandker (2006),
#   who used the panel to eliminate baseline selection differences.
#
#   feols() from {fixest}:
#     - Much faster than plm() for large panels
#     - Cluster-robust SEs at household level (cluster = ~nh) account for
#       serial correlation within households across the two periods.
#
#   NOTE: "year" is still identified because it varies within households
#   (it changes from 0 to 1 for every household).

did_fe <- feols(
  lexptot ~ year + dfmfdyr | nh,   # | nh = absorb household FE
  data    = hh,
  cluster = ~nh                    # cluster SEs at the household level
)
summary(did_fe)

# ── 1C. Parallel Trends Visualisation ─────────────────────────────────────────
#
#   This plot is the standard visual check for the parallel trends assumption.
#   If the two lines had parallel slopes BEFORE the program, we have more
#   confidence the assumption holds. With only two periods we cannot test
#   this formally, but the graph is still required for the exposition.
#
#   The COUNTERFACTUAL line (dashed) shows what the treated group WOULD
#   have experienced without microfinance, under the parallel trends
#   assumption (treated baseline + control group's change from 1991→1998).

# --- Compute group means ---
means <- hh %>%
  group_by(year, dfmfd98) %>%
  summarise(mean_y = mean(lexptot, na.rm = TRUE), .groups = "drop")

# --- Extract the four key points (labelled as in Figure 5.1 of the WB handbook) ---
y0 <- means %>% filter(year == 0, dfmfd98 == 1) %>% pull(mean_y)  # treated, 1991
y1 <- means %>% filter(year == 0, dfmfd98 == 0) %>% pull(mean_y)  # control, 1991
y2 <- means %>% filter(year == 1, dfmfd98 == 0) %>% pull(mean_y)  # control, 1998
y4 <- means %>% filter(year == 1, dfmfd98 == 1) %>% pull(mean_y)  # treated, 1998

# --- Counterfactual: treated baseline + control's change ---
# This is the heart of DiD: if treated households had followed the same
# trend as control households, where would they have ended up?
y3 <- y0 + (y2 - y1)

# DiD impact = actual treated outcome MINUS counterfactual
impact <- y4 - y3
cat("DiD impact (unadjusted means):", round(impact, 4), "\n")

# --- Data frames for plotting ---
lines_df <- tribble(
  ~x, ~xend, ~y,  ~yend, ~group,
  0,  1,     y0,  y4,    "Participants",
  0,  1,     y1,  y2,    "Control",
  0,  1,     y0,  y3,    "Counterfactual"
)

points_df <- tribble(
  ~time, ~income, ~group,
  0,     y0,      "Participants",
  0,     y1,      "Control",
  1,     y4,      "Participants",
  1,     y2,      "Control",
  1,     y3,      "Counterfactual"
)

# --- Build the plot ---
ggplot() +
  geom_segment(
    data = lines_df,
    aes(x = x, xend = xend, y = y, yend = yend,
        linetype = group, linewidth = group)
  ) +
  geom_point(
    data = points_df,
    aes(x = time, y = income, shape = group, fill = group),
    size = 4, stroke = 1.2
  ) +
  # Bracket showing the DiD impact
  annotate("segment",
           x = 1.06, xend = 1.06, y = y3, yend = y4,
           arrow = arrow(ends = "both", type = "open", length = unit(0.15, "cm")),
           linewidth = 0.6) +
  annotate("text",
           x = 1.08, y = (y3 + y4) / 2,
           label = paste0("Impact\n", round(impact, 3)),
           hjust = 0, size = 3.2) +
  # Y-axis labels matching the WB handbook notation
  annotate("text", x = -0.02, y = y4, label = "Y[4]", parse = TRUE, hjust = 1, size = 3.5) +
  annotate("text", x = -0.02, y = y3, label = "Y[3]", parse = TRUE, hjust = 1, size = 3.5) +
  annotate("text", x = -0.02, y = y2, label = "Y[2]", parse = TRUE, hjust = 1, size = 3.5) +
  annotate("text", x = -0.02, y = y1, label = "Y[1]", parse = TRUE, hjust = 1, size = 3.5) +
  annotate("text", x = -0.02, y = y0, label = "Y[0]", parse = TRUE, hjust = 1, size = 3.5) +
  # Arrow marking the program period
  annotate("segment",
           x = 0.25, xend = 0.72, y = min(y0, y1) - 0.15, yend = min(y0, y1) - 0.15,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"), linewidth = 1) +
  annotate("text", x = 0.48, y = min(y0, y1) - 0.22,
           label = "Program", size = 3.5) +
  scale_x_continuous(
    breaks = c(0, 1), labels = c("1991", "1998"),
    limits = c(-0.15, 1.25), expand = c(0, 0)
  ) +
  scale_y_continuous(breaks = NULL) +
  scale_shape_manual(values = c("Participants" = 21, "Control" = 21, "Counterfactual" = 21)) +
  scale_fill_manual(values = c("Participants" = "gray50", "Control" = "black", "Counterfactual" = "white")) +
  scale_linetype_manual(values = c("Participants" = "solid", "Control" = "solid", "Counterfactual" = "dashed")) +
  scale_linewidth_manual(values = c("Participants" = 0.9, "Control" = 0.9, "Counterfactual" = 0.7)) +
  labs(
    x = "Time", y = "Mean log(expenditure + 1)",
    title = "Figure 5.1  Difference-in-Differences (DiD)",
    subtitle = "Female microfinance participants vs. control households, Bangladesh 1991–1998",
    caption = paste0(
      "Dashed line = counterfactual (parallel trends assumption).\n",
      "Impact = Y₄ − Y₃ = ", round(impact, 3), ".\n",
      "Y₃ = Y₀ + (Y₂ − Y₁): treated baseline + control group's change 1991→1998."
    )
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.ticks.y    = element_blank(),
    axis.line       = element_line(linewidth = 0.5),
    plot.title      = element_text(size = 12, face = "bold"),
    plot.subtitle   = element_text(size = 10, color = "gray40"),
    plot.caption    = element_text(size = 9,  color = "gray50"),
    axis.title      = element_text(size = 11)
  )

# ── 1D. Multicollinearity Diagnostics ─────────────────────────────────────────
#
#   Multicollinearity inflates standard errors and makes individual
#   coefficient estimates unreliable. We check two metrics:
#
#   VIF (Variance Inflation Factor):
#     VIF = 1/(1 − R²_j), where R²_j is from regressing predictor j on all others.
#     Rule of thumb: GVIF^(1/(2*Df)) < 2 is acceptable.
#
#   Condition number (kappa):
#     Ratio of the largest to smallest eigenvalue of X'X.
#     kappa < 30 is generally acceptable.
#
#   NOTE: We include factor(nh) to mirror the FE model, but in practice
#   you would run VIF only on the structural regressors.

did_check <- lm(lexptot ~ year + dfmfdyr + factor(nh), data = hh)
cat("\n--- VIF for DiD model ---\n")
vif(did_check) %>% head(3)   # show only structural regressors

cat("\n--- Condition number ---\n")
kappa(model.matrix(~ year + dfmfdyr + factor(nh), data = hh))


###############################################################################
# EXERCISE 2: PROPENSITY SCORE MATCHING + DiD (PSM-DiD)
###############################################################################
#
# WHY COMBINE PSM WITH DiD?
#
# The basic DiD controls for time-invariant selection into microfinance
# (via household FEs). But if the TREND in outcomes differed between
# treated and control households BEFORE 1998 (a violation of parallel
# trends), DiD alone is biased.
#
# PSM-DiD addresses this by first constructing a comparison group that
# is similar to the treated group on OBSERVABLE pre-treatment characteristics,
# and then applying DiD on this matched sample.
#
# This is relevant here because Pitt & Khandker (1998) relied on the
# landholding eligibility rule (< 0.5 acres) as their identification
# strategy. However, Khandker (2006) noted that this rule was not always
# enforced, meaning some ineligible households participated. PSM can help
# narrow the comparison to households with similar propensities to
# participate, regardless of the eligibility rule.
#
# STEP 2A: Estimate the propensity score (probability of treatment)
# STEP 2B: Match treated to control households on the propensity score
# STEP 2C: Check balance after matching
# STEP 2D: Re-estimate DiD on the matched sample
#
# ─────────────────────────────────────────────────────────────────────────────

# ── 2A. Subset to baseline year only ─────────────────────────────────────────
#
#   PSM uses PRE-TREATMENT characteristics to predict treatment status.
#   We must use baseline (1991) data only — using post-treatment data would
#   contaminate the propensity score with the effect of the program itself.

hh_base <- hh %>% filter(year == 0)

# ── 2B. Propensity Score Matching with MatchIt ────────────────────────────────
#
#   We model: P(dfmfd98 = 1 | X) using a probit model (matching the
#   specification of Pitt & Khandker 1998).
#
#   Covariates (X) are household-level PRE-TREATMENT characteristics:
#     sexhead  — gender of household head (female-headed households may differ)
#     agehead  — age of household head
#     educhead — education of household head (human capital proxy)
#     lnland   — log land owned (THE eligibility instrument: < 0.5 acre rule)
#     vaccess  — access to paved road (market integration)
#     pcirr    — share of irrigated land in the village (productivity context)
#     rice, wheat, milk, oil — village-level price indices (cost of living)
#
#   WHY INCLUDE lnland?
#   Land ownership is the main eligibility criterion. Matching on it ensures
#   that treated and control households face a similar probability of being
#   eligible — this is the Pitt & Khandker identification strategy made explicit.
#
#   method = "nearest"  → 1-to-1 nearest-neighbour matching
#   caliper = 0.01      → only match pairs within 0.01 standard deviations
#                          of the propensity score (prevents poor matches)
#   ratio   = 1         → one control matched per treated unit

m_out <- matchit(
  dfmfd98 ~ sexhead + agehead + educhead + lnland +
    vaccess + pcirr + rice + wheat + milk + oil,
  data     = hh_base,
  method   = "nearest",
  distance = "probit",    # probit link (Pitt & Khandker 1998 specification)
  caliper  = 0.01,
  ratio    = 1
)

# Summary of matching: how many matched? How many dropped?
summary(m_out, un = FALSE)

# ── 2C. Balance diagnostics ───────────────────────────────────────────────────
#
#   After matching, we check whether the COVARIATE DISTRIBUTION is now
#   similar between treated and control households.
#
#   Standardised Mean Difference (SMD):
#     SMD = (mean_treated - mean_control) / SD_pooled_baseline
#     SMD < 0.1 is the conventional threshold for "good" balance.
#
#   If balance is poor, matching failed to create comparable groups
#   and the PSM-DiD estimate remains biased on observables.

cat("\n--- Balance table after PSM ---\n")
bal.tab(m_out, thresholds = c(m = 0.1))   # flag covariates with SMD > 0.1

# Love plot: visual summary of SMD before and after matching
love.plot(m_out, threshold = 0.1,
          title = "Covariate Balance Before and After PSM\n(Female Microfinance Participants, Bangladesh)")

# Propensity score overlap: do treated and matched controls share support?
# If they don't overlap, we are extrapolating outside the common support.
plot(m_out, type = "density", interactive = FALSE,
     main = "Propensity Score Distribution Before and After Matching")

# ── 2D. Build matched panel dataset ───────────────────────────────────────────
#
#   match.data() returns the matched baseline observations with two new columns:
#     distance — the estimated propensity score for each unit
#     weights  — the matching weights (1 for treated; may vary for controls)
#
#   We then RECOVER BOTH YEARS (1991 and 1998) for the matched households.
#   This is critical: we matched on baseline characteristics, but DiD
#   requires observations in both time periods.

matched_base <- match.data(m_out)          # matched baseline observations

# Household IDs that were successfully matched
matched_nh <- unique(matched_base$nh)
cat("\nNumber of matched households:", length(matched_nh), "\n")

# Merge propensity scores and weights back to the full panel
psm_hh <- hh %>%
  filter(nh %in% matched_nh) %>%
  left_join(
    matched_base %>% select(nh, distance, weights),
    by = "nh"
  )

# ── 2E. DiD on matched sample (unweighted) ────────────────────────────────────
#
#   Same model as Exercise 1A, but estimated only on the matched sample.
#   The matched control group is now more comparable to treated households
#   on observable pre-treatment characteristics, so the parallel trends
#   assumption is more credible.

did_psm <- lm(lexptot ~ year + dfmfd98 + dfmfdyr, data = psm_hh)
summary(did_psm)

# ── 2F. DiD on matched sample with analytical (IPW) weights ──────────────────
#
#   Beyond nearest-neighbour matching, we can REWEIGHT control observations
#   by their odds of treatment: ps / (1 - ps).
#   This gives higher weight to controls that look most like the treated.
#   Treated units keep weight = 1.
#
#   This is equivalent to inverse probability weighting (IPW) restricted
#   to the matched sample — it combines matching and weighting to
#   achieve better balance and reduce residual imbalance.

psm_hh <- psm_hh %>%
  mutate(
    a_weight = if_else(dfmfd98 == 0,
                       distance / (1 - distance),   # reweight controls
                       1)                            # treated units unchanged
  )

did_psm_wtd <- lm(
  lexptot ~ year + dfmfd98 + dfmfdyr,
  data    = psm_hh,
  weights = a_weight
)
summary(did_psm_wtd)


###############################################################################
# STEP 3 — RESULTS TABLE
###############################################################################
#
#   We compare four estimators:
#
#   (1) Basic DiD       — OLS on full panel, no matching, no FE
#                         Estimate closest to the raw group mean comparison.
#
#   (2) DiD + HH FE     — Adds household fixed effects, removing all
#                         time-invariant selection bias. This is the
#                         Khandker (2006) strategy.
#
#   (3) PSM DiD         — OLS on matched sample only.
#                         Removes observable selection bias via matching.
#
#   (4) PSM DiD (wtd)   — IPW-weighted OLS on matched sample.
#                         Further reduces residual imbalance after matching.
#
#   THE KEY COEFFICIENT IN ALL MODELS IS "dfmfdyr" (Treatment effect — DiD).
#   Compare its magnitude and significance across specifications.
#   Robustness across models strengthens causal inference.
#
#   INTERPRETING THE COEFFICIENT:
#   Because the outcome is log(1 + expenditure), the coefficient β ≈ 100·β%
#   increase in expenditure for female microfinance participants relative
#   to controls, after removing common time trends.
#   This maps directly to the Khandker (2006) finding of ~21% returns to
#   cumulative borrowing for female members in 1998–99.

modelsummary(
  list(
    "Basic DiD"      = did_basic,
    "DiD + HH FE"   = did_fe,
    "PSM DiD"        = did_psm,
    "PSM DiD (wtd)" = did_psm_wtd
  ),
  stars   = TRUE,
  coef_map = c(
    "dfmfdyr" = "Treatment effect (DiD)",
    "year"    = "Year trend (1998 vs 1991)",
    "dfmfd98" = "Female MFI member (baseline level diff)"
  ),
  title   = "Table 1. Impact of Female Microfinance Participation on log(Expenditure)",
  notes   = paste0(
    "Outcome: log(1 + total household expenditure). ",
    "dfmfdyr = dfmfd98 × year (DiD interaction). ",
    "DiD + HH FE uses feols() with household fixed effects and clustered SEs. ",
    "PSM uses nearest-neighbour probit matching on pre-treatment covariates ",
    "(caliper = 0.01). PSM DiD (wtd) adds inverse-probability weights. ",
    "* p<0.1, ** p<0.05, *** p<0.01."
  )
)


###############################################################################
# STEP 4 — SENSITIVITY: MALE vs. FEMALE PARTICIPANTS
###############################################################################
#
#   Pitt & Khandker (1998) found that FEMALE microfinance participation
#   had LARGER effects on household expenditure than male participation
#   (Tk 18 vs Tk 11 per Taka borrowed).
#
#   One interpretation: women reinvest more of borrowed funds into
#   household consumption goods, or face fewer alternative investment
#   opportunities. The gender constraint (men in men-only groups,
#   women in women-only groups) allows identification of both effects.
#
#   Here we replicate the female estimate and contrast it with the male one.

# Male DiD (same structure as female)
did_male <- lm(lexptot ~ year + dmmfd98 + dmmfdyr, data = hh)
summary(did_male)

# Male DiD with household FE
did_male_fe <- feols(
  lexptot ~ year + dmmfdyr | nh,
  data    = hh,
  cluster = ~nh
)
summary(did_male_fe)

# Compare female vs. male treatment effects
modelsummary(
  list(
    "Female DiD"    = did_basic,
    "Female FE DiD" = did_fe,
    "Male DiD"      = did_male,
    "Male FE DiD"   = did_male_fe
  ),
  stars    = TRUE,
  coef_map = c(
    "dfmfdyr" = "Female treatment effect (DiD)",
    "dmmfdyr" = "Male treatment effect (DiD)",
    "year"    = "Year trend (1998)"
  ),
  title  = "Table 2. Female vs. Male Microfinance: Impact on log(Expenditure)",
  notes  = paste0(
    "* p<0.1, ** p<0.05, *** p<0.01. ",
    "FE models absorb time-invariant household characteristics. ",
    "Khandker (2006) found female returns of ~21% vs. lower male returns in 1998–99."
  )
)


###############################################################################
# SUMMARY OF FINDINGS — What to report
###############################################################################
#
#   From Pitt & Khandker (1998) and Khandker (2006), we expect:
#
#   ✓ Female microfinance participation INCREASES household expenditure.
#   ✓ The effect is LARGER for women than for men.
#   ✓ By 1998–99, average returns to cumulative borrowing for women ≈ 21%.
#   ✓ BUT poverty reduction was LOWER in 1998–99 (2 pp) than 1991–92 (5 pp),
#     suggesting diminishing returns as borrowing stocks grow.
#
#   Robustness checks:
#   ✓ Estimate is consistent across Basic DiD, FE DiD, PSM DiD.
#   ✓ Good covariate balance after matching (SMD < 0.1 for most variables).
#   ✓ VIF and condition number within acceptable limits (no severe collinearity).
#
#   Caveats:
#   ⚠ The parallel trends assumption cannot be formally tested with 2 periods.
#   ⚠ The landholding eligibility rule was not always enforced (Khandker 2006),
#     which motivated the PSM approach as a robustness check.
#   ⚠ Selection into participation remains a concern even after PSM if
#     unobservable characteristics (motivation, social networks) differ.
#
###############################################################################
# END OF SCRIPT
###############################################################################