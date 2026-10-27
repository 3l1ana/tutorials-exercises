# ============================================================================
# Multiple regression in R -- ANSWER KEY
#
# Questions are followed by worked code and example written answers.
# Results use palmerpenguins::penguins, restricted to Chinstrap penguins.
# Requires tidyverse, broom, and palmerpenguins to be installed.
# ============================================================================

	library(tidyverse)
	library(broom)
	library(palmerpenguins)
	
	theme_set(theme_bw(base_size = 12))

# ----------------------------------------------------------------------------
# 1. Prepare and explore the data
# ----------------------------------------------------------------------------
# Question: How does mean flipper length vary with body mass and sex
# in Chinstrap penguins?
#
# a. Create chinstrap_dat: keep Chinstrap penguins, remove missing values
#    in flipper_length_mm, body_mass_g, bill_depth_mm, and sex, and make
#    female the reference level for sex. Use this sample throughout.

	chinstrap_dat <- penguins |>
	  filter(species == "Chinstrap") |>
	  drop_na(flipper_length_mm, body_mass_g, bill_depth_mm, sex) |>
	  mutate(sex = factor(sex, levels = c("female", "male")))

# b. Count the penguins of each sex.
	chinstrap_dat |>
	  count(sex)
	
# Answer: 68 penguins: 34 females and 34 males.

# c. Plot body mass (x) and flipper length (y), colored by sex.
	p_explore <- ggplot(
	  data = chinstrap_dat,
	  mapping = aes(x = body_mass_g, y = flipper_length_mm, color = sex)
	) +
	  geom_point(alpha = 0.7) +
	  labs(
	    x = "Body mass (g)",
	    y = "Flipper length (mm)",
	    color = "Sex",
	    title = "Chinstrap penguins: flipper length and body mass"
	  )
	
	p_explore

# d. Describe the relationship and the overlap in body-mass ranges.
	
	chinstrap_dat |>
	  group_by(sex) |>
	  summarize(
	    min_mass_g = min(body_mass_g),
	    max_mass_g = max(body_mass_g),
	    .groups = "drop"
	  )

	# Example answer: Flipper length generally increases with body mass.
	# Females range from 2700 to 4150 g; males range from 3250 to 4800 g.
	# Their observed ranges overlap from 3250 to 4150 g. Comparisons between
	# sexes at the same mass are best supported within this shared range.

# ----------------------------------------------------------------------------
# 2. Fit and interpret an additive model 
# ----------------------------------------------------------------------------
# a. Fit an additive model using body mass and sex as predictors.
	
	chinstrap_add <- lm(
	  flipper_length_mm ~ body_mass_g + sex,
	  data = chinstrap_dat
	)

# b. Examine coefficients with 95% confidence intervals.

	add_coefficients <- tidy(chinstrap_add, conf.int = TRUE)
	add_coefficients

# c. Interpret each coefficient, including units and what is held constant.

	# Answer:
	# - Intercept = 161.269 mm: predicted mean flipper length for a female
	#   with body mass 0 g. 
	# - body_mass_g = 0.008637 mm/g: a 1 g increase in body mass is associated
	#   with a predicted 0.008637 mm increase in mean flipper length, holding
	#   sex constant. This model assumes the same slope for both sexes.
	# - sexmale = 4.620 mm: at the same body mass, predicted mean flipper
	#   length is 4.620 mm greater for males than females. The model assumes
	#   this difference is constant across body mass.

# d. Calculate the predicted change for a 100 g increase in body mass.
	
	change_per_100g <- add_coefficients |>
	  filter(term == "body_mass_g") |>
	  transmute(change_per_100g_mm = estimate * 100)
	
	change_per_100g

	# Answer: About 0.864 mm greater mean flipper length per 100 g increase,
	# holding sex constant.

# e. Does the sexmale confidence interval include zero? Interpret it.

	add_coefficients |>
	  filter(term == "sexmale") |>
	  select(term, estimate, conf.low, conf.high, p.value)

	# Answer: No. The interval is approximately 1.636 to 7.604 mm.
	# Under the additive model and its assumptions, there is evidence of
	# greater mean flipper length for males at the same body mass.

# f. What assumption is made about slopes?
	
	# Answer: Females and males share one body-mass slope; predicted lines
	# are parallel. Fitting this model does not establish that population
	# slopes are identical.

# ----------------------------------------------------------------------------
# 3. Generate and plot predictions 
# ----------------------------------------------------------------------------
# a. Generate 100 masses within each sex's observed body-mass range.

	chinstrap_grid <- chinstrap_dat |>
  	group_by(sex) |>
  	reframe(
  	  body_mass_g = seq(
  	    min(body_mass_g),
  	    max(body_mass_g),
  	    length.out = 100
  	  )
  	)
	
	chinstrap_grid

# b. Generate predicted means and 95% confidence intervals.
	
	chinstrap_predictions <- predict(
	  chinstrap_add,
	  newdata = chinstrap_grid,
	  interval = "confidence",
	  level = 0.95
	) |>
	  as_tibble()
	

	chinstrap_bands <- bind_cols(chinstrap_grid, chinstrap_predictions)

	head(chinstrap_bands)

# c. Plot observations, predictions, and confidence ribbons.
	p_additive <- ggplot(
	  data = chinstrap_dat,
	  mapping = aes(x = body_mass_g, y = flipper_length_mm, color = sex)) +
	  geom_ribbon(
	    data = chinstrap_bands,
	    mapping = aes(
	      x = body_mass_g, ymin = lwr, ymax = upr,
	      fill = sex, group = sex),
	    inherit.aes = FALSE,
	    alpha = 0.18) +
	  geom_point(alpha = 0.5) +
	  geom_line(
	    data = chinstrap_bands,
	    mapping = aes(y = fit, group = sex),
	    linewidth = 1.1) +
	  labs(
	    x = "Body mass (g)",
	    y = "Flipper length (mm)",
	    color = "Sex", fill = "Sex",
	    title = "Chinstrap penguins: additive model",
	    subtitle = "Predicted means with 95% pointwise confidence intervals"
	  )

	p_additive

# d. Why are the predicted lines parallel?
	
	# Answer: The additive model fits one shared body-mass slope. The sexmale
	# coefficient shifts the male line vertically without changing its slope.

# e. What do the shaded bands describe?
	
	# Answer: Uncertainty in the predicted mean flipper length at each mass
	# for each sex, under model assumptions. 
	
# ----------------------------------------------------------------------------
# 4. Fit and evaluate an interaction model 
# ----------------------------------------------------------------------------
# a. Fit a model that allows the body-mass slope to differ by sex.
	
	chinstrap_int <- lm(
	  flipper_length_mm ~ body_mass_g * sex,
	  data = chinstrap_dat
	)

# b. Inspect coefficients and confidence intervals.
	
	int_coefficients <- tidy(chinstrap_int, conf.int = TRUE)
	
	int_coefficients

# c. Report the female slope, slope difference, and male slope.
	
	female_slope <- int_coefficients |>
	  filter(term == "body_mass_g") |>
	  pull(estimate)
	
	slope_difference <- int_coefficients |>
	  filter(term == "body_mass_g:sexmale") |>
	  pull(estimate)
	
	slope_summary <- tibble(
	  female_slope_mm_per_g = female_slope,
	  male_minus_female_slope_mm_per_g = slope_difference,
	  male_slope_mm_per_g = female_slope + slope_difference
	)
	
	?pull
	
	slope_summary
	
	# Answer: Female slope = 0.004883 mm/g; male-minus-female difference =
	# 0.006085 mm/g; male slope = 0.010968 mm/g. The estimated male slope
	# is steeper, but its difference from the female slope is uncertain.

# d. Explain the change in the sexmale interpretation.
	
	# Answer: In the additive model, sexmale is the constant male-minus-female
	# difference at any common body mass. In the interaction model, sexmale
	# is that difference specifically at body mass 0 g (about -17.802 mm).
	# With an interaction, the difference at mass x is:
	#   sexmale coefficient + interaction coefficient * x.

# e. Compare AIC values and calculate the difference.
	
	aic_comparison <- AIC(chinstrap_add, chinstrap_int) |>
	  as_tibble(rownames = "model") |>
	  mutate(delta_AIC = AIC - min(AIC))
	
	aic_comparison

	# Answer: Additive AIC is approximately 421.754; interaction AIC is
	# approximately 421.324. The interaction model is lower by about 0.430.
	# This small difference indicates similar support; AIC does not clearly
	# favor retaining the interaction. Both models use the same response
	# and the same observations.

# f. Examine uncertainty in the interaction coefficient.

		int_coefficients |>
  		filter(term == "body_mass_g:sexmale") |>
  		select(term, estimate, conf.low, conf.high, p.value)

	# Answer: The 95% interval is approximately -0.001881 to 0.014050 mm/g.
	# It includes zero, so there is not clear evidence of different population
	# slopes at the 5% significance level under these model assumptions.

# g. Which model would you select, and why?
	# Example answer: Select additive model because AIC support
	# is similar and the additional interaction coefficient is uncertain.
	# The simpler model is more parsimonious. There is not clear evidence
	# that the slopes differ between sexes.

# ----------------------------------------------------------------------------
# 5. Examine model diagnostics 
# ----------------------------------------------------------------------------
# Use the additive model selected in the example answer above.
# a. Inspect all four default diagnostic plots.

	plot(chinstrap_add)

# b. Obtain fitted values and residuals.

	chinstrap_results <- augment(chinstrap_add, data = chinstrap_dat)

# c. Plot residuals against body mass, colored by sex.
	
	p_residuals <- ggplot(
	  data = chinstrap_results,
	  mapping = aes(x = body_mass_g, y = .resid, color = sex)
	) +
	  geom_point(alpha = 0.7) +
	  geom_hline(yintercept = 0, linetype = "dashed") +
	  labs(
	    x = "Body mass (g)",
	    y = "Residual (mm)",
	    color = "Sex",
	    title = "Additive model: residuals versus body mass"
	  )
	
	p_residuals

# d. Describe any patterns in the residuals, including curvature,
#    changes in spread, or differences between sexes.

# Example answer: There is substantial scatter in the residuals.
# The estimated relationships suggest a shallower slope for females
# and a steeper slope for males, so look for opposing trends by sex.
# Examine whether there is a curved pattern or a change in spread,
# especially near the ends of each group's body-mass range.

# e. Examine the Q-Q plot. Are there substantial departures from the line?

# Answer: Most points follow the reference line fairly closely,
# especially in the middle. There are some departures in both tails,
# but no strong departure from normality is apparent.
# The labeled observations have relatively large standardized residuals
# and could be examined further.
	
	chinstrap_results |>
	  arrange(desc(.cooksd)) |>
	  select(body_mass_g, flipper_length_mm, sex, .resid, .hat, .cooksd) |>
	  slice_head(n = 3)

# ----------------------------------------------------------------------------
# 6. Center and standardize numeric predictors 
# ----------------------------------------------------------------------------
# New question: How does mean flipper length vary with body mass and
# bill depth?
#
# a. Fit the model in original units.
	
	chinstrap_numeric <- lm(
	  flipper_length_mm ~ body_mass_g + bill_depth_mm,
	  data = chinstrap_dat
	)

# b. Center each predictor using its mean.
	
	chinstrap_dat <- chinstrap_dat |>
	  mutate(
	    body_mass_c = body_mass_g - mean(body_mass_g),
	    bill_depth_c = bill_depth_mm - mean(bill_depth_mm)
	  )

# c. Fit a model with centered predictors.
	
	chinstrap_centered <- lm(
	  flipper_length_mm ~ body_mass_c + bill_depth_c,
	  data = chinstrap_dat
	)

# d. Standardize both predictors: subtract the mean, then divide by SD.
	
	chinstrap_dat <- chinstrap_dat |>
	  mutate(
	    body_mass_z = (body_mass_g - mean(body_mass_g)) / sd(body_mass_g),
	    bill_depth_z = (bill_depth_mm - mean(bill_depth_mm)) / sd(bill_depth_mm)
	  )

# e. Fit a model with standardized predictors.
	
	chinstrap_standardized <- lm(
	  flipper_length_mm ~ body_mass_z + bill_depth_z,
	  data = chinstrap_dat
	)

# f. Examine the coefficients from all three models.
#    Interpret the intercept in the centered model and both slopes
#    in the standardized model. Include units and explain what
#    is held constant.
	
	coefficient_comparison <- bind_rows(
	  original = tidy(chinstrap_numeric, conf.int = TRUE),
	  centered = tidy(chinstrap_centered, conf.int = TRUE),
	  standardized = tidy(chinstrap_standardized, conf.int = TRUE),
	  .id = "model") |>
	  select(model, term, estimate, std.error, conf.low, conf.high)
	
	coefficient_comparison

	# Answer: The centered intercept is approximately 195.824 mm: predicted
	# mean flipper length at the sample's mean mass (3733.088 g) and mean
	# bill depth (18.421 mm). 

	# The body_mass_z coefficient is approximately 3.269 mm: a one-SD
	# increase in body mass (about 384.335 g) is associated with a predicted
	# 3.269 mm increase in mean flipper length, holding bill depth constant.

	# The bill_depth_z coefficient is approximately 2.161 mm per one-SD 
	# increase in bill depth (about 1.135 mm), holding body mass constant. 
	

# g. Verify equality of fitted values, allowing for numerical rounding.
	all.equal(
  fitted(chinstrap_numeric),
  fitted(chinstrap_centered)
)
	
	all.equal(
	  fitted(chinstrap_numeric),
	  fitted(chinstrap_standardized)
	)
# Answer: TRUE.

# h. Explain what happens to the slope if you center or standardize. 
	
	# Answer:
	# - Centering shifts the predictor origin. A 1 g or 1 mm increment still
	#   has its original size, so the additive-model slopes are unchanged.
	# - Standardizing changes to units of SD. Each new slope
	#   equals the original slope multiplied by that predictor's sample SD.
	# - Flipper length was not standardized. The response remains in mm.
	# - Centered and standardized models have the same intercept because
	#   both assign zero to each predictor's sample mean.

# Optional check of the means, SDs, and fit summaries:
	
	chinstrap_dat |>
	  summarize(
	    mean_mass_c = mean(body_mass_c),
	    mean_depth_c = mean(bill_depth_c),
	    mean_mass_z = mean(body_mass_z),
	    mean_depth_z = mean(bill_depth_z),
	    sd_mass_z = sd(body_mass_z),
	    sd_depth_z = sd(bill_depth_z)
	  )
	
	bind_rows(
	  original = glance(chinstrap_numeric),
	  centered = glance(chinstrap_centered),
	  standardized = glance(chinstrap_standardized),
	  .id = "model"
	) |>
	  select(model, nobs, r.squared, adj.r.squared, sigma, AIC)
	
# i. Would standardizing fix curvature or unequal residual variance?
	
	# Answer: No. Centering and standardizing predictors leave fitted values
	# and residuals unchanged for these equivalent additive linear models.
	# They also do not remove correlation between the two predictors.

# ============================================================================
# Optional extension if time:
# compare additive and interaction predictions visually
# ============================================================================
# Generate predictions on the same grid and plot models side by side.
	
	comparison_lines <- bind_rows(
	  additive = augment(chinstrap_add, newdata = chinstrap_grid),
	  interaction = augment(chinstrap_int, newdata = chinstrap_grid),
	  .id = "model"
	)
	
	p_comparison <- ggplot(
	  data = chinstrap_dat,
	  mapping = aes(x = body_mass_g, y = flipper_length_mm, color = sex)
	) +
	  geom_point(alpha = 0.5) +
	  geom_line(
	    data = comparison_lines,
	    mapping = aes(y = .fitted, group = sex),
	    linewidth = 1.1
	  ) +
	  facet_wrap(~ model) +
	  labs(
	    x = "Body mass (g)",
	    y = "Flipper length (mm)",
	    color = "Sex",
	    title = "Chinstrap penguins: additive and interaction models",
	    subtitle = "Lines span each sex's observed body-mass range"
	  )
	
	p_comparison

	# Example answer: The interaction shows a steeper male slope and a
	# shallower female slope. Predictions are fairly similar across much of
	# the data, with greater differences toward the ends of each group's
	# observed range. Similar AIC support and the interaction CI
	# support a cautious interpretation of these visual differences.
	# ============================================================================
