# ============================================================================
# Multiple regression in R 
#
# The exercises use palmerpenguins::penguins, restricted to Chinstrap penguins.
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

chinstrap_dat <-
	  penguins |>
	  filter(species == "Chinstrap") |>
	  drop_na(flipper_length_mm, body_mass_g, bill_depth_mm, sex) |>
	  mutate(sex = factor(sex, levels = c("female", "male")))

chinstrap_dat


# b. Count the penguins of each sex.

chinstrap_dat |>
  count(sex)

# c. Plot body mass (x) and flipper length (y), colored by sex.

ggplot(data = chinstrap_dat, mapping = aes(x = body_mass_g, y = flipper_length_mm, color = sex)) +
  geom_point() +
  labs(
  title = "Body Mass vs Flipper Length by Sex",
  x = "Body Mass (g)", 
  y = "Flipper Length (mm)", 
  color = "Sex"
  )

# d. Describe the relationship and the overlap in body-mass ranges.
	
# females are generally smaller but there is overlap.
# I didn't realize she meant to code it until I was checking my answers mb

chinstrap_dat |>
  group_by(sex) |>
  summarize(
    min_mass_g = min(body_mass_g),
    max_mass_g = max(body_mass_g),
    .groups = "drop"
  )

# they overlap from 3250 to 4150 g ; min: fem = 2700 & male = 3250 ; 
# max: fem = 4150 & male = 4800

# ----------------------------------------------------------------------------
# 2. Fit and interpret an additive model 
# ----------------------------------------------------------------------------
# a. Fit an additive model using body mass and sex as predictors.
	
model_add <- lm(
  flipper_length_mm ~ body_mass_g + sex,
  data = chinstrap_dat
  )

# b. Examine coefficients with 95% confidence intervals.

conf_model_dat <- tidy(model_add, conf.int = TRUE) 
#term     estimate std.error statistic  p.value  conf.low conf.high
# female    161.      6.96       23.2   4.13e-33   147.       175.    
# body mass 0.00864   0.00196    4.41   3.95e- 5   0.00473    0.0125
# male      4.62      1.49       3.09   2.92e- 3   1.64       7.60  

# I renamed when I copy pasted bc it was easier but not with the answers below.

# c. Interpret each coefficient, including units and what is held constant.

# (Intercept) = 161.269 mm; predict mean flipper length for a female
# with a 0 g mass. 
# body_mass_g = 0.008637 mm/g; 1 g incr in body mass is related to a 0.008637 mm 
# incr in mean flipper length, holding sex constant 
# (aka comparing penguins within the same sex in the fitted model).
# sexmale = 4.620 mm; at the same body mass, predicted mean flipper
# length for males is 4.620 mm > than females.

	
# d. Calculate the predicted change for a 100 g increase in body mass.
	
change_100g <- conf_model_dat |>
  filter(term == "body_mass_g") |>
  transmute(change_100g_mm = estimate * 100)

change_100g
# ~ +0.864 mm mean flipper length per +100 g body mass

# e. Does the sexmale confidence interval include zero? Interpret it.


conf_model_dat |>
  filter(term == "sexmale") |>
  select(term, conf.low, conf.high, p.value)
# confidence int. of 1.636 to 7.604 mm: I'm not sure what that means in this context tbh

# f. What assumption is made about slopes?

# both sexes have the same slope.

# ----------------------------------------------------------------------------
# 3. Generate and plot predictions 
# ----------------------------------------------------------------------------
# a. Generate 100 masses within each sex's observed body-mass range.


chinstrap_frame <- 
  chinstrap_dat |>
  group_by(sex) |>
  reframe(
    body_mass_g = seq(
      min(body_mass_g),
      max(body_mass_g),
      length.out = 100
    )
  )

chinstrap_frame

# b. Generate predicted means and 95% confidence intervals.
# These bands describe uncertainty in the predicted mean, under the model assumptions.
# yes I took that^ from my notes bc I was confused as to how this was different from
# what we just did
	
chinstrap_predictions <- predict(
  model_add,
  newdata = chinstrap_frame,
  interval = "confidence",
  level = 0.95
) |>
  as_tibble()

chinstrap_predictions

#ok again, unclear af
chinstrap_binds <- bind_cols(chinstrap_frame, chinstrap_predictions)

head(chinstrap_binds)

# c. Plot observations, predictions, and confidence ribbons.

ggplot(
  data = chinstrap_dat, 
  mapping = aes(
    x = body_mass_g, 
    y = flipper_length_mm, 
    color = sex
    )
  ) +
  geom_ribbon(
    data = chinstrap_binds,
    mapping = aes(
      x = body_mass_g, 
      ymin = lwr, 
      ymax = upr,
      fill = sex, 
      group = sex
      ),
    inherit.aes = FALSE,
    alpha = 0.18
    ) +
  geom_point(alpha = 0.5) +
  geom_line(
	    data = chinstrap_binds,
	    mapping = aes(y = fit, group = sex),
	    linewidth = 1
	    )+
  labs(
    title = "Body Mass vs Flipper Length by Sex",
    x = "Body Mass (g)", 
    y = "Flipper Length (mm)", 
    color = "Sex"
    )

# d. Why are the predicted lines parallel?

#because the additive model assumes the lines are parallel

# e. What do the shaded bands describe?
	
# uncertainty in the predicted mean flipper length at each mass
# for each sex

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
  female_mm_per_g = female_slope,
  male_minus_female = slope_difference,
  male_mm_per_g = female_slope + slope_difference
)

slope_summary
#female_mm_per_g male_minus_female male_mm_per_g
#  0.00488           0.00608        0.0110

# d. Explain the change in the sexmale interpretation.

#   I don't get it tbh	
#   With an interaction, the difference at mass x is:
#   sexmale coefficient + interaction coefficient * x.

# e. Compare AIC values and calculate the difference.
	

aic_comparison <- AIC(model_add, chinstrap_int) |>
  as_tibble(rownames = "model") |>
  mutate(delta_AIC = AIC - min(AIC))

aic_comparison
# model            df   AIC   delta_AIC
# model_add         4  422.     0.430
# chinstrap_int     5  421.     0    

# f. Examine uncertainty in the interaction coefficient.

# again, wtf

# g. Which model would you select, and why?
	
#interaction bc its more accurate/specific

# ----------------------------------------------------------------------------
# 5. Examine model diagnostics 
# ----------------------------------------------------------------------------
# Use the additive model for the following diagnostics.
# a. Inspect all four default diagnostic plots.

plot(model_add)

# b. Obtain fitted values and residuals.

chinstrap_results <- augment(model_add, data = chinstrap_dat)

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

# I don't see any conspicuous patterns

# e. Examine the Q-Q plot. Are there substantial departures from the line?


chinstrap_results |>
  arrange(desc(.cooksd)) |>
  select(body_mass_g, flipper_length_mm, sex, .resid, .hat, .cooksd) |>
  slice_head(n = 3)

# we could be speaking parsletongue for all I know atp

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
	


# f. Examine the coefficients from all three models.
#    Interpret the intercept in the centered model and both slopes
#    in the standardized model. Include units and explain what
#    is held constant.
	



# g. Verify that the fitted values are equal across all three models,
# allowing for numerical rounding.
   
# h. Explain what happens to the slopes when you center or
#    standardize the predictors.	

# Optional check of the means, SDs, and fit summaries:
	
	
# i. Would standardizing fix curvature or unequal residual variance?
	

# ============================================================================
# Optional extension if time:
# compare additive and interaction predictions visually
# ============================================================================
# Generate predictions on the same grid and plot models side by side.
	
