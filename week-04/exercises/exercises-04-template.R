	# Exercise: Data Wrangling with dplyr — Part 1
	# Dataset: ggplot2::mpg
	
	# Estimated time: 30 minutes
	
	# Learning goals:
	
	# By the end of this exercise, you should be able to:
	# - filter rows with filter()
	# - select columns with select()
	# - reorder columns with relocate()
	# - rename columns with rename() and rename_with()
	# - create and modify variables with mutate()
	# - summarize data with summarise()
	# - calculate summaries by groups with group_by() and summarise()
	# - combine multiple dplyr functions in a single pipeline
	
	# Instructions:
	# 1. Write your code below each question.
	# 2. Use descriptive object names written in snake_case.
	# 3. Run your script from top to bottom before you finish.
	# 4. Do not delete the questions or instructions - these will help you later.
	
	# ------------------------------------------------------------
	# Setup
	# ------------------------------------------------------------
	
	library(tidyverse)
	
	# We will use the mpg dataset from {ggplot2}.
	# It contains fuel economy information for 234 vehicle models.
	#
	# Some important variables:
	# manufacturer = vehicle manufacturer
	# model        = vehicle model
	# displ        = engine displacement, in liters
	# year         = model year
	# cyl          = number of cylinders
	# trans        = transmission
	# drv          = drivetrain: f = front-wheel, r = rear-wheel, 4 = 4-wheel
	# cty          = city miles per gallon
	# hwy          = highway miles per gallon
	# class        = vehicle class
	
	
	
	# Take a quick look at the dataset.
	mpg
	
	glimpse(mpg)
	
	# ============================================================
	# Part 1: filter()
	# ============================================================
	
	# 1. Create an object called efficient_cars that contains only
	# vehicles with highway fuel economy of at least 30 mpg.
	
	efficient_cars <- mpg |> 
	  filter(hwy >= 30)
	
	efficient_cars
	
	# How many observations (rows) are in the resulting dataset?
	
nrow(efficient_cars)
#26

	# 2. Filter mpg to include only SUVs made in 2008.
	# Hint: Both conditions must be true.
	
mpg |> 
  filter()

	# 3. Filter mpg to include vehicles that:
	# - have 6 cylinders OR
	# - get more than 25 mpg on the highway.
	# Save the result as new_dat.
	

	#4. Filter mpg to include vehicles made by Toyota, Honda,
	# or Subaru.
	#
	# Hint: %in% may be useful here.
	
	
	
	# ============================================================
	# Part 2: select()
	# ============================================================
	
	# 5. Create a smaller dataset called mpg_small containing only:
	# manufacturer, model, class, displ, cty, hwy
	

	# 6. Starting with mpg, select:
	# - manufacturer
	# - model
	# - every variable from cty through hwy
	
	# Do this using : rather than writing every variable name.
	

	# 7. Starting with mpg, select manufacturer, model, and all
	# variables whose names end in "y".
	#
	# Hint: use a selection helper.
	

	
	# ============================================================
	# Part 3: relocate() and rename()
	# ============================================================
	
	# 8. Starting with mpg_small, move class so that it appears
	# immediately after manufacturer.
	#
	# Do not manually re-select every column; use relocate().
	# Save the result back to mpg_small.
	

	# 9. Starting with mpg_small, rename:
	# cty -> city_mpg
	# hwy -> highway_mpg
	
	# Save the result as mpg_named.
	

	# 10. Starting with mpg, use rename_with() to convert the names
	# of all variables ending in "y" to uppercase.
	#
	# Do not change the values in the columns.
	

	
	# ============================================================
	# Part 4: mutate()
	# ============================================================
	
	# 11. Starting with mpg_named, create a new variable called
	# mpg_difference that gives the difference between highway
	# and city fuel economy (mpg_difference = highway_mpg - city_mpg)
	#
	# Save the result as mpg_mutated.
	

	# 12. Engine displacement is currently in liters.
	# Starting with mpg_mutated, create a new variable called
	# engine_size_cm3 that converts engine displacement to
	# cubic centimeters.
	#
	# 1 liter = 1000 cubic centimeters
	
	# Save the result back to mpg_mutated.
	

	# ============================================================
	# Part 5: summarise()
	# ============================================================
	
	# 13. Using mpg, calculate the overall mean highway mpg.
	#
	# Name the resulting variable mean_hwy_mpg.
	

	# 14. Using mpg, calculate:
	# - mean highway mpg
	# - standard deviation of highway mpg
	# - minimum highway mpg
	# - maximum highway mpg
	#
	# Give each summary a descriptive name.
	


	# ============================================================
	# Part 6: group_by() + summarise()
	# ============================================================
	
	# 15. What is the mean highway mpg for each vehicle class?
	#
	# Your output should have one row per vehicle class.
	# Name the summary variable mean_hwy_mpg.
	
	
	# 16. For each vehicle class, calculate:
	# - mean city mpg
	# - mean highway mpg
	# - standard deviation of highway mpg
	#
	# Use .groups = "drop" so the resulting data frame is ungrouped.
	

	
	# 17. For vehicles made in 2008 only, calculate the minimum
	# and maximum highway mpg for each vehicle class.
	#
	# Write this as one pipeline using:
	# filter()
	# group_by()
	# summarise()
	

	
	
	# ============================================================
	# Part 7: Integrated challenge
	# ============================================================
	
	# 18. Starting with mpg, write ONE pipeline that:
	# a. keeps only vehicles from 2008
	# b. keeps only manufacturer, model, class, cty, and hwy
	# c. creates a variable called efficiency_gain equal to: hwy - cty
	# d. groups vehicles by class
	# e. calculates:
	# 	- mean highway mpg
	# 	- mean efficiency gain
	#
	# Name the final summary variables:
	# mean_highway_mpg and mean_efficiency_gain
	#
	# Save the final table as class_summary_2008.
	

	
	# ------------------------------------------------------------
	# Final check
	# ------------------------------------------------------------
	
	# Run your entire script from top to bottom.
	#
	# You should now have practiced:
	# filter()
	# select()
	# relocate()
	# rename()
	# rename_with()
	# mutate()
	# summarise()
	# group_by()
	# the pipe |>
	
	# Before finishing, make sure:
	# - all of your code runs without errors
	# - object names are descriptive
	# - you have used correct style and indentation
	# - you understand what each step in your pipelines is doing
	