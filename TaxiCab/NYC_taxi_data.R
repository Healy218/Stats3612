##STAT 3612: TEMPLATE FOR TAXI DATA #####

#INITIALIZATION
#install.packages("arrow")   #Run once
library(arrow)
library(data.table)

#READ IN DATA
#Changed save files so they would be easier to read.
taxi_trips_jan <- read_parquet('TaxiCab/janTaxi2025.parquet')
taxi_trips_july <- read_parquet('TaxiCab/julyTaxi2025.parquet')


########## PROBLEM 1 ###################
summary(taxi_trips_jan$trip_distance)
summary(taxi_trips_july$trip_distance)

trip_distance_jan <-taxi_trips_jan$trip_distance
trip_distance_jan <- trip_distance_jan[
  trip_distance_jan > 0 &
    trip_distance_jan < 25
]
trip_distance_july <- taxi_trips_july$trip_distance
trip_distance_july <- trip_distance_july[
  trip_distance_july > 0 &
    trip_distance_july < 25
]

########## FIND CONFIDENCE INTERVAL FOR THE AVERAGE TRIP DISTANCES ##########
### NOTE: Are the assumptions required for the below work justified? Why? 
### Yes. CLT makes our CI valid. 

### 95% CI for January Trips
mean_val <- mean(trip_distance_jan)  # sample mean
sample_sd <- sd(trip_distance_jan) # sample standard deviation
n <- length(trip_distance_jan)        # sample size
conf_level <- 0.95
degrees_of_freedom <- n - 1

# Calculate critical t-value
# The qt function is used to find the quantile of the t-distribution
t_critical <- qt(1 - (1 - conf_level) / 2, df = degrees_of_freedom)

# Calculate the margin of error
margin_of_error <- t_critical * (sample_sd / sqrt(n))

# Calculate the confidence interval
lower_bound <- mean_val - margin_of_error
upper_bound <- mean_val + margin_of_error

# Print the result
cat(sprintf(
  "95%% confidence interval for the average january trip distance is: [%.2f, %.2f]\n",
  lower_bound,
  upper_bound
))

### 95% CI for July Trips

mean_val <- mean(trip_distance_july)  # sample mean
sample_sd <- sd(trip_distance_july) # sample standard deviation
n <- length(trip_distance_july)        # sample size
conf_level <- 0.95
degrees_of_freedom <- n - 1

# Calculate critical t-value
# The qt function is used to find the quantile of the t-distribution
t_critical <- qt(1 - (1 - conf_level) / 2, df = degrees_of_freedom)

# Calculate the margin of error
margin_of_error <- t_critical * (sample_sd / sqrt(n))

# Calculate the confidence interval
lower_bound <- mean_val - margin_of_error
upper_bound <- mean_val + margin_of_error

# Print the result
cat(sprintf(
  "95%% confidence interval for the average july trip distance is: [%.2f, %.2f]\n",
  lower_bound,
  upper_bound
))

############# Problem 3 ###################################

trip_fare_jan <-taxi_trips_jan$fare_amount  
# Here is an initial summary.  I am checking to see if these are reasonable values

cat(
  "The inital january trip fare summary is:\n")
print(summary(trip_fare_jan))

# They are not! So I will attempt to clean this
#I will keep the trip fares between 0 to 500 and see the proportion of rides in this amount
trip_fare_jan_cleaned <- taxi_trips_jan$fare_amount[
  taxi_trips_jan$trip_distance > 0 &
  taxi_trips_jan$trip_distance < 25 &
  taxi_trips_jan$fare_amount > 0 &
  taxi_trips_jan$fare_amount< 500
]
cat(
  "The proportion of rides in this new data is:\n",
  length(trip_fare_jan_cleaned) / length(trip_fare_jan),"\n")

#Now check to see if this is more reasonable 
cat(
  "The cleaned january trip fare summary is:\n")
print(summary(trip_fare_jan_cleaned))

#Prediction Intervals
#############################################################
##NOTE: WHAT ASSUMPTIONS ARE REQUIRED FOR THE PREDICTION INTERVAL CALCULATION DONE BELOW?
## DOES THE DATA SUPPORT THESE ASSUMPTIONS??
# No! The underlying distribution must be at least approximately normally distributed
# A quick look at the histogram clearly invalidates this.

hist(trip_fare_jan_cleaned,col = 2,
     breaks = 100)

#See what happens if we just continue on:

# Define your parameters
sample_mean <- mean(trip_fare_jan_cleaned)
sample_sd <- sd(trip_fare_jan_cleaned)
sample_size <- length(trip_fare_jan_cleaned)
confidence_level <- 0.95

# Calculate the t-multiplier (for 95% interval, alpha = 0.05, alpha/2 = 0.025)
alpha <- 1 - confidence_level
t_multiplier <- qt(1 - alpha / 2, df = sample_size - 1)

# Calculate the margin of error component K * s
# Note the formula for the standard error of prediction is slightly different
# than the standard error of the mean (it includes the '1 +' term)
margin_of_error <- t_multiplier * sample_sd * sqrt(1 + 1/sample_size)

# Calculate the prediction interval bounds
lower_bound <- sample_mean - margin_of_error
upper_bound <- sample_mean + margin_of_error

# Print the results
cat(sprintf(
  "The %g%% prediction interval for cab fare is [%.2f, %.2f]\n",
  confidence_level * 100,
  lower_bound,
  upper_bound
))

### This is a better way to do it! ###########

## A DISTRIBUTION-FREE 95% PREDICTION INTERVAL  

#BECAUSE THE UNDERLYING DISTRIBUTION IS NOT NORMAL, TO FIND A PREDICTION INTERVAL I WILL GIVE THE MIDDLE 95 OF THE DATA

q_bounds <- quantile(trip_fare_jan_cleaned, 
                     c(0.025, 0.975))

cat(sprintf(
  "The distribution-free 95%% prediction interval is [%.2f, %.2f]\n",
  q_bounds[1], 
  q_bounds[2]
))


############# Problem 4 ###################################
# Is a hard question and you should think about how this is done. 
# The real answer is: just from the dataset you cannot determine the average amount made by a taxi
# I do not know how many distinct taxis there are in this data set. 
# My best answer is: We know the average fare amount. We will have to make an educated guess as to how many fares a day?
# We know the average trip time elapsed, maybe we can estimate average hours worked? Any good ideas?

############# Problem 5 ###################################

#### HISTOGRAMS ######### 
hist(trip_distance_jan[trip_distance_jan < 25],
     col = 3,
     breaks = 100)
hist(trip_distance_july[trip_distance_july < 25],
     col = 3,
     breaks = 100)