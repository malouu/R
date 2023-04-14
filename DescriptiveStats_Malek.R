# Load the built-in dataset "mtcars"
data(mtcars)

# Load the ggplot2 library
library(ggplot2)

# Print the first six rows of the dataset
head(mtcars)

# Get summary statistics for the dataset
summary(mtcars)

######### Miles per gallon "mpg" ##########  

# Take a random sample of 10 values from the mpg variable
set.seed(123) # Set the random seed for reproducibility
sample_mpg <- sample(mtcars$mpg, size = 10)
# Print the sample
sample_mpg

# Create a histogram of miles per gallon
hist(mtcars$mpg, main = "Distribution of mpg", xlab = "Miles per gallon", ylab = "Frequency", col = "lightblue", border = "white")

# Add a vertical line at the mean of mpg
abline(v = mean(mtcars$mpg), col = "red") 

######## Normal distribution plot of the "mpg" variable ########

# Calculate mean and standard deviation of mpg
mpg_mean <- mean(mtcars$mpg)
mpg_sd <- sd(mtcars$mpg)

# Create a sequence of values to plot the normal distribution
x <- seq(mpg_mean - 3 * mpg_sd, mpg_mean + 3 * mpg_sd, length.out = 100)

# Plot the normal distribution with a vertical line for the mean
ggplot(data.frame(x), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = mpg_mean, sd = mpg_sd), aes(color = "Density")) +
  geom_vline(xintercept = mpg_mean, color = "red", linetype = "dashed") +
  labs(x = "Miles per gallon", y = "Density", title = "Normal Distribution of Miles per Gallon") +
  scale_color_manual(values = "blue")

# Calculate the probability of getting a value between 20 and 25
pnorm(25, mean = mpg_mean, sd = mpg_sd) - pnorm(20, mean = mpg_mean, sd = mpg_sd)

library(moments)
skewness(mtcars$mpg)
kurtosis(mtcars$mpg)

#end normal distribution


##### mpg & gear #####

# Calculate the mean and standard deviation of mpg for cars with four gears
four_gears <- mtcars$gear == 4
mpg_four_gears <- mtcars$mpg[four_gears]
mean_mpg_four_gears <- mean(mpg_four_gears)
sd_mpg_four_gears <- sd(mpg_four_gears)

# Print the results
cat("Mean mpg for cars with four gears:", mean_mpg_four_gears, "\n")
cat("Standard deviation of mpg for cars with four gears:", sd_mpg_four_gears, "\n")

##### mpg & cyl #####

# Create boxplots of mpg by number of cylinders
boxplot(mpg ~ cyl, data = mtcars, main = "Miles per gallon by number of cylinders", 
        xlab = "Number of cylinders", ylab = "Miles per gallon")


##### mpg & wt #####

#Correlation coefficient between miles per gallon (mpg) and weight (wt)
cor(mtcars$mpg, mtcars$wt)

# Create a scatterplot of mpg vs. wt
plot(mtcars$wt, mtcars$mpg, main = "Relationship between mpg and weight",
     xlab = "Weight", ylab = "Miles per gallon", col = "blue", pch = 19)

# Add a regression line to the plot
abline(lm(mpg ~ wt, data = mtcars), col = "red") 


##### mpg & am #####

#mean of miles per gallon (mpg) for cars with manual transmission (am)
mean(mtcars$mpg[mtcars$am == 1])

# Create a bar chart of transmission type
transmission <- table(mtcars$am)
barplot(transmission, main = "Transmission type", xlab = "Transmission", ylab = "Frequency")

# Create boxplots of mpg by transmission type
ggplot(mtcars, aes(x = factor(am), y = mpg)) +
  geom_boxplot() +
  xlab("Transmission Type") +
  ylab("Miles per Gallon") +
  ggtitle("Boxplots of Miles per Gallon by Transmission Type")

##### mpg & hp #####

# Create a scatterplot of mpg and hp
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  xlab("Horsepower") +
  ylab("Miles per gallon") +
  ggtitle("Scatterplot of Miles per Gallon and Horsepower")












