## Exemplo 1

data(iris)


# creating a data frame from the iris data set
dat = data.frame(X = iris$Petal.Length,
                 Y = iris$Sepal.Length,
                 Z = iris$Petal.Width)

# linear regression model with interaction between X and Z
summary(lm(Y ~ X + Z + X:Z, data = dat))

summary(lm(Y ~ X*Z, data = dat))

summary(lm(Y ~ X:Z, data = dat))


## Exemplo 2

# https://www.geeksforgeeks.org/how-to-include-interaction-in-regression-using-r-programming/

# Dataset:
# Lung Capacity Dataset 
# Parameters/Variables: 
# Independent Variable(Y): LungCap
# Dependent Variable(X1): Smoke(Yes/No)
# Dependent Variable(X2): Age

# Read in the Lung Cap Data

LungCapData <- read_excel("LungCapData.xls")
write.csv(LungCapData, "LungCapData.csv")


# Attach LungCapData
attach(LungCapData)


# Plot the data, using different 
# colours for smoke(red)/non-smoke(blue)
# First, plot the data for 
# the Non-Smokers, in Blue
plot(Age[Smoke == "no"], 
     LungCap[Smoke == "no"], 
     col = "blue", 
     ylim = c(0, 15), xlim = c(0, 20), 
     xlab = "Age", ylab = "LungCap", 
     main = "LungCap vs. Age,Smoke")

# Now, add in the points for 
# the Smokers, in Solid Red Circles
points(Age[Smoke == "yes"], 
       LungCap[Smoke == "yes"],
       col = "red", pch = 16)

# And, add in a legend
legend(1, 15, 
       legend = c("NonSmoker", "Smoker"), 
       col = c("blue", "red"), 
       pch = c(1, 16), bty = "n")

# Fit a Reg Model, using Age,
# Smoke, and their INTERACTION
model1 <- lm(LungCap ~ Age*Smoke)
coef(model1)


# Note, that the "*" fits a model with 
# Age, Smoke and AgeXSmoke INT.
# Note, also that the same model 
# can be fit using the ":"
model1 <- lm(LungCap ~ Age + Smoke + Age:Smoke)

# Ask for a summary of the model
summary(model1)

# Now, let's add in the regression
# lines from our mode using the 
# abline command for the Non-Smokers, in Blue
abline(a = 1.052, b = 0.558,
       col = "blue", lwd = 3)

# And now, add in the line for Smokers, in Red
abline(a = 1.278, b = 0.498,
       col = "red", lwd = 3)

# Ask for that model summary again
summary(model1)

# Fit the model that does 
# NOT include INTERACTION
model2 <- lm(LungCap ~ Age + Smoke)
summary(model2)

