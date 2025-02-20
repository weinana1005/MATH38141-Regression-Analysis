# Q1a
data_matrix <- matrix(c(
  85.1, 8.5, 5.1, 14.7,
  106.3, 12.9, 5.8, 8.8,
  50.2, 5.2, 2.1, 5.1,
  130.6, 10.7, 8.4, 12.2,
  54.8, 3.1, 2.9, 10.6,
  30.3, 3.5, 1.2, 3.5,
  79.4, 9.2, 3.7, 9.7,
  91, 9, 7.6, 15.9,
  135.4, 15.1, 7.7, 20.8,
  89.3, 10.2, 4.5, 7.9
), nrow = 10, ncol = 4, byrow = TRUE)

#Each column as a vector
boxoffice <- data_matrix[, 1]
production <- data_matrix[, 2]
promotion <- data_matrix[, 3]
books <- data_matrix[, 4]

# Q1a. The plots
x_axis_limits <- range(c(0, production, promotion, books, 25))
y_axis_limits <- range(c(0, boxoffice, 150))
plot(production, boxoffice, main = "Scatter Plots of Box Office against each of the three variables", xlab = "Production, Promotion and Books", ylab = "Box Office", pch = 16, col = "blue", xlim = x_axis_limits, ylim = y_axis_limits)
points(promotion, boxoffice, pch = 16, col = "red")
points(books, boxoffice, pch = 16, col = "green")
legend("topright", legend = c("Production", "Promotion", "Books"), col = c("blue", "red", "green"), lwd = 2)
line1 <- lm(boxoffice ~ production)
abline(line1, col= "blue")
line1 <- lm(boxoffice ~ promotion)
abline(line1, col= "red")
line1 <- lm(boxoffice ~ books)
abline(line1, col= "green")

# Q1b
data1 <- c(1, 8.5, 5.1, 14.7,
          1, 12.9, 5.8, 8.8,
          1, 5.2, 2.1, 5.1,
          1, 10.7, 8.4, 12.2,
          1, 3.1, 2.9, 10.6,
          1, 3.5, 1.2, 3.5,
          1, 9.2, 3.7, 9.7,
          1, 9, 7.6, 15.9,
          1, 15.1, 7.7, 20.8,
          1, 10.2, 4.5, 7.9)

# Define X
X <- matrix(data1, ncol = 4, byrow = TRUE)
X
# Define Y
data_Y <- c(85.1, 106.3, 50.2, 130.6, 54.8, 30.3, 79.4, 91, 135.4, 89.3)
Y <- matrix(data_Y, ncol = 1)
Y


# Q1c
# Form the transpose of X
XT = t(X)
XT
# Calculate XTX and XTY 
XTX = t(X)%*%X
XTY = XT %*% Y
XTX
XTY
# Find the inverse for (XTX)
XTXI = solve(t(X)%*%X)
XTXI
# Find the least squares estimates of B0 B1 B2 B3
beta.hat = (solve(t(X)%*%X))%*%t(X)%*%Y
beta.hat

# Calculate predictions
Yhat = X %*% beta.hat
Yhat
# Calculate residuals
e = Yhat - Y
e

# Calculate SSR
SSR = t(beta.hat) %*% XTY - length(Y)*(mean(Y)^2)
SSR

# Calculate SSE
SSE = sum(e^2) 
SSE

# Calculate SST
SST = t(Y) %*% Y - length(Y)*(mean(Y)^2)

# Estimate sigma^2
S2 = SSE/(length(Y)-length(beta.hat))
S2
length(beta.hat)
length(Y)
# Fit the model
fit <- lm(Y ~ X)

# Display the fitted model
Z = summary(fit)
Z

# Display SSR and SSE
anova(fit)


# Calculate confidence intervals for B0 B1 B2 B3
alpha = 0.05
n = length(Y)
n
p = length(beta.hat)
p
t_crit = qt(1-alpha/2, n-p)
t_crit
1-alpha/2
low_b0  = beta.hat[1,1] - t_crit*sqrt(S2*XTXI[1,1])
high_b0  = beta.hat[1,1] + t_crit*sqrt(S2*XTXI[1,1])
c(low_b0,high_b0)

low_b1  = beta.hat[2,1] - t_crit*sqrt(S2*XTXI[2,2])
high_b1  = beta.hat[2,1] + t_crit*sqrt(S2*XTXI[2,2])
c(low_b1,high_b1)

low_b2  = beta.hat[3,1] - t_crit*sqrt(S2*XTXI[3,3])
high_b2  = beta.hat[3,1] + t_crit*sqrt(S2*XTXI[3,3])
c(low_b2,high_b2)

low_b3  = beta.hat[4,1] - t_crit*sqrt(S2*XTXI[4,4])
high_b3  = beta.hat[4,1] + t_crit*sqrt(S2*XTXI[4,4])
c(low_b3,high_b3)

# Q1e
SYY =(t(Y)%*%Y)-n*((mean(Y))^2) 
SYY
R2 = SSR/SYY
R2

# Q1g
# Define a new model without Books
data2 <- c(1, 8.5, 5.1,
           1, 12.9, 5.8,
           1, 5.2, 2.1,
           1, 10.7, 8.4,
           1, 3.1, 2.9,
           1, 3.5, 1.2,
           1, 9.2, 3.7,
           1, 9, 7.6,
           1, 15.1, 7.7,
           1, 10.2, 4.5)

# Define X
X.new <- matrix(data2, ncol = 3, byrow = TRUE)
X.new
# Fit the model
fit2 <- lm(Y ~ X.new)

# Display the fitted model
Z2 = summary(fit2)
Z2

# Display SSR and SSE
anova(fit2)

# Q1h
anova_result <- anova(fit, fit2)
anova_result
p_value <- anova_result$"Pr(>F)"[2]
p_value
f_ratio <- anova_result$"F"[2]
f_ratio
# Q1i
# Values for the X.books matrix
books_values <- c(14.7, 8.8, 5.1, 12.2, 10.6, 3.5, 9.7, 15.9, 20.8, 7.9)

# Create the matrix with the specified values, making it 10x1
X.books <- matrix(books_values, nrow = 10, ncol = 1)

# Output the matrix to check
print(X.books)

fit3 <- lm(Y ~ X.books)
anova(fit3)
anova_result2 <- anova(fit3)
anova_result2
p_value2 <- anova_result2$"Pr(>F)"[1]
p_value2
f_ratio2 <- anova_result2$"F"[1]
f_ratio2

bookssig = summary(fit3)
bookssig


