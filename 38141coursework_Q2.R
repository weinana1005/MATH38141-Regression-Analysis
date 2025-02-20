# Q2a
# Provided data
data_X3 <- c(1, 3, 294, 8.2, 8.2, 11,
             1, 2.2, 232, 6.9, 4.1, 12,
             1, 0.5, 149, 3, 4.3, 15,
             1, 5.5, 600, 12, 16.1, 1,
             1, 4.4, 567, 10.6, 14.1, 5,
             1, 4.8, 571, 11.8, 12.7, 4,
             1, 3.1, 512, 8.1, 10.1, 10,
             1, 2.5, 347, 7.7, 8.4, 12,
             1, 1.2, 212, 3.3, 2.1, 15,
             1, 0.6, 102, 4.9, 4.7, 8,
             1, 5.4, 788, 17.4, 12.3, 1,
             1, 4.2, 577, 10.5, 14, 7,
             1, 4.7, 535, 11.3, 15, 3,
             1, 0.6, 163, 2.5, 2.5, 14,
             1, 1.2, 168, 4.7, 3.3, 11,
             1, 1.6, 151, 4.6, 2.7, 10,
             1, 4.3, 342, 5.5, 16, 4,
             1, 2.6, 196, 7.2, 6.3, 13,
             1, 3.8, 453, 10.4, 13.9, 7,
             1, 5.3, 518, 11.5, 16.3, 1,
             1, 5.6, 615, 12.3, 16, 0,
             1, 0.8, 278, 2.8, 6.5, 14,
             1, 1.1, 142, 3.1, 1.6, 12,
             1, 3.6, 461, 9.6, 11.3, 6,
             1, 3.5, 382, 9.8, 11.5, 5,
             1, 5.1, 590, 12, 15.7, 0,
             1, 8.6, 517, 7, 12, 8)

# Create the matrix X3
A <- matrix(data_X3, ncol = 6, byrow = TRUE)
A

# Provided data
data_Y.2 <- c(231, 156, 10, 519, 437, 487, 299, 195, 20, 68, 570, 428, 464, 15, 65, 98, 398, 161, 397, 497, 528, 99, 1, 347, 341, 507, 400)
# Create matrix Y.2
Y.2 <- matrix(data_Y.2, ncol = 1)
Y.2

# Provided data
data_B <- c(1, 294, 
            1, 232, 
            1, 149, 
            1, 600, 
            1, 567, 
            1, 571, 
            1, 512, 
            1, 347, 
            1, 212, 
            1, 102, 
            1, 788, 
            1, 577, 
            1, 535, 
            1, 163, 
            1, 168, 
            1, 151, 
            1, 342, 
            1, 196, 
            1, 453, 
            1, 518, 
            1, 615, 
            1, 278, 
            1, 142, 
            1, 461, 
            1, 382, 
            1, 590, 
            1, 517)

# Create matrix B
B <- matrix(data_B, ncol = 2, byrow = TRUE)
B


# Q2b
n.2 = length(Y.2)
n.2
SYY =(t(Y.2)%*%Y.2)-n.2*((mean(Y.2))^2) 
SYY

# Data for A
AT = t(A)
ATA = t(A)%*%A
ATY.2 = AT %*% Y.2

ATA
ATY.2

ATAI = solve(t(A)%*%A)
ATAI
# Find the least squares estimates of B0 B1 B2 B3 B4 B5
beta.hatA =(ATAI)%*%t(A)%*%Y.2
beta.hatA


Y.2hat = A %*% beta.hatA

e.A = Y.2hat - Y.2

SSR.A = t(beta.hatA) %*% ATY.2 - length(Y.2)*(mean(Y.2)^2)
SSR.A

SSE.A = sum(e.A^2) 
SSE.A

SST.A = t(Y.2) %*% Y.2 - length(Y.2)*(mean(Y.2)^2)

S2_A = SSE.A/(length(Y.2)-length(beta.hatA))
S2_A


# Data for B
BT = t(B)
BTB = t(B)%*%B
BTY.2 = BT %*% Y.2

BTB
BTY.2

BTBI = solve(t(B)%*%B)
BTBI

beta.hatB =(BTBI)%*%t(B)%*%Y.2
beta.hatB


Y.2hatB = B %*% beta.hatB

e.B = Y.2hatB - Y.2

SSR.B = t(beta.hatB) %*% BTY.2 - length(Y.2)*(mean(Y.2)^2)
SSR.B

SSE.B = sum(e.B^2) 
SSE.B

SST.B = t(Y.2) %*% Y.2 - length(Y.2)*(mean(Y.2)^2)

S2_B = SSE.B/(length(Y.2)-length(beta.hatB))
S2_B

# Q2d
# Calculate confidence intervals for B0 B1 B2 B3
alpha.a = 0.1
n.2 = length(Y.2)
n.2
p.A = length(beta.hatA)
p.A
t_critA = qt(1-alpha.a/2, n.2-p.A)
t_critA


low_b3A  = beta.hatA[4,1] - t_critA*sqrt(S2_A*ATAI[4,4])
high_b3A  = beta.hatA[4,1] + t_critA*sqrt(S2_A*ATAI[4,4])
c(low_b3A,high_b3A)

low_b4A  = beta.hatA[5,1] - t_critA*sqrt(S2_A*ATAI[5,5])
high_b4A  = beta.hatA[5,1] + t_critA*sqrt(S2_A*ATAI[5,5])
c(low_b4A,high_b4A)

# Q2e
ANS1 = beta.hatA[1,1] + beta.hatA[2,1]*3 + beta.hatA[3,1]*500 + beta.hatA[4,1]*5 + beta.hatA[5,1]*5 + beta.hatA[6,1]*5
ANS1
ANS2 = beta.hatA[1,1] + beta.hatA[2,1]*5 + beta.hatA[3,1]*500 + beta.hatA[4,1]*10 + beta.hatA[5,1]*10 + beta.hatA[6,1]*10
ANS2

C <- c(1, 3, 500, 5, 5, 5,
       1, 5, 500, 10, 10, 10)
# Create matrix C
C <- matrix(C, ncol = 6, byrow = TRUE)
C

a <- c(1,
       -1)
a <- matrix(a, ncol = 1, byrow = TRUE)
a
t(a)

ac <- c(t(a)%*%C) 
ac

I.hat = ac %*% beta.hatA
I.hat

MSE.A = SSE.A/(27-6)
MSE.A

se.A = sqrt(MSE.A)
se.A

seI.I = se.A*sqrt(ac%*%solve(t(A)%*%A)%*%t(C)%*%a + t(a)%*%a)
seI.I

t_critA2 = qt(1-(0.05/2), 21)
t_critA2


low_I  = I.hat - t_critA2*seI.I
high_I  = I.hat + t_critA2*seI.I
c(low_I,high_I)

# Q2f
data_X4 <- c(1,  294, 11,
             1,  232, 12,
             1,  149, 15,
             1,  600,  1,
             1,  567,  5,
             1,  571,  4,
             1,  512, 10,
             1,  347, 12,
             1,  212, 15,
             1,  102,  8,
             1,  788,  1,
             1,  577,  7,
             1,  535,  3,
             1,  163, 14,
             1,  168, 11,
             1,  151, 10,
             1,  342,  4,
             1,  196, 13,
             1,  453,  7,
             1,  518,  1,
             1,  615,  0,
             1,  278, 14,
             1,  142, 12,
             1,  461,  6,
             1,  382,  5,
             1,  590,  0,
             1,  517,  8)
D <- matrix(data_X4, ncol = 3, byrow = TRUE)
print(D)
fitsmallomega <- lm(data_Y.2 ~ B)
fitbigomega1 <- lm(data_Y.2 ~ D)
summary(fitsmallomega)
summary(fitbigomega1)

anova_result <- anova(fitbigomega1,fitsmallomega)
print(anova_result)
p_value <- anova_result$"Pr(>F)"[2]
print(p_value) # p-value: 3.328534e-06 of the full model in this case

summary(fitsmallomega) #Summary of reduced omega p-value: 1.09e-13

