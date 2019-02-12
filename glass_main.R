#upload Glass data set

library(mlbench)
data("Glass")
N = dim(Glass)[1] #size of sample

#randomly re-order data 
set.seed(1)
order = sample(seq(1, N))
X = Glass[order, !names(Glass) %in% c("Type")]
y = as.numeric(Glass[order, names(Glass) %in% c("Type")])

#scaling to -1 to 1
X.min <- apply(X, 2, min)
X.max <- apply(X, 2, max)
X.min <- matrix(rep(X.min, N), nrow = N, byrow = T)
X.max <- matrix(rep(X.max, N), nrow = N, byrow = T)
a0 = -1
b0 = 1
X = (X - X.min) / (X.max - X.min) * (b0-a0) + a0
X = as.matrix(X)
Xones = cbind(rep(1, dim(X)[1]), X) #add bias

D = dim(X)[2]  #dimensionality
K =  max(y) #number of classes

outcomes = matrix(0, ncol = K, nrow = N)
for (i in 1:N) {
  outcomes[i, y[i]] = 1
}

#train multinom on the whole data
df = data.frame(cbind(y, X))
target_name = names(df)[1]
features = names(df)[2:ncol(df)]
formula = as.formula(paste(target_name, paste(features, collapse=" + "), sep=" ~ "))

library(nnet)
mult = multinom(formula, df)
params.mult = coef(mult)
probs.mult = predict(mult, type="probs", newdata=X)
loss_mult <- -log(apply(probs.mult * outcomes, 1, sum))
Loss_mult <- cumsum(loss_mult)
max(Loss_mult)
sum(params.mult^2)
y.mult = rep(0, N)
for (i in 1:N) {
  y.mult[i] = which.max(probs.mult[i, ])
}
correct.mult = sum(y.mult == y) / N

#params
M <- 3000
M0 <- 1000 #burn-in

#MCMC on test data set
AA1 = mcmc_online(outcomes, X, M, M0, a = 0.001, sigma = 0.1)
loss_AA1 <- -log(apply(AA1$gamma * outcomes, 1, sum))
Loss_AA1 <- cumsum(loss_AA1)
max(Loss_AA1)

AA2 = mcmc_online(outcomes, X, M, M0, a = 0.01, sigma = 0.1)
loss_AA2 <- -log(apply(AA2$gamma * outcomes, 1, sum))
Loss_AA2 <- cumsum(loss_AA2)
max(Loss_AA2)

time = seq(0, dim(X)[1])
a1 = 0.001
a2 = 0.01
bound1 = -(a1*sum(params.mult^2) + K*(D-1)/2*log(1+(D-1)/(8*a1)*time))
bound2 = -(a2*sum(params.mult^2) + K*(D-1)/2*log(1+(D-1)/(8*a2)*time))

pdf("glass_Loss_diff_a001_s01.pdf", height = 8.5, width = 8.5, paper = "special")
plot(time, c(0, Loss_mult - Loss_AA1), ylim = c(min(bound1), 0), lwd=3,cex=2,cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, type = "l", xlab = "Time", ylab = "", main= "Loss difference")
lines(time, bound1, lwd=3,cex=2)
dev.off()

pdf("glass_Loss_diff_a01_s01.pdf", height = 8.5, width = 8.5, paper = "special")
plot(time, c(0, Loss_mult - Loss_AA2), ylim = c(min(bound2), 0), lwd=3,cex=2,cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, type = "l", xlab = "Time", ylab = "", main= "Loss difference")
lines(time, bound2, lwd=3,cex=2)
dev.off()

