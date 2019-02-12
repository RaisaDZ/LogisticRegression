#upload Glass data set

D = 2  #dimensionality
K = 4  #number of classes
N = 1000  #size of sample

library(mlbench)
smiley = mlbench.smiley(n=N, sd1 = 0.1, sd2 = 0.05)

dt = data.frame(cbind(smiley$x, smiley$classes))
names(dt) = c("x1", "x2", "y")
plot(dt$x1, dt$x2, col = dt$y)

medians = aggregate(dt[, !names(dt) %in% c("y")], list(dt$y), median)
dt = merge(dt, medians, by.x = c("y"), by.y = ("Group.1"))

dt$flag_train = ifelse((dt$y == 1)&(dt$x1.x <= dt$x1.y), 1, 0)
dt$flag_train = ifelse((dt$y == 2)&(dt$x1.x > dt$x1.y), 1, dt$flag_train)
dt$flag_train = ifelse((dt$y == 3)&(dt$x2.x < dt$x2.y), 1, dt$flag_train)
dt$flag_train = ifelse((dt$y == 4)&(dt$x2.x < dt$x2.y), 1, dt$flag_train)

dt.train = dt[dt$flag_train == 1, ]
plot(dt.train$x1.x, dt.train$x2.x, col = dt.train$y)

pdf("smiley_data.pdf", height = 8.5, width = 8.5, paper = "special")
plot(dt$x1.x, dt$x2.x, type = "p", cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, xlab = "", ylab = "", main= "")
dev.off()

pdf("smiley_train.pdf", height = 8.5, width = 8.5, paper = "special")
plot(dt.train$x1.x, dt.train$x2.x, type = "p", cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, xlab = "", ylab = "", main= "")
dev.off()

set.seed(1)
order = sample(seq(1,N))
dt = dt[order, ]

X = as.matrix(dt[, c("x1.x", "x2.x")])
y = dt[, c("y")]

outcomes = matrix(0, ncol = K, nrow = N)
for (i in 1:N) {
  outcomes[i, y[i]] = 1
}

#train and test
X.train = as.matrix(dt[dt$flag_train == 1, c("x1.x", "x2.x")])
y.train = as.matrix(dt[dt$flag_train == 1, c("y")])
N.train = length(y.train)
X.test = as.matrix(dt[dt$flag_train == 0, c("x1.x", "x2.x")])
y.test = as.matrix(dt[dt$flag_train == 0, c("y")])

outcomes.train = matrix(0, ncol = K, nrow = N.train)
for (i in 1:N.train) {
  outcomes.train[i, y.train[i]] = 1
}

outcomes.test = matrix(0, ncol = K, nrow = (N-N.train))
for (i in 1:(N-N.train)) {
  outcomes.test[i, y.test[i]] = 1
}

Xones.test = cbind(rep(1, dim(X.test)[1]), X.test)

#params
M <- 3000
M0 <- 1000 #burn-in

#MCMC on test data set
AA = mcmc_online(outcomes.test, X.test, M, M0, a = 0.001, sigma = 1.5)
loss_AA <- -log(apply(AA$gamma * outcomes.test, 1, sum))
Loss_AA <- cumsum(loss_AA)
max(Loss_AA)

#train multinom on the train data
df.train = data.frame(cbind(y.train, X.train))
target_name = names(df.train)[1]
features = names(df.train)[2:ncol(df.train)]
formula = as.formula(paste(target_name, paste(features, collapse=" + "), sep=" ~ "))

library(nnet)
mult = multinom(formula, df.train, maxit = 10000)
params.mult = coef(mult)
probs.mult = predict(mult, type="probs", newdata=X.test)
loss_mult <- -log(apply(probs.mult * outcomes.test, 1, sum))
Loss_mult <- cumsum(loss_mult)
max(Loss_mult)
sum(params.mult^2)
y.mult = rep(0, nrow(X.test))
for (i in 1:nrow(X.test)) {
  y.mult[i] = which.max(probs.mult[i, ])
}
correct.mult = sum(y.mult == y.test) / nrow(X.test)

#multinom on test data
df.test = data.frame(cbind(y.test, X.test))
mult2 = multinom(formula, df.test, maxit = 10000)
params.mult2 = coef(mult2)
probs.mult2 = predict(mult2, type="probs", newdata=X.test)
loss_mult2 <- -log(apply(probs.mult2 * outcomes.test, 1, sum))
Loss_mult2 <- cumsum(loss_mult2)
max(Loss_mult2)
sum(params.mult2^2)
y.mult2 = rep(0, nrow(X.test))
for (i in 1:nrow(X.test)) {
  y.mult2[i] = which.max(probs.mult2[i, ])
}
correct.mult2 = sum(y.mult2 == y.test) / nrow(X.test)

probs.mult3 = matrix(0, ncol = K, nrow = dim(X.test)[1])
probs.mult3[1, ] = predict(mult, type="probs", newdata=X.test[1, , drop = FALSE])
#nn online on test data 
for (i in 1:(dim(X.test)[1]-1)) {
  y.temp = rbind(y.train, y.test[1:i, , drop = FALSE])
  X.temp = rbind(X.train, X.test[1:i, ])
  df.temp = data.frame(cbind(y.temp, X.temp))
  mult3 = multinom(formula, df.temp, maxit = 10000)
  probs.mult3[i+1, ] = predict(mult3, type="probs", newdata=X.test[i+1, , drop = FALSE])
}

loss_mult3 <- -log(apply(probs.mult3 * outcomes.test, 1, sum))
Loss_mult3 <- cumsum(loss_mult3)
max(Loss_mult3)

time = seq(0, dim(X.test)[1])
a = 0.001
bound = -(a*sum(params.mult2^2) + K*(D-1)/2*log(1+(D-1)/(8*a)*time))
pdf("smiley_Loss_diff_test.pdf", height = 8.5, width = 8.5, paper = "special")
plot(time, c(0, Loss_mult2 - Loss_AA), ylim = c(min(bound), 0), lwd=3,cex=2,cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, type = "l", xlab = "Time", ylab = "", main= "Loss difference")
lines(time, bound, lwd=3,cex=2)
dev.off()

pdf("smiley_Loss_diff_train.pdf", height = 8.5, width = 8.5, paper = "special")
plot(Loss_mult - Loss_AA, lwd=3,cex=2,cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, type = "l", xlab = "Time", ylab = "", main= "Loss difference")
dev.off()

pdf("smiley_Loss_diff_online.pdf", height = 8.5, width = 8.5, paper = "special")
plot(Loss_mult3 - Loss_AA, lwd=3,cex=2,cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, type = "l", xlab = "Time", ylab = "", main= "Loss difference")
lines(rep(0, dim(X.test)[1]), lwd=3,cex=2)
dev.off()

#check on train data set
AA1 = mcmc_batch_new(outcomes.train, X.train, M, M0, a = 0.001, sigma = 0.1)
AA2 = mcmc_batch_new(outcomes.train, X.train, M, M0, a = 0.001, sigma = 1.5)

pdf("logLikelihood.pdf", height = 8.5, width = 8.5, paper = "special")
par(mfrow=c(2,1))
plot(AA1$lik, lwd=3,cex=2,cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, type = "l", xlab = "Iteration", ylab = "", main = "sigma = 0.1")
plot(AA2$lik, lwd=3,cex=2,cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, type = "l", xlab = "Iteration", ylab = "", main = "sigma = 1.5")
dev.off()


