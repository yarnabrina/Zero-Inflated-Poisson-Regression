######	 Data 	######

library(moments)
library(pscl)
data <-
  within(read.csv("fish.csv"),
         camper <- factor(camper))
attach(data)


######	Descriptive	Analysis	######

n <- length(count)
xi <- sort(unique(count))
fi <- table(count)

am <-
  mean(count)
variance <-
  var(count)
gamma1 <-
  skewness(count)
gamma2 <- kurtosis(count)
print(c(am, variance, gamma1, gamma2))
plot(
  xi,
  fi,
  type = "h",
  lwd = 2,
  xlab = "No. of Fishes Caught",
  ylab = "Frequency",
  main = "Column Diagram of Fishing data"
)


######	Fitting Approaches	######

Observed <- c(fi[1:6], sum(fi[-(1:6)]))


Fit_pois <- fitdistr(count, "poisson")
l_pois <- Fit_pois$estimate
print(l_pois)
pi_exp_pois <-
  dpois(0:5, lambda = l_pois)
pi_exp_pois[7] <- 1 - sum(pi_exp_pois)
Expected <- n * pi_exp_pois
X_pois <-
  chisq.test(Observed, p = pi_exp_pois)$statistic
cp_pois <-
  qchisq(0.01, 7 - 1 - 1, lower.tail = FALSE)
print(c(X_pois, cp_pois))
barplot(
  rbind(Observed, Expected),
  beside = TRUE,
  names = c(0:5, "6 or more"),
  col = c("black", "grey"),
  legend = TRUE
)
title(xlab = "No. of Fishes Caught", ylab = "Frequency", main = "Poisson Fitting")

NB_fit <- fitdistr(count, "negative binomial")
r_nb <-
  NB_fit$estimate[1]
mu_nb <- NB_fit$estimate[2]
print(c(r_nb, mu_nb))
pi_exp_nb <-
  dnbinom(0:5, size = r_nb, mu = mu_nb)
pi_exp_nb[7] <- 1 - sum(pi_exp_nb)
Expected <- n * pi_exp_nb
X_nb <-
  chisq.test(Observed, p = pi_exp_nb)$statistic
cp_nb <- qchisq(0.01, 7 - 1 - 2, lower.tail = FALSE)
print(c(X_nb, cp_nb))
barplot(
  rbind(Observed, Expected),
  beside = TRUE,
  names = c(0:5, "6 or more"),
  col = c("black", "grey"),
  legend = TRUE
)
title(xlab = "No. of Fishes Caught", ylab = "Frequency", main = "Negative Binomial Fitting")

zip <-
  function(x, p, l) {
    ifelse(x == 0, (p + ((1 - p) * exp(-l))), ((1 - p) * exp(-l) * (l ^ x) /
                                                 factorial(x)))
  }
L <- c(5, 10, 25, 50)
P <- c(0.2, 0.4, 0.6, 0.8)
n <- 1000
k <- 500
l <- c()
p <- c()
l_mme_mean <- c()
l_mle_mean <- c()
p_mme_mean <- c()
p_mle_mean <- c()
l_mme_mse <- c()
l_mle_mse <- c()
p_mme_mse <- c()
p_mle_mse <- c()
for (i in 1:length(L))
{
  for (j in 1:length(P))
  {
    q <- (i - 1) * length(P) + j
    l[q] <- L[i]
    p[q] <- P[j]
    p_mme <- c()
    l_mme <- c()
    p_mle <- c()
    l_mle <- c()
    for (s in 1:k)
    {
      obs <- c()
      for (t in 1:n)
      {
        r = runif(1)
        ifelse(r < p[q], obs <-
                 c(obs, 0), obs <- c(obs, rpois(1, l[q])))
      }
      am <- mean(obs)
      var <- var(obs)
      l_mme[s] <- (var / am) - 1 + am
      p_mme[s] <- 1 - (am / l_mme[s])
      fit <-
        fitdistr(obs, zip, list(p = p_mme[s], l = l_mme[s]))
      p_mle[s] <- fit$estimate[1]
      l_mle[s] <- fit$estimate[2]
    }
    p_mme_mean[q] <-
      mean(p_mme)
    p_mle_mean[q] <-
      mean(p_mle)
    l_mme_mean[q] <- mean(l_mme)
    l_mle_mean[q] <- mean(l_mle)
    p_mme_mse[q] <-
      mean((p_mme - p[q]) ^ 2)
    p_mle_mse[q] <-
      mean((p_mle - p[q]) ^ 2)
    l_mme_mse[q] <-
      mean((l_mme - l[q]) ^ 2)
    l_mle_mse[q] <- mean((l_mle - l[q]) ^ 2)
  }
}
Result <-
  data.frame(
    l,
    p,
    l_mme_mean,
    l_mle_mean,
    p_mme_mean,
    p_mle_mean,
    l_mme_mse,
    l_mle_mse,
    p_mme_mse,
    p_mle_mse
  )
print(Result)
ZIP_fit <- fitdistr(count, zip, start = list(p = 0.5, l = 5))
p_zip <-
  ZIP_fit$estimate[1]
l_zip <- ZIP_fit$estimate[2]
print(c(p_zip, l_zip))
pi_exp_zip <-
  zip(0:5, p = p_zip, l = l_zip)
pi_exp_zip[7] <- 1 - sum(pi_exp_zip)
Expected <- n * pi_exp_zip
X_zip <-
  chisq.test(Observed, p = pi_exp_zip)$statistic
cp_zip <- qchisq(0.01, 7 - 1 - 2, lower.tail = FALSE)
print(c(X_zip, cp_zip))
barplot(
  rbind(Observed, Expected),
  beside = TRUE,
  names = c(0:5, "6 or more"),
  col = c("black", "grey"),
  legend = TRUE
)
title(xlab = "No. of Fishes Caught", ylab = "Frequency", main = "Zero Inflated Poisson Fitting")


######	Regression Models		######

NB_reg <- glm.nb(count ~ -1 + persons + child + camper)

ZIP_reg <-
  zeroinfl(count ~ -1 + persons + child + camper, dist = "poisson")

summary(NB_reg)
