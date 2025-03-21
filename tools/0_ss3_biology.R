# load library
library(here)
library(tidyverse)

# function to fit a cumulative normal distribution to get transition probabilities
fnormcum <- function(parms, ages, propmale) 
{
  mu <- parms[1]
  sigma <- parms[2]
  asym <- parms[3]
  
  val.pred <- pnorm(q = ages, mean = mu, sd = sigma) * asym
  
  NM <- rep(0, length(val.pred))
  NF <- rep(1, length(NM))
  for (i in 2:length(NM)) {
    NM[i] <- NM[i - 1] + NF[i - 1] * val.pred[i - 1]
    NF[i] <- NF[i - 1] * (1 - val.pred[i - 1])
  }
  pM <- NM / (NM + NF)
  ss <- sum((pM - propmale)^2)
  ss
}


dat <- read.csv(here("scenarios", "slp_sex.csv"))

plus_group = 6
dat2 <- dat |>
  mutate(age = case_when(age < plus_group ~ age,
                         TRUE ~ plus_group)) |>
  group_by(age) |>
  summarize(
    n = sum (n),
    n.female = sum(n.female),
    n.male = sum(n.male),
  ) |>
  mutate(
    prop.female = n.female/n,
    prop.male = n.male/n
  )

ages3 <- dat$age[dat$age > plus_group]
dat3 <- dat2  |> 
  filter(age == plus_group) |>
  select(-age) |>
  uncount(length(ages3)) |>
  mutate(age = ages3) |>
  rbind(dat2)


age.ob <- dat3$age
propm.ob <- dat3$prop.male

low <- c(0, 0, 0) # lower bounds on estimated parms (mu, sigma, asym)
up <- c(6, 100, 1) # upper bounds on estimated parms (mu, sigma, asym)
fit.prop <- nlminb(c(0.1, 0.1, 0.1), objective = fnormcum, ages = age.ob, propmale = propm.ob, lower = low, upper = up)
mu <- fit.prop$par[1]; mu
sigma <- fit.prop$par[2]; sigma
asym <- fit.prop$par[3]; asym

ages = c(min(dat$age):max(dat$age))

prob.transition <- pnorm(q = ages, mean = mu, sd = sigma) * asym

NM <- rep(0, length(ages))
NF <- rep(1, length(NM))
for (i in 2:length(NM)) {
  NM[i] <- NM[i - 1] + NF[i - 1] * prob.transition[i - 1]
  NF[i] <- NF[i - 1] * (1 - prob.transition[i - 1])
}
pM <- NM / (NM + NF)

plot(ages, prob.transition, ylab = "Proportion male or probability of transition", xlab = "age", type = "l", lwd = 2, ylim = c(0, 1))
grid(col = "lightgray")
lines(ages, pM, col = "blue", lwd = 2)
points(age.ob, propm.ob)

legend("topleft",
       legend = c("Probability of transition", "Predicted proportion", "Observed proportion"),
       pch = c(-1, -1, 1), lty = c(1, 1, -1), col = c(1, 4, 1), bg = "white"
)


age.ob <- dat2$age
propm.ob <- dat2$prop.male

low <- c(0, 0, 0) # lower bounds on estimated parms (mu, sigma, asym)
up <- c(6, 100, 1) # upper bounds on estimated parms (mu, sigma, asym)
fit.prop2 <- nlminb(c(1, 1, 0.05), objective = fnormcum, ages = age.ob, propmale = propm.ob, lower = low, upper = up)
mu2 <- fit.prop2$par[1]; mu
sigma2 <- fit.prop2$par[2]; sigma
asym2 <- fit.prop2$par[3]; asym

prob.transition2 <- pnorm(q = ages, mean = mu2, sd = sigma2) * asym2

NM <- rep(0, length(ages))
NF <- rep(1, length(NM))
for (i in 2:length(NM)) {
  NM[i] <- NM[i - 1] + NF[i - 1] * prob.transition2[i - 1]
  NF[i] <- NF[i - 1] * (1 - prob.transition2[i - 1])
}
pM <- NM / (NM + NF)

plot(ages, prob.transition2, ylab = "Proportion male or probability of transition", xlab = "age", type = "l", lwd = 2, ylim = c(0, 1))
grid(col = "lightgray")
lines(ages, pM, col = "red", lwd = 2)
points(age.ob, propm.ob)

legend("topleft",
       legend = c("Probability of transition", "Predicted proportion", "Observed proportion"),
       pch = c(-1, -1, 1), lty = c(1, 1, -1), col = c(1, 2, 1), bg = "white"
)


mu <- fit.prop$par[1]; mu
sigma <- fit.prop$par[2]; sigma
asym <- fit.prop$par[3]; asym

ages = c(min(dat$age):max(dat$age))

prob.transition <- pnorm(q = ages, mean = mu, sd = sigma) * asym

NM <- rep(0, length(ages))
NF <- rep(1, length(NM))
for (i in 2:length(NM)) {
  NM[i] <- NM[i - 1] + NF[i - 1] * prob.transition[i - 1]
  NF[i] <- NF[i - 1] * (1 - prob.transition[i - 1])
}
pM <- NM / (NM + NF)

plot(ages, prob.transition, ylab = "Proportion male or probability of transition", xlab = "age", type = "l", lwd = 2, ylim = c(0, 1))
grid(col = "lightgray")
lines(ages, pM, col = "blue", lwd = 2)
points(age.ob, propm.ob)

legend("topleft",
       legend = c("Probability of transition", "Predicted proportion", "Observed proportion"),
       pch = c(-1, -1, 1), lty = c(1, 1, -1), col = c(1, 4, 1), bg = "white"
)


age.ob <- dat2$age
propm.ob <- dat2$prop.male

low <- c(0, 0, 0) # lower bounds on estimated parms (mu, sigma, asym)
up <- c(6, 100, 1) # upper bounds on estimated parms (mu, sigma, asym)
fit.prop2 <- nlminb(c(1, 1, 0.05), objective = fnormcum, ages = age.ob, propmale = propm.ob, lower = low, upper = up)
mu2 <- fit.prop2$par[1]; mu
sigma2 <- fit.prop2$par[2]; sigma
asym2 <- fit.prop2$par[3]; asym

prob.transition2 <- pnorm(q = ages, mean = mu2, sd = sigma2) * asym2

NM <- rep(0, length(ages))
NF <- rep(1, length(NM))
for (i in 2:length(NM)) {
  NM[i] <- NM[i - 1] + NF[i - 1] * prob.transition2[i - 1]
  NF[i] <- NF[i - 1] * (1 - prob.transition2[i - 1])
}
pM <- NM / (NM + NF)

plot(ages, prob.transition2, ylab = "Proportion male or probability of transition", xlab = "age", type = "l", lwd = 2, ylim = c(0, 1))
grid(col = "lightgray")
lines(ages, pM, col = "red", lwd = 2)
points(age.ob, propm.ob)

legend("topleft",
       legend = c("Probability of transition", "Predicted proportion", "Observed proportion"),
       pch = c(-1, -1, 1), lty = c(1, 1, -1), col = c(1, 2, 1), bg = "white"
)




ss3.mu <- 0.5
ss3.sigma <- 0.001
ss3.asym <- 0.05

ss3.prob.transition <- pnorm(q = ages, mean = ss3.mu, sd = ss3.sigma) * ss3.asym

NM <- rep(0, length(ages))
NF <- rep(1, length(NM))
for (i in 2:length(NM)) {
  NM[i] <- NM[i - 1] + NF[i - 1] * ss3.prob.transition[i - 1]
  NF[i] <- NF[i - 1] * (1 - ss3.prob.transition[i - 1])
}
pM <- NM / (NM + NF)

png(file = here("documentation", "images", "ss3_herm.png"))
plot(ages, prob.transition2, ylab = "Proportion male or probability of transition", xlab = "age", type = "l", lwd = 2, ylim = c(0, 1))
grid(col = "lightgray")
lines(ages, pM, col = 2, lwd = 2)
points(dat$age, dat$prop.male, col = 2)

legend("topleft",
       legend = c("Probability of transition", "Predicted proportion", "Observed proportion"), bty = "n",
       pch = c(-1, -1, 1), lty = c(1, 1, -1), col = c(1, 2, 2)
)
dev.off()



X <- dat2$age
Y <- dat2$prop.male/(max(dat2$prop.male))

fn <- function(x) {
   mu <- x[1];
   sigma <- exp(x[2])
   sum((Y-pnorm(X,mu,sigma))^2)
}

est <- nlm(fn, c(1,1))$estimate

png(file = here("documentation", "images", "sex_ratio_fit.png"))
plot(dat$age, dat$prop.male, ylab = "Proportion male", xlab = "Age")
points(dat2$age,dat2$prop.male, col = "blue")
curve(pnorm(x, est[1], exp(est[2]))*max(dat3$prop.male), col = "blue", add=T)
grid(col = "lightgray")
legend("topleft",
       legend = c("Obs. Data", 
                  "Obs. with Combined Age 6+", 
                  "Fit"), bty = "n",
       pch = c(1, 1, -1), lty = c(-1, -1, 1), col = c(1, "blue", "blue")
)
dev.off()

fit_age = c(0:20)
fit_dat = data.frame(
  age = fit_age,
  fit.male = round(pnorm(fit_age, est[1], exp(est[2]))*max(dat3$prop.male), 4)
) |> 
  mutate(fit.female = 1-fit.male) |>
  left_join(dat, by = join_by(age))
write.csv(fit_dat, here("tools", "slp_sex_fit.csv"), row.names = FALSE)



est[1]
exp(est[2])
max(dat3$prop.male)

# fecundity = maturity * a * W^b (biomass) * sex ratio
