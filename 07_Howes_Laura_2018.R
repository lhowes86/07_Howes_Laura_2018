
#' Homework week 07
#' 
#' @Date 2018-10-26
#' 
#' @author Laura Howes
#' -----------------------------------------------------------------------

library(MASS)
library(modelr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)


#---
# Problem 1
#---

####Grid Sampling! Based on Friday's lab, load up the pufferfish data and use grid sampling to find the MLE of the slope, intercept and residual SD of this model. Feel free to eyeball results from an lm() fit to get reasonable values. Try not to do this for a grid of more than ~100K points (more if you want!). It's ok to be coarse. Compare to lm.

setwd("C:/Users/Laura/Dropbox/Grad school/BIOL 607 Biostats/Homework/data")
getwd()

puffer_fish <- read_csv ("./16q11PufferfishMimicry Caley & Schluter 2003.csv")

head(puffer_fish)

#looking at a lm() fit:

puffer_fish_lm <- lm(predators ~ resemblance, data = puffer_fish)

summary(puffer_fish_lm)

#make a function

lik_fun <- function(slope, intercept, resid_sd){
  
  pred_fit <- intercept + slope * puffer_fish$resemblance
  
  #likelihood
  sum(dnorm(puffer_fish$predators, pred_fit, resid_sd, log=TRUE))
}

#grid sample

puffer_grid <- crossing(slope = seq(2.5, 3.5, 0.05), 
                        intercept = seq(0.5, 4, 0.05),
                        resid_sd = seq(2.9, 3.1, 0.01)) %>% 
  rowwise() %>% 
  mutate(logl = lik_fun(slope, intercept, resid_sd)) %>%
  ungroup()

#MLE
puffer_grid %>% filter(logl == max(logl))



#---
# Problem 2
#---

####Surfaces! Filter the dataset to the MLE of the SD. Plot the surface for the slope and intercept in whatever way you find most compelling. You might want to play around with zooming in to different regions, etc. Have fun!

MLE_SD <- puffer_grid %>% filter(resid_sd == max(resid_sd))

MLE_SD_plot <- ggplot(data = MLE_SD, 
                      mapping = aes(x = slope, y = intercept, z = exp(logl))) +
  geom_contour(aes(color = ..level..))

MLE_SD_plot

#---
# Problem 3
#---

####GLM! Now, compare those results to results from glm. Show the profiles and confidence intervals from glm() for the slope and intercept.

#glm
puffer_glm <- glm(predators ~ resemblance,
                 family = gaussian(link = "identity"), data = puffer_fish)

#profile                 
puff_glm_prof <- profile(puffer_glm)

plot(puff_glm_prof)
#is a straight line

library(profileModel)
puffer_prof_mod <- profileModel(puffer_glm,
                              objective = "ordinaryDeviance")

plot(puffer_prof_mod)

#Confidence intervals
confint(puffer_glm)

#---
# Problem 4
#---

####Get Outside of GLM! So, often, we have more complex models than the above. There are a variety of optimizers out there, and packages for accessing them. One of the best is bbmle by Ecologist Ben Bolker (whose dad is emeritus at UMB in computer science! Go visit him! He's fantastic!)

####Load up 'bbmle and try out mle2. It's a bit different, in that the first argument is a function that minimizes the log likelihood (not maximizes). The second argument is a list of start values - e.g. list(slope = 2, intercept = 5, resid_sd = 2). Try and fit your model with mle2 using start values close to the actual estimates. Look at the summary and plot the profile. Note, you might get a lot of errors because it will try impossible values of your residual SD. Also, note that you'll have to rewrite your likelihood function to return the negative log likelihood (or write a wrapper that does so). A small thing

library(bbmle)

#rewrite your likelihood function to return negative log likelihood 
neg_lik_fun <-  function(slope, intercept, resid_sd) -1*lik_fun(slope, intercept, resid_sd)

summary(puffer_glm)

#fit your model with mle2 using start values close to the actual estimates
puff_mle2 <- mle2(neg_lik_fun, start = list(slope = 2, intercept = 1, resid_sd = 3))

#look at summary and plot the profile 

summary(puff_mle2)

puff_mod2 <- profile(puff_mle2)

plot(puff_mod2)

#---
# Problem 5
#---

####Start values! What happens if you start with start values very far away from the initial values. Failing here is fine. But what do you think is happening, and what does this say about the value of start values?

far_start <- mle2(neg_lik_fun, start = list(slope = 245, intercept = -17, resid_sd = 300))

#look at summary and plot the profile

summary(far_start)

puff_mod_far <- profile(far_start)

plot(puff_mod_far)

#####I'm not sure, the plots look the same to me? It's hard to see the differences, but there are some slight ones. I think my far_start values aren't good. 

#---
# Problem 6
#---

####Algorithms! By default, mle2 uses the Nelder-Mead algorithm via the optim function. What happens if you add an method argument to "SANN" or "L-BFGS-B" (and for the later, which is bounded sampling, give it a lower argument for your residual value, so it's always positive). See  ?optim for some more guidance. Do these both converge to the same value? Based on their profiles, do you trust them? (Note, Simulated annealing takes a looooong time. Go have a cuppa while the profile for that one runs).

SANN_puffer <- mle2(neg_lik_fun, start = list(slope = 2, intercept = 1, resid_sd = 3),
                    method = "SANN")

summary(SANN_puffer)

SANN_puff_mod <- profile(SANN_puffer)

plot(SANN_puff_mod)

BFGS_puffer <- mle2(neg_lik_fun, start = list(slope = 2, intercept = 1, resid_sd = 3),
                    method = "L-BFGS-B")
               
summary(BFGS_puffer)

BFGS_puff_mod <- profile(BFGS_puffer)

plot(BFGS_puff_mod)

?optim

#####They have good profiles - I trust them.




