wall <- read.table("wallaby.dat",header=TRUE) ## Read in the wallaby data
wall[616,"Age"] <- 78 ## Fix a clear typo
wall <- subset(wall,Age>365&Age<730&Ear>200)
wallsub <- subset(wall,Age>365&Weight>20000)

## Show the shrinkage effect

library(ggplot2)
dat <- wallsub[wallsub$Anim<=100,] ## Wallabies with Anim > 100 have very few data points

dat$Age0 <- dat$Age - 365 ## Scale the age variable to fix a convergence error in mixed model
LME <- lmer(Weight~Age0 + (Age0|Anim), data = dat)

r.effs <- ranef(LME)$Anim ## Extract the random effects
f.effs <- fixef(LME) ## Extract the fixed effects

RE <- data.frame( t(f.effs + t(r.effs)) )
colnames(RE) <- c("Intercept", "Slope")
RE$Anim <- as.integer(rownames(RE))

plotAnim <- function(anim) {
  g <- ggplot(data = dat[dat$Anim==anim,], aes(x = Age0, y = Weight))
  g <- g + scale_y_continuous(limits = c(min(RE$Intercept), 60000)) + geom_point(size=3) + geom_smooth(method = "lm", size = 1, colour = "black", se = FALSE) +
    geom_abline(data = RE[RE$Anim==anim,], aes(intercept = Intercept, slope = Slope), size = 1, linetype = "dashed", colour = "blue") +
    geom_abline(aes(intercept = f.effs[1], slope = f.effs[2]), size = 1, linetype = "dotted", colour = gray(0.5) ) +
    ggtitle(paste("Wallaby", anim)) +
    theme( title = element_text(size = rel(1.3)))
  return(g)
}

plotAnim(79)
plotAnim(93)
plotAnim(65)


