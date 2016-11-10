df <- data.frame(mother = rep(c("Mother 1", "Mother 2"), each = 20),
                 offspring = rep(c("Male", "Female"), each = 10))
by(df$Y, list(df$mother, df$offspring), mean)

X <- model.matrix(~ offspring, data = df)
b <- rep( rnorm(2), each = 20 )

Y <- rnorm( 40, mean = X%*%c(5,1) + b)
df$Y <- Y

g <- ggplot( data = df, aes(x = mother, y = Y)) +
  xlab(NULL) +
  ylab("Time to skill acquisition") +
  theme_bw(base_size = 16)

g1 <- g + geom_point(size = 3)
g1
g2 <- g + geom_point( aes(colour = offspring), size = 3 ) +
  scale_colour_manual( values = c("blue", "red"))
g2


dfc <- data.frame(m = means, offspring = c("Female", "Male"))

h <- ggplot( data = df, aes(x = "", y = Y)) + 
  xlab(NULL) + 
  ylab("Time to skill acquisition") + 
  theme_bw(base_size = 16)

h1 <- h + geom_jitter(position = position_jitter(width = 0.1), size = 3)
h2 <- h + geom_jitter( aes(colour = offspring), position = position_jitter(width = 0.1), size = 3 ) +
  scale_colour_manual( values = c("blue", "red"))

means <- as.vector(by(df$Y, df$offspring, mean))

h3 <- h2 + annotate("segment", x = 0.5, xend = 1.5, y = means, yend = means,
                    colour = c("blue", "red"), size = 1.5)
h3

h4 <- h + geom_jitter( aes(colour = offspring, shape = mother), position = position_jitter(width = 0.1), size = 3 ) +
  scale_colour_manual( values = c("blue", "red") )

means <- as.vector(by(df$Y, list(df$offspring, df$mother), mean))

h5 <- h4 + annotate("segment", x = 0.5, xend = 1.5, y = means, yend = means,
                    colour = c("blue", "red", "blue", "red"),
                    size = 1.5,
                    linetype = c("solid", "solid", "dashed", "dashed"))



