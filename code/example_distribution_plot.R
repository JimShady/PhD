library(ggplot2)

samples <- 45000 
mean    <- 15
sd      <- 2.5

exposures <- data.frame(person = 1:45000, pm25 = rnorm(n=samples, mean=mean, sd=sd))

plot <- ggplot(exposures, aes(x=pm25, group=1)) + 
  geom_histogram() +
  xlab(expression(paste("PM"[2.5], 'exposure (', mu, 'g ', m^-3, ")", sep=""))) +
  theme_bw() +
  theme(axis.text = element_text(size = 16, colour = 'black'),
        axis.title = element_text(size = 16, colour = 'black')) +
  annotate('segment', x = mean-sd, xend=mean-sd, y=0, yend=5000, colour='blue') +
  annotate('segment', x = mean+sd, xend=mean+sd, y=0, yend=5000, colour='blue')

ggsave(plot,file = "/home/james/mounts/James/PhD/11 - Evaluation Chapter/outputs/theoretical_lhem_pm25.png",
       width = 10,
       height = 10, units = "cm")


population_data_sd    <- sd(exposures$pm25)
population_size       <- samples
X                     <- 1.96^2 * population_data_sd^2 / (mean*0.1/2)^2;
result               <- c(1.96, mean*0.1, X)

plot <- ggplot(exposures, aes(x=pm25, group=1)) + 
  geom_histogram() +
  xlab(expression(paste("PM"[2.5], 'exposure (', mu, 'g ', m^-3, ")", sep=""))) +
  theme_bw() +
  theme(axis.text = element_text(size = 16, colour = 'black'),
        axis.title = element_text(size = 16, colour = 'black')) +
  annotate('segment', x = mean-sd, xend=mean-sd, y=0, yend=5000, colour='blue') +
  annotate('segment', x = mean+sd, xend=mean+sd, y=0, yend=5000, colour='blue') +
  annotate('rect',  xmin = mean-mean*0.05, xmax=mean+mean*0.05,
                    ymin=0, ymax=5000, fill='red',
           alpha=0.4)

ggsave(plot,file = "/home/james/mounts/James/PhD/11 - Evaluation Chapter/outputs/theoretical_lhem_pm25_samples.png",
       width = 10,
       height = 10, units = "cm")

