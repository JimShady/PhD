library(ggplot2)

minutes           <- seq(1:100)
low               <- c(runif(33, 3, 7))
medium            <- c(runif(34, 40, 60))
high              <- c(runif(33, 3, 7))
concentrations    <- c(low, medium, high)

data        <- data.frame(minutes, concentrations)

plot <- ggplot(data, aes(minutes, concentrations)) +
      geom_line() +
      theme(axis.text = element_text(size = 14, colour = "black"),
            axis.title = element_text(size = 14, colour = "black")
            ) +
      annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 6,
              alpha = .2, colour="blue") +
      annotate("rect", xmin = 0, xmax = 100, ymin = 8, ymax = 60,
               alpha = .2, colour="green") +
      geom_hline(aes(yintercept=7), colour="red") +
      xlab("Mintes of journey on tube") +
      ylab("PM2.5 concentation") +
      annotate("text", 50, 20, label = "x 1.82", size = 6) +
      annotate("text", 50, 3, label = "x 0.44", size = 6)

setwd("/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results")

ggsave(plot, file="tube_correction_example.png", width=10, height=10, units = "cm")
