library("RPostgreSQL")
library("ggplot2")
library("scales")
library("reshape")
library("scales")
library("ggmap")
library("sp")
library("maptools")
library("RColorBrewer")
library("rgdal")
library("gridExtra")

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="james_traffic", user="james", password="XXXXX", host="localhost")

locations <- dbGetQuery(con, paste("
SELECT	  person.ppid,
          st_x(st_setsrid(st_makepoint(hhose::numeric, hhosn::numeric),27700)) AS x,
          st_y(st_setsrid(st_makepoint(hhose::numeric, hhosn::numeric),27700)) AS y
FROM      person
LEFT JOIN household
ON        person.phid = household.hhid
WHERE	  ppiwt::numeric > 0 AND bad_flag IS NULL
"))

## Set a working directory to output the graphs too
setwd("/home/james/mounts/James/PhD/9 - Dynamic Comparison Chapter/Outputs/geographical_missclassification/")

## import the missclassification data I've already ran
load("~/mounts/James/PhD/9 - Dynamic Comparison Chapter/Outputs/address_point_v_lhem/results.Rda")

## now need to link the two on PPID
new_results <- merge(locations, results, by='ppid')

rm(results)
rm(locations)

new_results$missclassification_percentage_pm25 <- 100 * ((new_results$lhem_pm25 - new_results$household_pm25) / new_results$household_pm25)
new_results$missclassification_percentage_no2 <- 100 * ((new_results$lhem_no2 - new_results$household_no2) / new_results$household_no2)

london <- readOGR(dsn = ".", layer = "london")
london <- fortify(london, region="name")

## First make a cumulative distribution plot of the missclassification percentages

plot1 <- ggplot(new_results, aes(missclassification_percentage_pm25)) + stat_ecdf(size=2, colour="red") +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.line = element_line(colour="black"),
        axis.text=element_text(size=20, color="black"),
        axis.title=element_text(size=20, color="black"),
        plot.title=element_text(size=20, colour="black"),
        legend.text=element_text(size=20, colour = "black"),
        legend.title=element_blank(),
        legend.justification=c(1,1),
        legend.position=c(1,1)) +
  labs(#title=expression(paste("Mean daily exposure to PM"[2.5], " (", mu, "g m" ^ "-3", "), (LHEM v. Address-point)")),
    x=expression(paste("PM"[2.5], " Missclassification %")), 
    y="Cumulative percentage of subjects")

plot2 <- ggplot(new_results, aes(missclassification_percentage_pm25)) + stat_ecdf(size=2, colour="blue") +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.line = element_line(colour="black"),
        axis.text=element_text(size=20, color="black"),
        axis.title=element_text(size=20, color="black"),
        plot.title=element_text(size=20, colour="black"),
        legend.text=element_text(size=20, colour = "black"),
        legend.title=element_blank(),
        legend.justification=c(1,1),
        legend.position=c(1,1)) +
  labs(#title=expression(paste("Mean daily exposure to PM"[2.5], " (", mu, "g m" ^ "-3", "), (LHEM v. Address-point)")),
    x=expression(paste("NO"[2], " missclassification %")), 
    y="")

pdf("cumulative_missclass_dist.pdf", width=14, height=6.4)
pushViewport(viewport(layout = grid.layout(1, 2)))
print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

## Now a map of the people with an increase

plot1 <- ggplot(data = new_results[new_results$missclassification_percentage_no2 > 0, ], aes(x = x, y = y)) +
      geom_polygon(data = london, aes(x = long, y = lat, group = group), fill="grey", color = "black") +
  geom_point(size=2.5, colour="red") +
      theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title =  element_text(size=20, color="black"),
        legend.title = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="", x=expression(paste("PM"[2.5])), y="")

plot2 <- ggplot(data = new_results[new_results$missclassification_percentage_pm25 > 0, ], aes(x = x, y = y)) +
  geom_polygon(data = london, aes(x = long, y = lat, group = group), fill="grey", color = "black") +
  geom_point(size=2.5, colour="blue") +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title =  element_text(size=20, color="black"),
        legend.title = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="", x=expression(paste("NO"[2])), y="")

pdf("address_lhem_increases.pdf", width=14, height=6.4)
pushViewport(viewport(layout = grid.layout(1, 2)))
print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()