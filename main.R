library(dplyr)
library(ggplot2)

# Create a ParticulateMatter object using the project data source
# parameters.
pm <- ParticulateMatter(
    source_host="https://d396qusza40orc.cloudfront.net",
    source_path="exdata/data/NEI_data.zip")

# Read the requested data into data.frames
NEI <- pm$getNEI()
SCC <- pm$getSCC()


# Convenience wrapper to output a plotting function to a PNG file.
plot2png <- function(filename, plotfn) {
    png(filename, width=480, height=480)
    plotfn()
    dev.off()
}

# Question 1:
# Using the base plotting system, make a plot showing the
# total PM2.5 emission from all sources
# for each of the years 1999, 2002, 2005, and 2008.
question1 <- function() {
    NEI %>% group_by(year) %>% summarize(TotalEmissions=sum(Emissions)) -> q1
    plot(q1$year, q1$TotalEmissions, t="l",
        xlab="Emission Year",
        ylab="Total PM2.5 Emissions",
        main="Total PM2.5 Emission from All Sources")
}
plot2png("question1.png", question1)


question2 <- function() {
    NEI %>% filter(fips == "24510") %>% group_by(year) %>%
        summarize(TotalEmissions=sum(Emissions)) -> q2
    plot(q2$year, q2$TotalEmissions, t="l",
         xlab="Emission Year",
         ylab="Total PM2.5 Emissions",
         main="Total PM2.5 Emission from All Sources\nBarlimore City")
}

plot2png("question2.png", question2)


question3 <- function() {
    NEI %>% filter(fips=="24510") %>% group_by(year, type) %>%
        summarise(TotalEmissions=sum(Emissions)) -> q3
    qplot(year, TotalEmissions, data=q3, color=type,
          facets= . ~ type) + geom_line()
    ggsave(file="question3.png", width=4, height=4, scale=0.5, dpi=150)
}
question3()
