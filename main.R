library(dplyr)

# Create a ParticulateMatter object using the project data source
# parameters.
pm <- ParticulateMatter(
    source_host="https://d396qusza40orc.cloudfront.net",
    source_path="exdata/data/NEI_data.zip")

# Read the requested data into data.frames
NEI <- pm$getNEI()
SCC <- pm$getSCC()

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


pm$plot2png("question1.png", question1)
