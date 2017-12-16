library(dplyr)
library(ggplot2)

summary_df <- readRDS("summarySCC_PM25.rds")

## Third plot
## Emissions by year for Baltimore City, grouped by type, using the ggplot2 plotting system.
total_emissions <- summary_df %>%
  filter(fips=="24510") %>%
  group_by(year, type) %>% 
  summarise(total=sum(Emissions))

qplot(data=total_emissions,
      x=year,
      y=total,
      col=type,
      geom="line",
      xlab="Year", 
      ylab="Total Emissions (tons)", 
      main="Total PM2.5 Emissions of Baltimore City 1999-2008")
ggsave("plot3.png", width = 8, height = 5)
