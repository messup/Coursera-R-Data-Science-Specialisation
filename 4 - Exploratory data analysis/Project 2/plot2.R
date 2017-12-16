library(dplyr)

summary_df <- readRDS("summarySCC_PM25.rds")

total_emissions <- summary_df %>%
  filter(fips=="24510") %>%
  group_by(year) %>% 
  summarise(total=sum(Emissions))

png(filename="plot2.png", width=1000, height=750, pointsize=24)
plot(total_emissions$year, 
     total_emissions$total, 
     type="b", 
     xlab="Year", 
     ylab="Total Emissions (tons)", 
     main="Total PM2.5 Emissions of Baltimore City 1999-2008",
     ylim=c(0, 4000))
dev.off()
