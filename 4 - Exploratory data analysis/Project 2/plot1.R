library(dplyr)

summary_df <- readRDS("summarySCC_PM25.rds")

## First plot
## Total emissions by year, using the base plotting system.
total_emissions <- summary_df %>% 
  group_by(year) %>% 
  summarise(total=sum(Emissions))

png(filename="plot1.png", width=1000, height=750, pointsize=24)
plot(total_emissions$year, 
     total_emissions$total / 1e6, 
     type="b", 
     xlab="Year", 
     ylab="Total Emissions (million tons)", 
     main="Total PM2.5 Emissions of the US 1999-2008",
     ylim=c(0, 8))
dev.off()
