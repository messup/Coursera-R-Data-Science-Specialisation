library(dplyr)
library(ggplot2)

## Sixth plot
## Emissions from motor vehicles in Baltimore City and Los Angeles

summary_df <- readRDS("summarySCC_PM25.rds")
source_df = readRDS("Source_Classification_Code.rds")

sources <- source_df %>%
  filter(Data.Category=="Onroad") %>%
  select(SCC)

selections <- as.numeric(sources$SCC)
sources <- as.character(levels(sources$SCC))[selections]

total_emissions <- summary_df %>%
  filter(fips=="24510" | fips=="06037") %>%
  filter(SCC %in% sources) %>%
  group_by(year, fips) %>% 
  summarise(total=sum(Emissions))

get_city <- function(fips){
  if (fips=="06037") return("Los Angeles")
  if (fips=="24510") return("Baltimore City")
  return(NA)
}
total_emissions$city <- sapply(total_emissions$fips, get_city)

qplot(data=total_emissions,
      x=year,
      y=total,
      geom="line",
      xlab="Year", 
      ylab="Emissions (tons)", 
      main="PM2.5 Emissions in Baltimore City and Los Angeles due to motor vehicles 1999-2008",
      col=city)
ggsave("plot6.png", width = 8, height = 5)