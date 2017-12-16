library(dplyr)
library(ggplot2)

summary_df <- readRDS("summarySCC_PM25.rds")
source_df = readRDS("Source_Classification_Code.rds")

sources <- source_df %>%
  filter(Data.Category=="Onroad") %>%
  select(SCC)

selections <- as.numeric(sources$SCC)
sources <- as.character(levels(sources$SCC))[selections]

total_emissions <- summary_df %>%
  filter(fips=="24510") %>%
  filter(SCC %in% sources) %>%
  group_by(year) %>% 
  summarise(total=sum(Emissions))

qplot(data=total_emissions,
      x=year,
      y=total,
      geom="line",
      xlab="Year", 
      ylab="Emissions (tons)", 
      main="PM2.5 Emissions in Baltimore City due to motor vehicles 1999-2008",
      ylim=c(0, 400))
ggsave("plot5.png", width = 8, height = 5)