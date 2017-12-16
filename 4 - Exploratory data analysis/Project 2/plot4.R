library(dplyr)
library(ggplot2)
library(stringr)

summary_df <- readRDS("summarySCC_PM25.rds")
source_df = readRDS("Source_Classification_Code.rds")

## Fourth plot
## Emissions due to coal sources across the US
sources <- source_df %>%
  filter(str_detect(EI.Sector, "[Cc]oal")) %>%
  select(SCC)

selections <- as.numeric(sources$SCC)
sources <- as.character(levels(sources$SCC))[selections]

total_emissions <- summary_df %>%
  filter(SCC %in% sources) %>%
  group_by(year) %>% 
  summarise(total=sum(Emissions))

qplot(data=total_emissions,
      x=year,
      y=total,
      geom="line",
      xlab="Year", 
      ylab="Emissions (tons)", 
      main="PM2.5 Emissions in the US due to combustion of coal 1999-2008",
      ylim=c(0, 6e5))
ggsave("plot4.png", width = 8, height = 5)
