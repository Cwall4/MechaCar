# NYC Citi Bike Analysis 

This is a summary of the analytic work that I did with the NYC Citi Bike data.

# Population

## Des Moines Analysis

According to [this link](https://www.census.gov/quickfacts/fact/table/desmoinescityiowa/PST045219), Des Moines had a population of 214,237 on July 1, 2019. This is compared to New York City's July 1, 2019 population (from the same database) of 8,336,817. Des Moines' population is approximately 2.57% of NYC's. 

## Estimated Number of Bikes

Without any other information, we might then estimate that bike-sharing in Des Moines would require about 2.5% as many bikes as in NYC. Before making any decisions though, we would want more details on city density, existing options for transportation, and so on. With the number of bikes in NYC at 13,983, 2.5% of that is almost 350.

[Here's a link to the Tableau dashboard.](https://public.tableau.com/profile/colin.wallace#!/vizhome/Challenge_15909835732000/Challenge?publish=yes)

# Infrastructure

## Des Moines

One potential issue with making these extrapolations from NYC to Des Moines was whether the city was bike-friendly. In researching I found several maps, [this one](https://dmampo.maps.arcgis.com/apps/webappviewer/index.html?id=c48776f60395490eb3029f5b29fc7b88) appearing the most detailed, of Des Moines' trails and bike lanes. This map was found at [the City of Des Moines website](https://www.dsm.city/departments/parks_and_recreation-division/places/trails.php), which has additional resources about the city's system. I would encourage further study of whether the city's existing bike infrastructure is sufficient for introduction of Citi Bike sharing.

# Gender Diversity

## New York City

New York City, according to [this link](https://www.census.gov/quickfacts/fact/table/newyorkcitynewyork,desmoinescityiowa/PST045219) has a 52.3% female population.

Based on our Citi Bike sample data, about 25% of NYC bike trips were taken by females. We then might put together a rough guess that with 52% of the population, but only 25% of the trips, females are half as likely as males to use Citi Bikes. If we are willing to extrapolate this to Des Moines, the lower proportion of females may increase per capita bike usage.

## Des Moines

Des Moines, according to [this link](https://www.census.gov/quickfacts/fact/table/desmoinescityiowa/PST045219), has a 50.8% female population.

# McDonald's Density

## New York City

Based on the ny_mcdonalds.csv file given, NYC has 26 McDonald's within a 0.08 by 0.13 area.
This is a density of 26 / (0.08 * 0.13) = 2500 per unit.

There also appears to be some positive correlation between McDonald's locations and popular bike start / end locations, at least in NYC.

## Des Moines

Based on the ia_mcdonalds.csv file given, Des Moines has 20 McDonald's within a 0.16 by 0.37 area.
This is a density of 20 / (0.16 * 0.37) = 337.8 per unit.