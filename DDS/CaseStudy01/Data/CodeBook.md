# Codebook

## Study Design
The data was provided to us. We have no knowledge of the collection procedures used. We do not know if this data is a sample of the population of craft beers and breweries OR if the data is the entire population. We also do not know the time or time frame for the data.

There are two datasets used in this analysis: Beers and Breweries. 

## Beers.csv
Name: Name of the beer.
Beer_ID: Unique identifier of the beer.
ABV: Alcohol by volume of the beer.
IBU: International Bitterness Units of the beer.
Brewery_ID: Brewery id associated with the beer.
Style: Style of the beer.
Ounces: Ounces of beer.

## Breweries.csv
Brewery_id: Unique identifier of the brewery.
Name: Name of the brewery.
City: City where the brewery is located.
State: U.S. State where the brewery is located.

## Tidying Data
There are missing values present for ABV(3%) and IBU(42%). For the purpose of this analysis, we omit records when the pertinent field is missing values. For example, all records are used in the Brewery Counts by State graph, but 62 records are removed on the Median ABV by State graph because ABV is missing.

