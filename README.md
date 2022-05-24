# Diversity in Cochrane Reviews' authorship: A meta-research study
The aim of this meta-research study is to examine the level of country, region, language, and gender diversity in Cochrane Reviews' authorship via a fully-automated reproducible approach.

Here, I will update the results whenever I can devote some time to this spare time project. All the latest codes and data are available through this GitHub repository.

The number of citations retrieved from Cochrane Library on May 8, 2022 was 8,824. I managed to retrieve affiliaitons for 8,795 citations through Cochrane Library (99.7%). Then, I splitted affiliations and put each one in a column and then extracted country, region, and gender of each author. Unfortunately, I don't think there is a way to identify non-binary people from their names; therefore, there is only two genders in the database (). Non-binary and transgender people are estimated to have a proportion of almost 2% in the Brazilian population ([DOI: 10.1038/s41598-021-81411-4](https://doi.org/10.1038/s41598-021-81411-4)).

## Country and region diversity
### First authors
#### Countries
Top five countries with the highest number of first authors:
| Country        | Frequency |
|----------------|-----------|
| United Kingdom | 2923      |
| Australia      | 1025      |
| United States  | 600       |
| Canada         | 599       |
| China          | 418       |

This maps shows the total number of first authors from each country.
![First authors world map](figures/fig_first_authors_countries_2.png)

#### United Nations regions
Regions sorted descending based on the total number of first authors:
| UN Region | Frequency |
|-----------|-----------|
| Europe    | 4650      |
| Americas  | 1609      |
| Oceania   | 1189      |
| Asia      | 993       |
| Africa    | 303       |

#### World Bank regions
Regions sorted descending based on the total number of first authors:
| WB Region                  | Frequency |
|----------------------------|-----------|
| Europe & Central Asia      | 4657      |
| East Asia & Pacific        | 1857      |
| North America              | 1199      |
| Latin America & Caribbean  | 409       |
| Sub-Saharan Africa         | 279       |
| South Asia                 | 183       |
| Middle East & North Africa | 160       |

#### World Bank Income-level regions
Regions sorted descending based on the total number of first authors:
| WB Income Region      | Frequency |
|-----------------------|-----------|
| High income: OECD     | 7110      |
| Upper-middle income   | 1213      |
| Lower-middle income   | 263       |
| Low income            | 90        |
| High income: non-OECD | 68        |
