# Illinois Environmental Justice Communities Map

[This code](https://github.com/TroyHernandez/IL_EJ_map) generates [a map](https://troyhernandez.shinyapps.io/IL_EJ_map/) (patience! it takes a minute load) that visualizes the Illinois Power Agency's (IPA) Environmental Justice (EJ) Communities.  The IPA has a fund for renewable energy according to the [Future Jobs Energy Act](https://www.illinois.gov/sites/ipa/Pages/Renewable_Resources.aspx) which has a pot of money for job training for folks in EJ communities to install solar panels and what not.

Here is what the IPA said about calculating which communities met the “EJ criteria”:

> The Agency would then weight each factor using an approach adapted from CalEnviroScreen: Census tracts would be ranked for each environmental and demographic indicator, a resulting percentile score would be found for each tract, and the percentile scores would be averaged, resulting in an environmental and demographic score for each tract.  The two averages would be multiplied together to determine a score.

The IPA goes on to say:

> Communities with scores in the top 25% of scores would then be defined as Environmental Justice Communities for the purpose of the Illinois Solar for All Program.

That means: For the purposes of this visualization, census blocks with an EnviroScore of 75% or greater are "Environmental Justice Communities".

The environmental indicators are:

1. National Scale Air Toxics Assessment Air Toxics Cancer Risk
2. National Scale Air Toxics Assessment Respiratory Hazard Index
3. National Scale Air Toxics Assessment Diesel PM (DPM)
4. Particulate Matter (PM2.5)
5. Ozone
6. Lead Paint Indicator
7. Traffic Proximity and Volume
8. Proximity to Risk Management Plan Sites
9. Proximity to Treatment Storage and Disposal Facilities
10. Proximity to National Priorities List Sites
11. Proximity to Major Direct Water Dischargers

The demographic indicators are:

1. Percent low-income
2. Percent minority
3. Less than high school education
4. Linguistic isolation
5. Individuals under age 5
6. Individuals over age 64

This code is a modified version of [Gene Leynes map demo](https://github.com/geneorama/wnv_map_demo) as presented at the [Chicago R User Group October 14th Lightening talks](https://github.com/Chicago-R-User-Group/Oct.-14th-Short-Talks).  Thanks Gene!