---
title: The Easier Way to Create a Map of Norway Using {csmaps}
date: 2021-08-24
description: The Easier Way to Create a Map of Norway Using {csmaps}
slug: the-easier-way-to-create-a-map-of-norway-using-csmaps
categories:
  - ggplot
tags:
  - ggplot
  - map
  - norway
execute:
  fig.retina: 2
  fig.align: center
  fig.show: hold
  results: hold
  out.width: 80%
editor_options:
  chunk_output_type: console
---

<style type="text/css">
p.stand-out-paragraph code {
  background-color: rgba(165, 132, 138, 0.5);
}
</style>

{{< standout >}}
As of March 2023 <code>{fhimaps}</code> and <code>{splmaps}</code> are no longer supported due to budget cuts at the institution supporting them. The amazing developers (led by [Richard Aubrey White](https://www.rwhite.no) and [Chi Zhang](https://andreaczhang.github.io)) have moved the data and functionality to a new package called `{csmaps}`. The code below is updated to reflect the changes.
{{< /standout >}}

Every now and then you discover a discover a much simpler solution to a problem you spent a lot of time solving. This recently happened to me on the topic of creating a map of Norway in R. In this post, I want to go through the process of what I learned.

Previously, I used a JSON file and the `{geojsonio}` package to create a map of Norway and its fylker (counties) in particular. This was a very flexible and robust way of going about this, but also quite cumbersome. This method relies on a high-quality JSON file, meaning, a file that is detailed enough to display everything nicely, but not too detailed that it takes a ton of time and computing power to create a single plot. While I'll still use this method if I need to create a map for places other than Norway, I think I've found a better and easier solution for plotting Norway and the fylker and kommuner in the [`{csmaps}`](https://www.csids.no/csmaps/) package.

{{< sidenote >}}
The group has [a bunch of packages](https://www.csids.no) for data science within public health
{{< /sidenote >}}

The `{csmaps}` package is created by the [*Consortium for Statistics in Disease Surveillance*](https://www.csids.no) team. It's part of a series of packages (which they refer to as the "csverse"), which includes a package containing basic health surveillance data ([`{csdata}`](https://www.csids.no/csdata/)), one for real-time analysis in disease surveillance ([`{sc9}`](https://www.csids.no/sc9/)) and a few more. Here I'll dive into the `{csmaps}` package with some help from the `{csdata}` package. I'll also use the `{ggmap}` package to help with some other data and plotting. It's perhaps important to note that `{ggmap}` does contain a map of Norway as a whole, but not of the fylker and kommuner (municipalities), hence the usefulness of the `{csmaps}` package, which contains both. I'll also use `{tidyverse}` and `{ggtext}` as I always do. I won't load `{csmaps}` with the `library()` function, but will use the `::` operator instead since it'll make it easier to navigate the different datasets included.

``` r
library(tidyverse)
library(csmaps)
library(ggtext)
library(ggmap)
```

So let's have a look at what's included. You'll see that nearly all maps come in either a `data.table` format or an `sf` format. Here I'll use only the data frames, since they're a lot easier to work with. The maps in `sf` format can be useful elsewhere, but I think for most purposes it's easier and more intuitive to work with data frames.

``` r
data(package = "csmaps") |>
  pluck("results") |>
  as_tibble() |>
  select(Item, Title) |>
  print(n = 18)
```

A comprehensive version of this list is also included in the [reference](https://www.csids.no/csmaps/reference/index.html) for this package.

So let's have a look at one of those maps. For instance the one with the new fylker from 2020 with an inset of Oslo.

``` r
map_df <- nor_county_map_b2020_insert_oslo_dt |>
  glimpse()
```

    Rows: 4,493
    Columns: 5
    $ long          <dbl> 5.823860, 5.969413, 6.183042, 6.302433, 6.538059, 6.6935…
    $ lat           <dbl> 59.64576, 59.57897, 59.58607, 59.76743, 59.84285, 59.822…
    $ order         <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1…
    $ group         <fct> 11.1, 11.1, 11.1, 11.1, 11.1, 11.1, 11.1, 11.1, 11.1, 11…
    $ location_code <chr> "county_nor11", "county_nor11", "county_nor11", "county_…

Immediately you can see that there's a lot of rows, each representing a point on the map. A data frame with a larger number of rows would be more detailed (i.e. less straight lines, more detail in curvatures of borders etc.). Let's create a very simple map. We'll use the `geom_polygon` to turn our data frame into a map. The location of the points are given in longitudes and latitudes (like x- and y-coordinates), the group serves to make sure lines are drawn correctly (try running the code below without `group = group` and see what happens). The `location_code` denotes the county number (which isn't from 1 to 11, but instead uses some other standard format matching numbers in other government datasets). Let's see the simplest map:

``` r
ggplot(map_df, aes(
  x = long, y = lat,
  group = group, fill = location_code
)) +
  geom_polygon()
```

<img src="index.markdown_strict_files/figure-markdown_strict/minimal-plot-1.png" width="768" />

Now let's convert the awkward county numbers to the actual names of the fylker. The `{csdata}` package has a data frame with codes and names for all the kommuner, fylker, and even regions (Øst, Vest, Nord-Norge etc.). We're only interested in the fylker here, so we'll select the unique location codes and the corresponding location names.

``` r
county_names <- csdata::nor_locations_names(border = 2020) |>
  filter(str_detect(location_code, "^county")) |>
  distinct(location_code, location_name)

print(county_names)
```

        location_code        location_name
     1:  county_nor42                Agder
     2:  county_nor34            Innlandet
     3:  county_nor15      Møre og Romsdal
     4:  county_nor18             Nordland
     5:  county_nor03                 Oslo
     6:  county_nor11             Rogaland
     7:  county_nor54    Troms og Finnmark
     8:  county_nor50            Trøndelag
     9:  county_nor38 Vestfold og Telemark
    10:  county_nor46             Vestland
    11:  county_nor30                Viken

{{< sidenote >}}
The [`{scico}`](https://github.com/thomasp85/scico) package contains a collection of color palettes that are great for accessibility
{{< /sidenote >}}

Now let's also create a nice color palette to give each fylke a nicer color than the default ggplot colors. We'll create a named vector to match each fylke with a color from the *batlow* palette by [Fabio Crameri](https://www.fabiocrameri.ch/colourmaps/) implemented in the `{scico}` package.

``` r
county_colors <- setNames(
  scico::scico(
    n = nrow(county_names),
    palette = "batlow"
  ),
  nm = county_names$location_name
)
```

Let's see what we can make now. We'll add the county names to the large data frame containing the longitudes and latitudes and then create a plot again. I'll also add some other style elements, such as a labels to the x- and y-axes, circles instead of squares for the legend and a map projection. For Norway especially I think a conic map projection works well since the northern fylker are so massive and the southern fylker are more dense, so adding a conic projection with a cone tangent of 40 degrees makes it a bit more perceptionally balanced (`lat0` refers to the cone tangent, the details are complicated but a higher cone tangent results a greater distortion in favor of southern points).

``` r
map_df |>
  left_join(county_names, by = "location_code") |>
  ggplot(aes(
    x = long, y = lat,
    fill = location_name, group = group
  )) +
  geom_polygon(key_glyph = "point") +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  scale_x_continuous(
    labels = scales::label_number(suffix = "\u00b0W")
  ) +
  scale_y_continuous(
    labels = scales::label_number(suffix = "\u00b0N")
  ) +
  scale_fill_manual(
    values = county_colors,
    guide = guide_legend(override.aes = list(shape = 21, size = 4))
  ) +
  coord_map(projection = "conic", lat0 = 40) +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.2),
    legend.text = element_text(size = 5),
    legend.key.height = unit(10, "pt"),
    legend.background = element_rect(
      fill = "white",
      color = "transparent"
    )
  )
```

<img src="index.markdown_strict_files/figure-markdown_strict/simple-plot-1.png" width="768" />

## Norway with Scandinavia

Sometimes it's useful to plot Norway in geographical context. We can overlay Norway on a map of Scandinavia or Europe to create a more aesthetically pleasing figure that is less scientific but easier to read. For that we'll first extract the longitude and latitude extremities of Norway, so we can easily center Norway on our new map.

``` r
str_glue("Range across longitude: {str_c(range(map_df$long), collapse = ', ')}")
str_glue("Range across latitude: {str_c(range(map_df$lat), collapse = ', ')}")
```

    Range across longitude: 4.64197936086325, 31.0578692314387
    Range across latitude: 57.9797576545731, 71.1848833506563

Let's also combine the map with some actual data. The `{csdata}` package contains some simple data on age distribution in kommuner and fylker. Let's take the data from the different fylker in the last available year and see what proportion of the total population is younger than 18.

``` r
age_data <- csdata::nor_population_by_age_cats(
  border = 2020,
  cats = list(seq(18))
) |>
  filter(
    str_detect(location_code, "^county"),
    calyear == max(calyear)
  ) |>
  pivot_wider(
    id_cols = location_code,
    names_from = age, values_from = pop_jan1_n
  ) |>
  janitor::clean_names() |>
  rename(age_0_18 = x001_018) |>
  mutate(proportion = age_0_18 / total)
```

Let's create a map without the Oslo inset, combine it with the age distribution data and plot it on top of a map of the world cropped to just Scandinavia. So for this we load another data frame and use `left_join` to merge the age distribution data into one data frame. The `{ggmap}` package has a map of the entire world, which we'll use. To avoid awkward overlap, we'll plot everything except Norway from that world map (since we'll have our own better map to use instead). We'll set the `fill` to proportion of the population under the age of 18 and set similar style elements to make the figure look nicer. I used the extremities we extracted earlier as a guideline, but we can play around with the crop of the map to get something that works best.

``` r
nor_county_map_b2020_default_dt |>
  left_join(age_data, by = "location_code") |>
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(
    data = map_data("world") |> filter(region != "Norway"),
    fill = "grey80"
  ) +
  geom_polygon(aes(fill = proportion), key_glyph = "point") +
  labs(fill = "Proportion of the population younger than 18") +
  scico::scale_fill_scico(
    palette = "devon", limits = c(0.15, 0.31),
    labels = scales::percent_format(accuracy = 1),
    guide = guide_colorbar(
      title.position = "top", title.hjust = 0.5,
      barwidth = 10, barheight = 0.5, ticks = FALSE
    )
  ) +
  coord_map(
    projection = "conic", lat0 = 60,
    xlim = c(-8, 40), ylim = c(57, 70)
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "#A2C0F4",
      color = "transparent"
    ),
    legend.direction = "horizontal",
    legend.position = c(0.8, 0.1),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6)
  )
```

<img src="index.markdown_strict_files/figure-markdown_strict/age-plot-1.png" width="768" />

## Geocoding

{{< sidenote br="3em" >}}
The Google Maps API requires a personal API key, you can read how to obtain and register an API key [here](https://github.com/dkahle/ggmap)
{{< /sidenote >}}

The `{ggmap}` package also has an incredibly useful function called `mutate_geocode()` which transforms a string with an address or description in character format to longitude and latitude. Since `{ggmap}` uses the Google Maps API, it works similarly to typing in a description in Google Maps. So an approximation of the location will (most likely) get you the right result (e.g. with "Hospital Lillehammer"). Note that `mutate_geocode` uses `lon` instead of `long` as column name for longitude. Just to avoid confusion, I'll rename the column to `long`.

``` r
hospitals_df <- tibble(location = c(
  "Ullevål Sykehus, Oslo",
  "Haukeland universitetssjukehus, Bergen",
  "St. Olav, Trondheim",
  "Universitetssykehuset Nord-Norge, Tromsø",
  "Stavanger Universitetssjukehus",
  "Sørlandet Hospital Kristiansand",
  "Hospital Lillehammer"
)) |>
  mutate_geocode(location) |>
  rename(long = lon)
```

This is the list of coordinates it gave us:

``` r
print(hospitals_df)
```

    # A tibble: 7 × 3
      location                                  long   lat
      <chr>                                    <dbl> <dbl>
    1 Ullevål Sykehus, Oslo                    10.7   59.9
    2 Haukeland universitetssjukehus, Bergen    5.36  60.4
    3 St. Olav, Trondheim                      10.4   63.4
    4 Universitetssykehuset Nord-Norge, Tromsø 19.0   69.7
    5 Stavanger Universitetssjukehus            5.73  59.0
    6 Sørlandet Hospital Kristiansand           7.98  58.2
    7 Hospital Lillehammer                     10.5   61.1

Now let's put these on top of the map. We'll use the same map we used earlier. We'll add the locations using a simple `geom_point`. This time I'll also add a line from the inset to Oslo's location on the map. The `xend` and `yend` coordinates takes some trial-and-error but I think it's worth it. I'll also add labels to each of the points with the help of the `{ggrepel}` package to illustrate what the points stand for. I suppose usually this is obvious from context, but I just wanted to show how it can be done anyway.

``` r
set.seed(21)

map_df |>
  left_join(county_names, by = "location_code") |>
  ggplot(aes(
    x = long, y = lat,
    fill = location_name, group = group
  )) +
  geom_polygon(key_glyph = "point") +
  geom_segment(
    data = hospitals_df |> filter(str_detect(location, "Oslo")),
    aes(x = long, y = lat, xend = 19.5, yend = 62),
    inherit.aes = FALSE
  ) +
  geom_point(
    data = hospitals_df, aes(x = long, y = lat),
    inherit.aes = FALSE,
    shape = 18, color = "firebrick",
    size = 4, show.legend = FALSE
  ) +
  ggrepel::geom_label_repel(
    data = hospitals_df, aes(x = long, y = lat, label = location),
    size = 2, alpha = 0.75, label.size = 0, inherit.aes = FALSE
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  scale_fill_manual(
    values = county_colors,
    guide = guide_legend(override.aes = list(
      size = 4, shape = 21,
      color = "transparent"
    ))
  ) +
  coord_map(projection = "conic", lat0 = 60) +
  theme_void() +
  theme(
    legend.position = c(0.2, 0.7),
    legend.text = element_text(size = 5),
    legend.key.height = unit(10, "pt")
  )
```

<img src="index.markdown_strict_files/figure-markdown_strict/plot-w-cities-1.png" width="768" />

## Combine the map with other data

Let's take it a step further and now look at how we can combine our map with data that didn't come directly from the FHI. Instead I downloaded some data from the Norwegian Statistics Bureau ([Statistisk sentralbyrå, SSB](https://www.ssb.no)) on land use in the kommuner ([link](https://www.ssb.no/en/natur-og-miljo/areal/statistikk/arealbruk-og-arealressurser)). This came in the form of a semi-colon separated .csv file.

``` r
area_use <- read_delim("./data/areal.csv",
  delim = ";", skip = 2
) |>
  janitor::clean_names()

print(area_use)
```

    # A tibble: 356 × 20
       region   area_2021_residentia…¹ area_2021_recreation…² area_2021_built_up_a…³
       <chr>                     <dbl>                  <dbl>                  <dbl>
     1 3001 Ha…                   9.61                   1.85                   2.51
     2 3002 Mo…                  10.4                    1.86                   1.59
     3 3003 Sa…                  14.0                    2.95                   3.49
     4 3004 Fr…                  19.9                    5.14                   3.05
     5 3005 Dr…                  18.4                    1.17                   1.26
     6 3006 Ko…                   7.68                   2.23                   2.38
     7 3007 Ri…                  11                      3.99                   4.18
     8 3011 Hv…                   2.31                   5.79                   0.65
     9 3012 Ar…                   0.64                   0.73                   0.8 
    10 3013 Ma…                   1.38                   0.58                   1.68
    # ℹ 346 more rows
    # ℹ abbreviated names: ¹​area_2021_residential_areas,
    #   ²​area_2021_recreational_facilities,
    #   ³​area_2021_built_up_areas_for_agriculture_and_fishing
    # ℹ 16 more variables: area_2021_industrial_commercial_and_service_areas <dbl>,
    #   area_2021_education_and_day_care_facilities <dbl>,
    #   area_2021_health_and_social_welfare_institutions <dbl>, …

You can see there's 356 rows, each representing a different kommune in Norway. The columns here represent the surface area (in km<sup>2</sup>) with different designations (e.g. forest, health services, agriculture etc.). All data here is from 2021. Now, kommunes have different sizes, so I want to get the designations of interest as percentages of total area in the kommune. Here I assumed that the sum of all designations is equal to the total size of each kommune. I also want to extract the kommune number, since we'll use that to merge this data frame with the map later. The kommune number needs to be 4 digits, so we need to add a leading 0 in some instances. Then we'll create the `location_code` column which will match the `location_code` column in the data frame from `{csmaps}`. Then we'll calculate the percentage land use for different designations. Here I'm just interested in *"bare rock, gravel, and blockfields"*, *"wetland*, *"forest"*, and *"Open firm ground"*.

``` r
area_use <- area_use |>
  mutate(
    total_area = rowSums(across(where(is.numeric))),
    kommune_code = parse_number(region),
    kommune_code = format(kommune_code, digits = 4),
    kommune_code = str_replace_all(kommune_code, " ", "0"),
    location_code = str_glue("municip_nor{kommune_code}"),
    perc_rocks = area_2021_bare_rock_gravel_and_blockfields / total_area,
    perc_wetland = area_2021_wetland / total_area,
    perc_forest = area_2021_forest / total_area,
    perc_open_ground = area_2021_open_firm_ground / total_area
  ) |>
  arrange(kommune_code) |>
  glimpse()
```

    Rows: 356
    Columns: 27
    $ region                                                              <chr> "0…
    $ area_2021_residential_areas                                         <dbl> 51…
    $ area_2021_recreational_facilities                                   <dbl> 1.…
    $ area_2021_built_up_areas_for_agriculture_and_fishing                <dbl> 0.…
    $ area_2021_industrial_commercial_and_service_areas                   <dbl> 10…
    $ area_2021_education_and_day_care_facilities                         <dbl> 3.…
    $ area_2021_health_and_social_welfare_institutions                    <dbl> 1.…
    $ area_2021_cultural_and_religious_activities                         <dbl> 0.…
    $ area_2021_transport_telecommunications_and_technical_infrastructure <dbl> 36…
    $ area_2021_emergency_and_defence_services                            <dbl> 0.…
    $ area_2021_green_areas_and_sports_facilities                         <dbl> 9.…
    $ area_2021_unclassified_built_up_areas_and_related_land              <dbl> 8.…
    $ area_2021_agricultural_land                                         <dbl> 9.…
    $ area_2021_forest                                                    <dbl> 27…
    $ area_2021_open_firm_ground                                          <dbl> 7.…
    $ area_2021_wetland                                                   <dbl> 4.…
    $ area_2021_bare_rock_gravel_and_blockfields                          <dbl> 0.…
    $ area_2021_permanent_snow_and_glaciers                               <dbl> 0.…
    $ area_2021_inland_waters                                             <dbl> 28…
    $ area_2021_unclassified_undeveloped_areas                            <dbl> 0,…
    $ total_area                                                          <dbl> 45…
    $ kommune_code                                                        <chr> "0…
    $ location_code                                                       <glue> "…
    $ perc_rocks                                                          <dbl> 0.…
    $ perc_wetland                                                        <dbl> 0.…
    $ perc_forest                                                         <dbl> 0.…
    $ perc_open_ground                                                    <dbl> 0.…

Then the next step is very similar to what we've done before. We'll use `left_join` to merge the data frame containing the land use variables with the data frame containing the map with the kommune borders. I want to plot the four designations of interest in one figure, so I'll transform the plot to a long format using `pivot_longer`. Then I'll create a new label with nicer descriptions of the designations, and then the rest is similar to before. We'll facet the plot based on the designation:

``` r
nor_municip_map_b2020_split_dt |>
  left_join(area_use, by = "location_code") |>
  pivot_longer(
    cols = starts_with("perc"),
    names_to = "land_type", values_to = "percentage"
  ) |>
  mutate(land_type_label = case_when(
    str_detect(land_type, "rocks") ~ "Bare rock, gravel and rockfields",
    str_detect(land_type, "wetland") ~ "Wetland",
    str_detect(land_type, "forest") ~ "Forest",
    str_detect(land_type, "open_ground") ~ "Open firm ground"
  )) |>
  ggplot(aes(
    x = long, y = lat,
    group = group, fill = percentage
  )) +
  geom_polygon() +
  labs(fill = "Percentage") +
  scico::scale_fill_scico(
    palette = "acton",
    labels = scales::label_percent(),
    limits = c(0, 1),
    guide = guide_colorbar(
      barheight = 0.5, barwidth = 12,
      ticks = FALSE, direction = "horizontal",
      title.position = "top", title.hjust = 0.5
    )
  ) +
  facet_wrap(~land_type_label) +
  coord_map(projection = "conic", lat0 = 60) +
  theme_void() +
  theme(
    legend.position = "bottom",
    strip.text.x = element_textbox_simple(
      size = rel(1.25), halign = 0.5,
      margin = margin(10, 0, 10, 0, "pt")
    )
  )
```

<img src="index.markdown_strict_files/figure-markdown_strict/plot-kommune-faceted-1.png" width="768" />

## Oslo

The last thing I want to show is a map of Oslo! `{csmaps}` contains a detailed map of the bydeler that we will use. Now, these bydeler are again coded and `{csdata}` (since it's update) now contains a [data frame](https://www.csids.no/csmaps/articles/customization.html#add-location-name-for-ward-and-population-for-oslo-map) with the corresponding names for all geography levels (fylker, municipalities, bydeler, etc.). We could get our data from there, but we also need something to visualize so we'll scrape a Wikipedia article for Oslo's bydeler which contains a table with the bydel numbers, the names, and some data we can use for visualization. We'll extract the table from the website using `{rvest}`, do some data wrangling and prepare it for merging into the data frame with the map. I won't go into the wrangling much here, we're interested mainly in the plotting of the data right now.

{{< sidenote >}}
I won't go into much detail on web scraping here, you can see one of the [other posts](https://danielroelfs.com/blog/) for more details
{{< /sidenote >}}

``` r
bydel_data <- "https://en.wikipedia.org/wiki/List_of_boroughs_of_Oslo" |>
  rvest::read_html() |>
  rvest::html_table() |>
  pluck(1) |>
  janitor::clean_names() |>
  mutate(
    inhabitants = str_remove_all(residents, "[[:blank:]]"),
    inhabitants = as.numeric(inhabitants),
    area = str_remove_all(area, "km2"),
    area = str_replace_all(area, ",", "."),
    area = str_squish(area),
    area = as.numeric(area),
    pop_density = inhabitants / area
  ) |>
  arrange(number) |>
  mutate(
    bydel_nr = format(number, digits = 2),
    bydel_nr = str_replace_all(bydel_nr, " ", "0"),
    location_code = str_glue("wardoslo_nor0301{bydel_nr}")
  )

print(bydel_data)
```

    # A tibble: 15 × 8
       borough residents  area number inhabitants pop_density bydel_nr location_code
       <chr>   <chr>     <dbl>  <int>       <dbl>       <dbl> <chr>    <glue>       
     1 Gamle … 58 671      7.5      1       58671       7823. 01       wardoslo_nor…
     2 Grüner… 62 423      4.8      2       62423      13005. 02       wardoslo_nor…
     3 Sagene  45 089      3.1      3       45089      14545. 03       wardoslo_nor…
     4 St. Ha… 38 945      3.6      4       38945      10818. 04       wardoslo_nor…
     5 Frogner 59 269      8.3      5       59269       7141. 05       wardoslo_nor…
     6 Ullern  34 596      9.4      6       34596       3680. 06       wardoslo_nor…
     7 Vestre… 50 157     16.6      7       50157       3022. 07       wardoslo_nor…
     8 Nordre… 52 327     13.6      8       52327       3848. 08       wardoslo_nor…
     9 Bjerke  33 422      7.7      9       33422       4341. 09       wardoslo_nor…
    10 Grorud  27 707      8.2     10       27707       3379. 10       wardoslo_nor…
    11 Stovner 33 316      8.2     11       33316       4063. 11       wardoslo_nor…
    12 Alna    49 801     13.7     12       49801       3635. 12       wardoslo_nor…
    13 Østens… 50 806     12.2     13       50806       4164. 13       wardoslo_nor…
    14 Nordst… 52 459     16.9     14       52459       3104. 14       wardoslo_nor…
    15 Søndre… 39 066     18.4     15       39066       2123. 15       wardoslo_nor…

`{csmaps}` also provides a very useful data frame containing the geographical center or best location to put a label to avoid overlap and make it as clear as possible which label corresponds to which bydel. So we'll merge those two together.

``` r
bydel_centres <- oslo_ward_position_geolabels_b2020_default_dt |>
  inner_join(bydel_data, by = "location_code")
```

Then we'll create the final plot. This will be more-or-less identical to what we did before.

``` r
oslo_ward_map_b2020_default_dt |>
  left_join(bydel_data, by = "location_code") |>
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(color = pop_density, fill = pop_density)) +
  geom_label(
    data = bydel_centres, aes(label = borough, group = 1),
    alpha = 0.5, label.size = 0
  ) +
  labs(fill = "N<sup>o</sup> of inhabitants per km<sup>2</sup>") +
  scico::scale_color_scico(
    palette = "turku",
    limits = c(0, 1.5e4),
    guide = "none"
  ) +
  scico::scale_fill_scico(
    palette = "turku",
    limits = c(0, 1.5e4),
    labels = scales::number_format(),
    guide = guide_colorbar(
      title.position = "top", title.hjust = 0.5,
      barwidth = 15, barheight = 0.75, ticks = FALSE
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_markdown(),
    legend.direction = "horizontal"
  )
```

<img src="index.markdown_strict_files/figure-markdown_strict/plot-oslo-1.png" width="768" />

**BONUS**

An example of when you might use the `sf` data format is in interactive maps. Here's a short example of what that might look like. Here we'll use the `{leaflet}` package (which is an R wrapper for the homonymous JavaScript library) to create an interactive map where we can zoom and hover on the different fylker. This won't work on printed media, but it might be nice to include in a digital format. You could possibly extract the corresponding HTML and JavaScript code and embed it on a webpage separately.

``` r
library(leaflet)

map_sf <- csmaps::nor_county_map_b2020_default_sf |>
  sf::st_as_sf() |>
  left_join(county_names, by = "location_code")

map_sf |>
  leaflet() |>
  addProviderTiles(providers$Esri.WorldStreetMap) |>
  addPolygons(
    fillColor = unname(county_colors),
    weight = 0.1,
    opacity = 1,
    fillOpacity = 0.75,
    highlightOptions = highlightOptions(
      color = "#333", bringToFront = TRUE,
      weight = 2, opacity = 1
    )
  )
```

<iframe seamless src="/2021-easy-map-norway-leafMap.html" width="100%" height="500" frameBorder="0" style="padding-top: 1em;">
</iframe>

**EDIT (2022-09-04)**: Updated the blogpost to replace usage of the retiring `{fhimaps}` and `{fhidata}` packages with the newer `{splmaps}` and `{spldata}` packages from FHI. The `{fhidata}` package included a dataset on vaccination rates in Norway, but since this isn't incorporated in the new `{spldata}` package I replaced that plot with a plot about age distribution.
In addition, I also replaced all usage of the NORMENT internal `{normentR}` package with the `{scico}` package which is available on CRAN.

**EDIT (2023-06-18)**: Updated the blogpost to reflect the deprecation of the `{splmaps}` and `{spldata}` packages. Replaced them with the replacement package `{csmaps}` available on CRAN.

<link rel="stylesheet" href="https://unpkg.com/leaflet@1.7.1/dist/leaflet.css" integrity="sha512-xodZBNTC5n17Xt2atTPuE1HxjVMSvLVW9ocqUKLsCC5CXdbqCmblAshOMAS6/keqq/sMZMZ19scR4PsZChSR7A==" crossorigin=""/>

<script src="https://unpkg.com/leaflet@1.7.1/dist/leaflet.js" integrity="sha512-XQoYMqMTK8LvdxXYG3nZ448hOEQiglfqkJs1NOQV44cWnUrBc8PkAOcXy20w0vlaXaVUearIOBhiXZ5V3ynxwA==" crossorigin=""></script>
