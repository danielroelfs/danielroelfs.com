---
title: Remote sensing of vineyard health
date: 2025-05-13T00:00:00.000Z
description: Remote sensing of vineyard health
slug: remote-sensing-viticulture
categories:
  - miscellaneous
tags:
  - miscellaneous
  - python
engine: knitr
editor_options:
  chunk_output_type: console
---


<style type="text/css">
.footnotes {
  font-size: 80%;
}

sup {
  vertical-align: top;
  position: relative;
  top: -0.5em;
}
</style>

Throughout my career so far I've been lucky to work with many different types of data. It's exciting how all different kinds of data can be represented and interpreted by different programming languages[^1]. So today I'm going to do another one of these and dive into analyzing data from satellite images. Learning to parse data from freely available satellite imagery is both a useful skill to have and quite fun, so today I'll share a little project that I have whipped together. There are more ambitious projects we can use these analyses for, but let's start simple.

You may skip to the {{< xref text=\"load data\" link=\"#load-data\" >}} section to skip the introduction and hop straight to the code.

{{< sidenote br=\"20em\" >}}
While we cannot see infrared light, it is [perceptible as heat in some situations](https://science.nasa.gov/ems/07_infraredwaves/)
{{< /sidenote >}}

The project we'll focus on today is to use satellite imagery to analyze the health of vineyards. We'll first do a little experiment to show the concept, and then we'll apply the same analysis to some real use case. The analysis we'll be using today is based on the [Normalized Difference Vegetation Index](https://gisgeography.com/ndvi-normalized-difference-vegetation-index/), abbreviated as *NDVI*. Let me explain briefly (and probably crudely) how it works. Most cameras, when you take a picture of something will try to recreate the colors as accurately as possible. The most common way to do this under the hood is by measuring the intensity of the three primary color bands (red, green blue, or RGB) so that for example your iPhone can create a composite image of those three colors. The RGB color bands are commonly recorded in regular cameras because it overlaps with the human visual spectrum, but other colors are of course available in more expensive cameras, like satellites. So in addition to RGB colors, satellites may also record infrared bands, which has longer wavelengths than the human eye can observe.

The NDVI uses a combination of the near-infrared and the red color bands to make an estimation of the "greenness" of vegetation. The illustration below illustrates the concept.

{{< image src=\"illustration_ndvi.png\" alt=\"simplified illustration showing how NDVI works\" >}}

{{< sidenote >}}
Obviously light hits basically everything in the frame, also non-vegetation, and this is a confounder we need to deal with later
{{< /sidenote >}}

Red and near-infrared light hits the leaves of the vegetation and green leaves would absorb a lot of the red light and reflect a larger proportion of the near-infrared light, while yellow (or unhealthy) leaves would reflect a larger proportion of the red light and absorb more of the near-infrared. The proportion of the light in the different color bands reflected determines the NDVI. The NDVI is basically a proportion of the red light reflected compared to the near-infrared light. The formula looks as follows:

$$
\text{NDVI} = \frac{\text{near-infrared} - \text{red}}{\text{near-infrared} + \text{red}}
$$

So let's look at this hypothetical situation below. The first image is of some healthy leaves, the second is of some unhealthy leaves. If we fill in the values above in the NDVI formula we get the following values for the healthy plant:

$$
\text{NDVI}_{\text{healthy}} = \frac{0.50 - 0.08}{0.50 + 0.08} = 0.72 = 72\\%
$$

and the following value for the unhealthy plant:

$$
\text{NDVI}_{\text{unhealthy}} = \frac{0.40 - 0.30}{0.40 + 0.30} = 0.14 = 14\\%
$$

At this you might realize, based on the formula above your knowledge on light absorption, that red leaves late in autumn will reflect all the near-infrared light and most of the red light (given that a lot of leaves will have fallen off already), meaning the NDVI would be close to 0. At this point you'll also realize that most values will go from 0 to 1 (or 0% to 100%). Of course, this calculation will go for each pixel in your image.

Let's now look at a quick example of the concept. Again, we need a little bit of background on the concept we'll look at. Once every so often, a plot in a vineyard needs to be cleared for some reason in a process called [replanting](https://lodigrowers.com/vineyard-replanting/). The reason could be a disease in the plants, or it could be that the soil needs to be given time to replenish nutrients, or maybe the winemaker wants to plant different grapes in the plot. We can look at the NDVI values for a plot that has recently been through replanting (our *case*) and one that hasn't (our *control*). I found an image on [Copernicus Browser](https://browser.dataspace.copernicus.eu/?zoom=18&lat=48.14349&lng=7.28968&themeId=DEFAULT-THEME&visualizationUrl=U2FsdGVkX1%2BQjYPD7tQXBt6um3IBcQxf%2BsHuyy%2BC%2BGQpsxC7IdsGBdT6ieUtEbWE6cr2i5U0alG6nHMXZW%2FwfBYUF1%2BhkOOjw%2BZJeIh2PAk9ndn%2Bm2gPjwF7VvazRkKt&datasetId=S2_L2A_CDAS&demSource3D=%22MAPZEN%22&cloudCoverage=30&dateMode=SINGLE) of two plots right next to each other where one was replanted in 2018:

{{< image src=\"screenshot_casecontrol.png\" caption=\"Two plots of land, one recently replanted, and one regular plot for production\" alt=\"Sattelite image of two plots of land\" >}}

In order to compare these two plots to each other we need to generate an outline for each plot. This we'll do in a tool called [geojson.io](http://geojson.io) where we can simply draw a shape around the plot and we'll get the [GeoJSON](https://geojson.org) data for the two plots (also called *features*). The outlines I drew look like this:

{{< image src=\"screenshot_casecontrol_outlined.png\" caption=\"Same plots of land, outlined in GeoJSON format\" alt=\"Sattelite image of two plots of land with the boundaries highlighted\" >}}

While the images above use the RGB color scheme to represent colors as the human eye would see it, the Copernicus Browser also allows for other visualizations, such as the NDVI. So for example, we can easily identify the plot recently replanted by the much brighter (or yellower) colors in that area. You'll also notice that the roads are easily identifiable. For the analysis we'll limit the NDVI calculations only to the areas inside of the GeoJSON specified and you can imagine that the quality of the analysis is quite closely correlated with the quality of the GeoJSON.

<figure>
<img src="screenshot_casecontrol_ndvi.png" alt="The NDVI visualization in the Copernicus Browser for the same plots" />
<figcaption aria-hidden="true">The NDVI visualization in the <a href="https://browser.dataspace.copernicus.eu/?zoom=18&amp;lat=48.14349&amp;lng=7.28968&amp;themeId=DEFAULT-THEME&amp;visualizationUrl=U2FsdGVkX1%2BQjYPD7tQXBt6um3IBcQxf%2BsHuyy%2BC%2BGQpsxC7IdsGBdT6ieUtEbWE6cr2i5U0alG6nHMXZW%2FwfBYUF1%2BhkOOjw%2BZJeIh2PAk9ndn%2Bm2gPjwF7VvazRkKt&amp;datasetId=S2_L2A_CDAS&amp;demSource3D=%22MAPZEN%22&amp;cloudCoverage=30&amp;dateMode=SINGLE">Copernicus Browser</a> for the same plots</figcaption>
</figure>

It's the difference in the NDVI values inside of the boundaries of the GeoJSON features that we'll use for the eventual analysis. First we'll do the case-control comparison, and we'll get into the more realistic case later where we'll look at two grand cru wine areas to compare the health over time.

To run the satellite analysis we'll use the [`openeo`](https://openeo.org/documentation/1.0/python/) module. It provides a bunch of functionality to interact with data from different satellites and with a backend hosted by [Copernicus](https://www.copernicus.eu/en), which is a part of the EU space programme managed by the European Commission which makes all this freely available, which is amazing. In addition to `openeo`, we'll also use some default modules, and the [`signal`](https://docs.scipy.org/doc/scipy/tutorial/signal.html) submodule from [`scipy`](https://docs.scipy.org/doc/scipy/index.html).

``` python
import os
import json

import pandas as pd
import numpy as np
import scipy.signal

import openeo
```

## Load data

So let's (finally) look at some Python code. We connect to the Copernicus server using the `connect()` function from `openeo`. There are several servers to choose from, but the [openeo.dataspace.copernicus.eu](openeo.dataspace.copernicus.eu) one works fine. On this link you will also find the web editor where you can log in and see the jobs you've previously run on the server. We can use the connection object to see what "collections" are available on this server.

``` python
conn = openeo.connect("openeo.dataspace.copernicus.eu")

_ = [print(x) for x in conn.list_collection_ids()]
```

    SENTINEL3_OLCI_L1B
    SENTINEL3_SLSTR
    SENTINEL_5P_L2
    COPERNICUS_VEGETATION_PHENOLOGY_PRODUCTIVITY_10M_SEASON1
    COPERNICUS_VEGETATION_PHENOLOGY_PRODUCTIVITY_10M_SEASON2
    COPERNICUS_PLANT_PHENOLOGY_INDEX
    ESA_WORLDCOVER_10M_2020_V1
    ESA_WORLDCOVER_10M_2021_V2
    COPERNICUS_VEGETATION_INDICES
    SENTINEL2_L1C
    SENTINEL2_L2A
    SENTINEL1_GRD
    COPERNICUS_30
    LANDSAT8_L2
    SENTINEL3_SYN_L2_SYN
    SENTINEL3_SLSTR_L2_LST
    SENTINEL1_GLOBAL_MOSAICS
    SENTINEL3_OLCI_L2_LAND
    SENTINEL3_OLCI_L2_WATER
    SENTINEL3_SYN_L2_AOD

For this project we'll use `SENTINEL2_L2A`, which corresponds to the [Sentinel-2A](https://sentiwiki.copernicus.eu/web/s2-mission) satellite launched in 2015. You can get the full description with all kinds of metadata using the command below, but it will output a really long JSON with a lot of info I don't quite understand, but it contains some interesting stuff about physics stuff, geometries, and the different bands available etc. which is interesting and relevant for those that have worked with time series and sensor data.

``` python
json.dumps(conn.describe_collection("SENTINEL2_L2A"), indent=2)
```

In order to connect to the server from Python we first need to authenticate using the ["OpenID Connect authentication flow"](https://documentation.dataspace.copernicus.eu/APIs/openEO/authentication.html). If you're calling this function for the first time it'll print some instructions on how to set up the authentication for future use, and every time after that initial setup, it'll connect automatically without requiring user input.

``` python
conn.authenticate_oidc()
```

    Authenticated using refresh token.
    <Connection to 'https://openeo.dataspace.copernicus.eu/openeo/1.2/' with OidcBearerAuth>

## Get the satellite data

{{< sidenote br=\"6em\" >}}
The ease of working [multidimensional matrices](https://se.mathworks.com/help/matlab/math/multidimensional-arrays.html) is still one of the biggest perks of MATLAB IMO
{{< /sidenote >}}

Finally, after all this time, we're ready to download some satellite data. The data we'll request through the `load_collection()` function will return a data type `openeo` has termed a ["data cube"](https://openeo.org/documentation/1.0/datacubes.html). For those familiar with MATLAB it most closely resembles a multidimensional matrix. For those unfamiliar with multidimensional matrices in MATLAB, it is basically a set of 2D images (x and y), made 3D by including a dimension to represent the different colors (e.g. red, green, blue, near-infrared), and a fourth dimension to represent time or the different timepoints. We won't actually get to directly inderact with the multidimensional matrix in Python directly, but we'll do it though a series of functions implemented by `openeo`.

{{< sidenote br=\"6em\" >}}
Different satellites will obviously offer different temporal limits based on when they were first put in orbit
{{< /sidenote >}}

The `load_connection()` function takes a number of arguments, most importantly the satellite we want to extract data from, the temporal limits, and the bands one wants to extract. We'll get all data from the beginning of 2016 to 2025. The bands are coded, and the satellite we use here has a range of bands available, each reflecting another aspect of what the satellite can measure. For a full overview of the band codes and what they represent, see the [full table here](https://docs.sentinel-hub.com/api/latest/data/sentinel-2-l2a/#available-bands-and-data). As mentioned, to calculate the NDVI we strictly speaking only need the red band (B04) and the near-infrared band (B08), but we'll load another data layer called the ["Scene Classification Map"](https://custom-scripts.sentinel-hub.com/custom-scripts/sentinel-2/scene-classification/) which classifies each pixel in the image into what is likely represented, e.g. water, vegetation, clouds, etc. We'll use that layer to mask some of the clouds from the data we'll analyse later. The final argument we'll specify is the `max_cloud_cover` which filters the images to only that have less than 20% cloud coverage in the image on the specified date for the specified area (we'll specify the area later).

``` python
cube_s2 = conn.load_collection(
  "SENTINEL2_L2A",
  temporal_extent=["2016-01-01", "2025-01-01"],
  bands=[
      "B04",  # Red
      "B08",  # Near-infrared
      "SCL",  # Scene classification
  ],
  max_cloud_cover=20,
)
```

So let's load the geographical area we want to look at. As I mentioned above, we can specify an area we want to analyse using the GeoJSON format and we already outlined two plots above. I've added another plot to the GeoJSON to test the replanting analysis pipeline. For the real use-case scenario, I've also drawn outlines around two areas classified as "grand cru" by the [AOC](https://en.wikipedia.org/wiki/Appellation_d'origine_contrôlée). It was hard finding a GeoJSON map of the areas classified as *grand cru*, but then I found this website: [vinsalsace.com](https://www.vinsalsace.com/en/carte/#!/grands-crus/). On this website you'll find a map created using [Leaflet](https://leafletjs.com) with the outlines for all the *grand cru* areas in the Alsace wine region. These outlines are made using GeoJSON and since Leaflet runs client-side the GeoJSON definitions are available in the source code of the website, so using a bit of digging we can download the GeoJSON definitions of two areas we're interested in ([Altenberg de Bergheim](https://www.vinsalsace.com/en/carte/#!/grands-crus/altenberg-de-bergheim/) and [Schlossberg](https://www.vinsalsace.com/en/carte/#!/grands-crus/schlossberg/)). `openeo` is a bit picky about which GeoJSON definitions it accepts (e.g. it doesn't work well with [MultiPolygons](https://docs.mapbox.com/android/java/api/libjava-geojson/7.3.1/com/mapbox/geojson/MultiPolygon.html)), so it seemed easiest to just load the GeoJSON from [vinsalsace.com](https://www.vinsalsace.com/en/carte/#!/grands-crus/) into [geojson.io](http://geojson.io) and trace them myself using the Polygon option for both the *grand cru* areas I wanted to compare. Why these two particular areas? No particular reason, they sounded cool I guess. This resulted in a single GeoJSON file with a 5 different polygons, three for the PoC case about replanting, and two that correspond to two different *grand cru* areas for the real use case. See the picture below for what areas the GeoJSON we'll use covers. You'll find the two little adjacent plots in the bottom left.

{{< image src=\"screenshot_geojson.png\" caption=\"Geographical boundaries in the GeoJSON used in the analysis. The two large areas in the bottom left and top right are the grand cru areas, and the three little specks in the middle are plots for the replanting examples.\" alt=\"Larger sattelite image showing the outlines of the plots of land of interest for the futher analysis\" >}}

We can load the file as we would any other JSON using the snippet below:

``` python
with open(os.path.join("data", "viticulture.geojson"), "r") as f:
    area_of_interest = json.load(f)
```

{{< sidenote >}}
We could also call them "features"
{{< /sidenote >}}

And we can parse the polygon names from the JSON using the code below, which will put it in a simple Pandas data frame for later use.

``` python
name_dict = {}
for i in range(len(area_of_interest["features"])):
  name_dict[i] = [
    area_of_interest["features"][i]["properties"]["name"]
  ]

df_feature_names = (
  pd.DataFrame(name_dict)
  .T.rename(columns={0: "feature_name"})
  .reset_index()
  .rename(columns={"index": "feature_index"})
)
df_feature_names
```

       feature_index           feature_name
    0              0    Replanting (case 1)
    1              1    Replanting (case 2)
    2              2   Replanting (control)
    3              3  Altenberg de Bergheim
    4              4            Schlossberg

Now we have the geographical boundaries set up, we can go ahead and calculate the NDVI.

## Calculate NDVI

Calculating the NDVI is exceedingly simple once we have defined the setup. Note that we haven't actually used any compute resources yet on the satellite data. Thus far we've just defined a "recipe" or the analysis we want to do. We'll get to actually running the computationally heavy stuff in just a bit. Right now we can just define what we want to do, define the recipe if you will. We'll simply extract the red and near-infrared color band from the data cube, and apply the formula as descibed earlier.

``` python
red = cube_s2.band("B04")
nir = cube_s2.band("B08")
ndvi = (nir - red) / (nir + red)
```

The `ndvi` variable is a `openeo` data cube object containing the calculation we'd like to do for the bands specified. But thus far we still haven't defined which area we'd like to limit the analysis to. For this we'll apply a function to the `ndvi` data cube object called `aggregate_spatial()`. We can supply the GeoJSON definitions of our areas of interest there, and we can specify that we want the mean NDVI for each of the areas (or features) in the GeoJSON. It is important to note that the `aggregate_spatial()` function can only handle three-dimensional data cubes, so you need to supply e.g. an NDVI (which reduces the bands to a single dimension instead of two, i.e. red and near-infrared).

When we initiate the computation later `openeo` will then calculate the mean NDVI for each of the 5 areas in the GeoJSON for each satellite image that matches the requirements specified (i.e. less then 30% cloud cover for the area of interest between 2016 and 2025). In essence this will be a time series of NDVI values for each of the 5 features since 2016. Instead of a data cube, this will now return a [vector cube](https://openeo.org/documentation/1.0/datacubes.html#aggregate).

``` python
ndvi_aggregated = ndvi.aggregate_spatial(
  geometries=area_of_interest, reducer="mean"
)
```

{{< sidenote br=\"10em\" >}}
Why do manual work if we can automate it in Python right?
{{< /sidenote >}}

Now we have defined everything we need to ask the server to actually perform the calculation. What we'll do is we'll apply the `execute_batch()` function to the `ndvi_aggregated` vector cube. We need to specify the format we want to get returned, and we'll choose the simple CSV format, and we'll provide a name which is not strictly necessary, but will help identify the jobs in the [online portal](https://openeo.dataspace.copernicus.eu/) among the batch jobs where you can see progress etc. and can download the output again if need be. We can also download the files directly in Python using the `get_results()` function and the `download_file()` function to save the CSV file to a local path.

``` python
job_raw = ndvi_aggregated.execute_batch(
  out_format="CSV",
  title="NDVI timeseries"
)
job_raw.get_results().download_file(
  os.path.join("data", "ndvi_timeseries_raw.csv")
)
```

When we have saved on our local drive we can load it again into a Pandas data frame and add the area names to each of the features specified in the GeoJSON file.

``` python
df = pd.read_csv(
  os.path.join("data", "ndvi_timeseries_raw.csv")
).reset_index()
df["date"] = pd.to_datetime(df["date"])
df.rename(columns={"band_unnamed": "ndvi"}, inplace=True)
df = df.merge(df_feature_names, on="feature_index")
```

We can then also have a look at what the file looks like:

``` python
df.sort_values(by="date").head(n=10)
```

         index                      date  ...      ndvi           feature_name
    267    267 2016-01-26 00:00:00+00:00  ...  0.402549    Replanting (case 2)
    266    266 2016-01-26 00:00:00+00:00  ...  0.454793    Replanting (case 1)
    265    265 2016-01-26 00:00:00+00:00  ...  0.485785            Schlossberg
    264    264 2016-01-26 00:00:00+00:00  ...  0.447628   Replanting (control)
    263    263 2016-01-26 00:00:00+00:00  ...  0.499384  Altenberg de Bergheim
    268    268 2016-03-09 00:00:00+00:00  ...  0.491412            Schlossberg
    269    269 2016-03-09 00:00:00+00:00  ...  0.469254   Replanting (control)
    522    522 2016-03-09 00:00:00+00:00  ...  0.482192    Replanting (case 1)
    523    523 2016-03-09 00:00:00+00:00  ...       NaN    Replanting (case 2)
    375    375 2016-05-05 00:00:00+00:00  ...  0.302739    Replanting (case 1)

    [10 rows x 5 columns]

Now we can finally look at some results. To do this we simply plot the time series for each of the features in the case-control scenario. Now, both a large and a small geographical region to analyse come with their advantages and disadvantages. For smaller areas the boundaries are usually more clearly defined and there's less chance of superfluous data to be included (e.g. roads) while in larger areas this is more likely, but any other confounders (e.g. shadow, temporary cover using plastic nets, etc.) may have a higher likelihood of averaging out across larger areas. So in the plot below there will be some outliers.

Since the raw data is rather noisy, we'll smooth out the time series using the local polynomial regression method ([LOESS](https://en.wikipedia.org/wiki/Local_regression)) implemented in `geom_smooth()` in the `{ggplot2}` package with a window width of 0.2 to get a plot that is easier to interpret and less affected by the outliers. Could we in theory just remove the outliers? Sure, but one shouldn't remove outliers simply because they are outliers. Outliers should only be removed if they are erroneous measurements, so unless we have clear evidence they are actual errors, we'll smooth them out.

``` r
py$df |>
  as_tibble() |>
  drop_na(ndvi) |>
  filter(
    str_detect(feature_name, "Replanting")
  ) |>
  mutate(
    feature_label = str_remove(feature_name, "Replanting \\("),
    feature_label = str_remove(feature_label, "\\)"),
    feature_label = str_to_sentence(feature_label)
  ) |>
  ggplot(aes(x = as.Date(date), y = ndvi, color = feature_label)) +
  geom_hline(yintercept = 0, linewidth = 0.75, color = "grey") +
  geom_line(linewidth = 1, alpha = 0.2, show.legend = FALSE) +
  geom_point(size = 2, alpha = 0.2, stroke = 0, show.legend = FALSE) +
  geom_smooth(
    method = "loess",
    span = 0.2,
    linewidth = 2,
    lineend = "round",
    se = FALSE,
    key_glyph = "point"
  ) +
  labs(
    title = "NDVI for the replanting areas",
    x = NULL,
    y = "NDVI",
    color = NULL
  ) +
  scale_x_date(breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  ggthemes::scale_color_tableau(
    guide = guide_legend(
      override.aes = list(size = 6, alpha = 1)
    )
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold"),
    legend.position = "inside",
    legend.position.inside = c(0.1, 0.2),
    legend.background = element_rect(fill = "#ffffff55")
  )
```

<img src="index.markdown_strict_files/figure-markdown_strict/plot-raw-time-series-1.png" width="768" />

So what can we see here? The red "control" plot follows a somewhat irregular cyclical path with a period of about 1 year. This is to be expected since (as the illustration in the introduction showed) leaves that turn yellow, red, and brown in autumn will have lower NDVI values than the green leaves do in the summer. This is particularly visible in years 2018 to 2021. We can also see that two "case" plots (blue and orange) show very clear dips. We specifically picked "case 1" because we knew it had been replanted in 2018. We can see in the plot what that looks like in terms of the NDVI values. So then it's also easy to infer when the "case 2" plot had been replanted, which appears to be in 2021. From both "case" plots it also appears that replanting in both these cases happened in late autumn (possibly right after one last harvest). From both plots we can also quite nicely see that it takes about 2 years for the plot to fall back into the regular pattern of the other plots.

## Mask non-vegetation

Earlier we specified in the `load_collection()` call that we only want images with a maximum of 20% cloud cover. This removed *a lot* of images obviously (even for an area with exceptionally nice weather). However, the images that have less less than 20% cloud cover aren't necessarily "clear blue" skies. The images may still contain up to 20% cloud coverage which confounds the analysis. Lowering the threshold may improve the quality of the images, but it will also drastically reduce the number of datapoints available. I find that 20% cloud cover is acceptable in most cases as long as we account for it by masking out the clouds.

{{< sidenote >}}
*SCL* stands for "scene classification" and is a synthetic layer used for various purposes besides masking clouds
{{< /sidenote >}}

For this we use the last of the bands we specified when the loaded the collection: the [SCL band](https://custom-scripts.sentinel-hub.com/custom-scripts/sentinel-2/scene-classification/). Looking at the [description](https://custom-scripts.sentinel-hub.com/custom-scripts/sentinel-2/scene-classification/) for the SCL band we'll find that bands 4 and 5 represent vegation or non-vegetation (e.g. sand and asphalt) areas respectively. The other 10 labels represent various forms of clouds, snow and ice, shadows, and technical errors. We can get a mask where all the areas where the SCL layer indicates clouds or other abnormalities is set to `True`.

``` python
scl = cube_s2.band("SCL")
mask = ~((scl == 4) | (scl == 5))
```

Then we "smooth out" the mask by applying some Gaussian blur to the mask. For this we'll first define a Gaussian window using the `scipy.signal.windows.gaussian()` function with a window size of 7 and a standard deviation of 1, which produces a single array of size (7, ) with a max of 1 in the middle. Then we'll use the `outer()` function to turn the array into a matrix of size (7, 7) that will serve as the kernel like in the simplified example below:

``` python
np.outer([1, 2, 1], [1, 2, 1])
```

    array([[1, 2, 1],
           [2, 4, 2],
           [1, 2, 1]])

{{< sidenote >}}
Simplified, the process of applying two functions to each other to obtain a third is called [*convolution*](https://en.wikipedia.org/wiki/Convolution)
{{< /sidenote >}}

We'll then apply this kernel to the mask to get apply the Gaussian mask to the clouds. We can then also apply some thresholding. This will then generate a mask that can be applied to the data cube.

``` python
# 2D gaussian kernel
g = scipy.signal.windows.gaussian(7, std=1)
kernel = np.outer(g, g)
kernel = kernel / kernel.sum()

mask = mask.apply_kernel(kernel)
#mask = mask > 0.1
```

Now we can apply the mask to the NDVI data and apply the geographical aggregation again.

``` python
ndvi_masked = ndvi.mask(mask)
timeseries_masked = ndvi_masked.aggregate_spatial(
  geometries=area_of_interest, reducer="mean"
)
```

... and run the batch job again on the masked dataset the same way as before and save it as well.

``` python
job = timeseries_masked.execute_batch(
out_format="CSV", title="Masked NDVI timeseries"
)
job.get_results().download_file(
  os.path.join("data", "ndvi_timeseries_masked.csv")
)
```

If we then load the dataset again through Python...

``` python
df_masked = pd.read_csv(
  os.path.join("data", "ndvi_timeseries_masked.csv")
).reset_index()
df_masked["date"] = pd.to_datetime(df_masked["date"])
df_masked.rename(columns={"band_unnamed": "ndvi"}, inplace=True)
df_masked = df_masked.merge(df_feature_names, on="feature_index")
```

we can plot the masked time series again for the replanting cases.

``` r
py$df_masked |>
  as_tibble() |>
  drop_na(ndvi) |>
  filter(
    str_detect(feature_name, "Replanting")
  ) |>
  mutate(
    feature_label = str_remove(feature_name, "Replanting \\("),
    feature_label = str_remove(feature_label, "\\)"),
    feature_label = str_to_sentence(feature_label)
  ) |>
  ggplot(aes(x = as.Date(date), y = ndvi, color = feature_label)) +
  geom_hline(yintercept = 0, linewidth = 0.75, color = "grey") +
  geom_line(linewidth = 1, alpha = 0.2, show.legend = FALSE) +
  geom_point(
    size = 2,
    alpha = 0.2,
    stroke = 0,
    show.legend = FALSE
  ) +
  geom_smooth(
    method = "loess",
    span = 0.2,
    linewidth = 2,
    lineend = "round",
    se = FALSE,
    key_glyph = "point"
  ) +
  labs(
    title = "Masked NDVI for the replanting areas",
    x = NULL,
    y = "NDVI",
    color = NULL
  ) +
  scale_x_date(breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  ggthemes::scale_color_tableau(
    guide = guide_legend(
      override.aes = list(size = 6, alpha = 1)
    )
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold"),
    legend.position = "inside",
    legend.position.inside = c(0.1, 0.2),
    legend.background = element_rect(fill = "white")
  )
```

<img src="index.markdown_strict_files/figure-markdown_strict/plot-masked-time-series-1.png" width="768" />

Unless you scroll back-and-forth the differences are probably not that easily spotted, but it is definitely notable in the reduction in the number of outliers (the slightly opaque lines). The NDVI values in those outliers are very low, indicating an absence of signal which may reflect that the sattelite captured the top side of white or grey clouds. Masking is therefore an essential tool in the data cleaning process of remote sensing data.

Now obviously the replanting case are fairly obvious since the change in NDVI values is so great when the entire plot is razed. Changes in real scenarios are (barring a natural disaster) a lot more subtle. The plot below shows the NDVI times series for the two grand cru areas. Remember that the geographical areas for these areas is a lot larger than the single plots we looked at earlier, so while it's possible that some plots were replanted it will be offset by the other plots in the grand cru area, also smoothing out the NDVI lines.

<details class="code-fold">
<summary>Show code</summary>

``` r
py$df_masked |>
  as_tibble() |>
  drop_na(ndvi) |>
  filter(
    str_detect(feature_name, "Replanting", negate = TRUE)
  ) |>
  mutate(
    feature_label = str_remove(feature_name, "Replanting \\("),
    feature_label = str_remove(feature_label, "\\)"),
  ) |>
  ggplot(aes(x = as.Date(date), y = ndvi, color = feature_label)) +
  geom_hline(yintercept = 0, linewidth = 0.75, color = "grey") +
  geom_line(linewidth = 1, alpha = 0.2, show.legend = FALSE) +
  geom_point(
    size = 2,
    alpha = 0.2,
    stroke = 0,
    show.legend = FALSE
  ) +
  geom_smooth(
    method = "loess",
    span = 0.2,
    linewidth = 2,
    lineend = "round",
    se = FALSE,
    key_glyph = "point"
  ) +
  labs(
    title = "Masked NDVI for the grand cru areas",
    x = NULL,
    y = "NDVI",
    color = NULL
  ) +
  scale_x_date(breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  ggthemes::scale_color_tableau(
    guide = guide_legend(
      override.aes = list(size = 6, alpha = 1)
    )
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold"),
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.2),
    legend.background = element_rect(fill = "#ffffff55")
  )
```

</details>

<img src="index.markdown_strict_files/figure-markdown_strict/plot-masked-time-series-grand-cru-1.png" width="768" />

The curves in the plot show that the changes here a lot more subtle than in the replanting cases. As for good or bad years we can see that the leaves were particularly green in 2019 and 2021. Interpreting these lines is where one would require domain knowledge on wine making, of which I have little. I'm suspecting the divergance around the end of 2024 is due to the nature of the smoothed line, since the raw data seems to overlap a lot more.

However, speculating is also fun and it's my blogpost so I can do what I want. There's hardly any peak in 2020 which may be due to lack of maintenance due to the pandemic perhaps. It also appears that the vines in Schlossberg peak a bit earlier than those in the Altenberg area, perhaps noticeable in 2019 and 2023. Now, it would be interesting to see if the rated quality of the wines or the production amount was in fact also greater in 2019 and 2023 compared to other years. This is where again the domain knowledge would play a role.

Nonetheless, this was a fun exercise where I got to play around with some satellite data. This application using NDVI is in active use by companies operating in [viticulture](https://doi.org/10.1038/s41598-021-81652-3) and other types of [agriculture](https://agrio.app/What-you-should-know-about-monitoring-NDVI-imagery/#:~:text=NDVI%20is%20a%20remote%20sensing,canopy%20density%2C%20and%20soil%20type.). Remote sensing has a number of applications outside agriculture too, most importantly in mapping the level of destruction in Gaza through for for example in mapping the [destruction of homes and increase in the number of tents](https://www.bloomberg.com/graphics/2025-gaza-damage-assessment/?accessToken=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzb3VyY2UiOiJTdWJzY3JpYmVyR2lmdGVkQXJ0aWNsZSIsImlhdCI6MTc0MTE5NDcyOCwiZXhwIjoxNzQxNzk5NTI4LCJhcnRpY2xlSWQiOiJTU05MR1hEV0xVNjgwMCIsImJjb25uZWN0SWQiOiJEMjk1RDc5RkRFQzc0QjQ4OEJDMUY1Mjc1Mjg1RUQ2NSJ9.zajGZ3H2oU-5gl4lwBAscWG60XYuRUPUgx12qsPREwY) and [mapping missile craters and again widespread destruction](https://doi.org/10.1093/nsr/nwae304). Other uses include [mapping glacial retreat](https://doi.org/10.5194/tc-18-5595-2024) through climate change, [deforestation](https://doi.org/10.1186/s13021-017-0078-9), and [tracking oil spills](https://doi.org/10.3390/s18010091). All these topics are perhaps a lot more interesting than the one discussed above, but my lack of domain knowledge would prevent me from doing any sort of analysis on this without doing some extensive research first. Nonetheless, this case on viticulture was fun to do, I hope it may bring you some insights the same way it did me.

[^1]: One of my favorite projects was analysis on video recordings from a camera inside an MRI scanner to see if the participants were asleep and analyzing the video recording as time series data in MATLAB
