
<!-- README.md is generated from README.Rmd. Please edit that file -->

# birdseyeview

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/birdseyeview)](https://CRAN.R-project.org/package=birdseyeview)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of birdseyeview is to make it easy to create the types of maps,
plots, and tables used for community plans.

This package was initially designed to use the [overedge
package](https://elipousson.github.io/overedge/) and data packages like
[mapbaltimore](https://elipousson.github.io/mapbaltimore/) or
[bcpss](https://elipousson.github.io/bcpss/) to create reproducible maps
and tables for a range of needs. The {overedge} package has since been
superseded by sfext, getdata, and maplayer. maplayer incorporated most
of the mapping functions creating for birdseyeview. These duplicative
functions were removed from birdseyeview in Setember 2022.

## Installation

You can install the development version of birdseyeview like so:

``` r
remotes::install_github("elipousson/birdseyeview")
```

## Example

``` r
library(birdseyeview)
library(getdata)
library(sfext)
library(maplayer)
```

### Make tables

``` r
parks <-
  getdata::get_location_data(
    data = "parks",
    package = "mapbaltimore"
  )

parks %>%
  dplyr::slice_head(n = 4) %>%
  dplyr:::select(name, address, park_district, acres, geometry) %>%
  dplyr::group_by(park_district) %>%
  gt::gt() %>%
  gt_sf_rows(fill = "forestgreen", color = "lightgreen", size = 6)
```

<div id="zntuztalnp" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#zntuztalnp .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#zntuztalnp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zntuztalnp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#zntuztalnp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#zntuztalnp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zntuztalnp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zntuztalnp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#zntuztalnp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#zntuztalnp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zntuztalnp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zntuztalnp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#zntuztalnp .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#zntuztalnp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#zntuztalnp .gt_from_md > :first-child {
  margin-top: 0;
}

#zntuztalnp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zntuztalnp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#zntuztalnp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#zntuztalnp .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#zntuztalnp .gt_row_group_first td {
  border-top-width: 2px;
}

#zntuztalnp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zntuztalnp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#zntuztalnp .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#zntuztalnp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zntuztalnp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zntuztalnp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zntuztalnp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zntuztalnp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zntuztalnp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zntuztalnp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zntuztalnp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zntuztalnp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zntuztalnp .gt_left {
  text-align: left;
}

#zntuztalnp .gt_center {
  text-align: center;
}

#zntuztalnp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zntuztalnp .gt_font_normal {
  font-weight: normal;
}

#zntuztalnp .gt_font_bold {
  font-weight: bold;
}

#zntuztalnp .gt_font_italic {
  font-style: italic;
}

#zntuztalnp .gt_super {
  font-size: 65%;
}

#zntuztalnp .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#zntuztalnp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#zntuztalnp .gt_indent_1 {
  text-indent: 5px;
}

#zntuztalnp .gt_indent_2 {
  text-indent: 10px;
}

#zntuztalnp .gt_indent_3 {
  text-indent: 15px;
}

#zntuztalnp .gt_indent_4 {
  text-indent: 20px;
}

#zntuztalnp .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col">name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col">address</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col">acres</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col">map</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="4" class="gt_group_heading">Clifton</td>
    </tr>
    <tr class="gt_row_group_first"><td class="gt_row gt_left">Abell Open Space</td>
<td class="gt_row gt_left">301 E 32nd St</td>
<td class="gt_row gt_center">0.1687215 [acres]</td>
<td class="gt_row gt_center"><img src="data:image/png;base64,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" style="height:100px;"></td></tr>
    <tr><td class="gt_row gt_left">Adams Park</td>
<td class="gt_row gt_left">1530 Montpelier St</td>
<td class="gt_row gt_center">0.6228435 [acres]</td>
<td class="gt_row gt_center"><img src="data:image/png;base64,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" style="height:100px;"></td></tr>
    <tr><td class="gt_row gt_left">Alhambra Park</td>
<td class="gt_row gt_left">5200 Alhambra Ave</td>
<td class="gt_row gt_center">0.9927695 [acres]</td>
<td class="gt_row gt_center"><img src="data:image/png;base64,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" style="height:100px;"></td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="4" class="gt_group_heading">Gwynns Falls</td>
    </tr>
    <tr class="gt_row_group_first"><td class="gt_row gt_left">Alexander Odum Park</td>
<td class="gt_row gt_left">3111 Presstman St</td>
<td class="gt_row gt_center">1.3572063 [acres]</td>
<td class="gt_row gt_center"><img src="data:image/png;base64,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" style="height:100px;"></td></tr>
  </tbody>
  
  
</table>
</div>

``` r
park_photos <-
  getdata::get_flickr_photos(
    user_id = "baltimoreheritage",
    tags = "druidhillpark",
    img_size = "m",
    sort = "date-posted",
    per_page = 20
  )

park_photos[1:6, ] %>%
  dplyr::select(title, datetaken, image_height, image_width, image_url) %>%
  tbl_photo_key(photo_col = "image_url", orientation = "landscape", number = TRUE)
```

### Make maps

``` r
library(ggplot2)
```

``` r
ggplot() +
  maplayer::layer_location_context(
    data = parks[245, ],
    fill = "green",
    context = parks,
    context_params = list(fill = "forestgreen", color = "gray60", alpha = 1)
  )
```

<img src="man/figures/README-layer_show_context-1.png" width="100%" />

``` r
# make_group_layers has been dropped from birdseyeview but isn't available in maplayer yet
park_district_layers <-
  make_group_layers(
    data = parks %>% sf::st_centroid(),
    mapping = aes(color = name),
    groupname_col = "park_district"
  )

clifton_district <-
  getdata::get_location(
    type = "park_districts",
    package = "mapbaltimore",
    name = "Clifton"
  )

ggplot() +
  park_district_layers[[1]] +
  guides(color = "none") +
  layer_show_location(
    data = clifton_district
  ) +
  theme_void()
```
