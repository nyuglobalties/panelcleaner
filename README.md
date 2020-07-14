
<!-- README.md is generated from README.Rmd. Please edit that file -->

# panelcleaner

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

Sometimes during data collection, survey structures may change over time
due to a litany of potential issues. In order to combine these datasets
together into a long format, panelcleaner attempts to identify as many
issues as possible prior to binding datasets together by thoroughly
documenting the state of each variable for each wave. Moreover, with the
assistance of
[`rcoder`](https://github.com/Global-TIES-for-Children/rcoder),
categorical data can be easily recoded into a single, homogenized coding
while not losing the labels or any associated metadata.

## Usage

panelcleaner is built around a *panel mapping file*, which is a
spreadsheet like this:

<table>

<thead>

<tr>

<th style="text-align:left;">

name\_1

</th>

<th style="text-align:left;">

description\_1

</th>

<th style="text-align:left;">

coding\_1

</th>

<th style="text-align:left;">

name\_2

</th>

<th style="text-align:left;">

description\_2

</th>

<th style="text-align:left;">

coding\_2

</th>

<th style="text-align:left;">

panel

</th>

<th style="text-align:left;">

homogenized\_name

</th>

<th style="text-align:left;">

homogenized\_coding

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

id

</td>

<td style="text-align:left;">

Participant ID

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

id

</td>

<td style="text-align:left;">

Partcipant ID

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

test\_panel

</td>

<td style="text-align:left;">

id

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

question1

</td>

<td style="text-align:left;">

The first question

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

q1

</td>

<td style="text-align:left;">

The first question

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

test\_panel

</td>

<td style="text-align:left;">

question\_1

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

question2

</td>

<td style="text-align:left;">

The second question

</td>

<td style="text-align:left;">

coding(code(‘Yes’, 1), code(‘No’, 0))

</td>

<td style="text-align:left;">

q2

</td>

<td style="text-align:left;">

The second question

</td>

<td style="text-align:left;">

coding(code(‘Yes’, ‘YES’), code(‘No’, ‘NO’))

</td>

<td style="text-align:left;">

test\_panel

</td>

<td style="text-align:left;">

question\_2

</td>

<td style="text-align:left;">

coding(code(‘Yes’, 1), code(‘No’, 0))

</td>

</tr>

</tbody>

</table>

The “name”, “description”, and “coding” columns refer to how the data
was structured in waves 1 & 2. The “code” columns are specific to
categorical variables, and the `coding()` objects (written out as
strings to be interpreted later) represent the categorical level
structure. See
[rcoder](https://github.com/Global-TIES-for-Children/rcoder) for more
information.

Once the mapping is defined, the homogenization process can begin. The
first step in the process is to gather waves of data into a *panel*
object:

``` r
wave1 <- tibble::tibble(
  id = LETTERS[1:4],
  question1 = sample(1:100, 4, replace = TRUE),
  question2 = sample(0:1, 4, replace = TRUE)
)

wave2 <- tibble::tibble(
  id = LETTERS[1:4],
  q1 = sample(1:100, 4, replace = TRUE),
  q2 = sample(c("NO", "YES"), 4, replace = TRUE)
)

(test_panel <- enpanel("test_panel", wave1, wave2))
#> <Unhomogenized Panel: 'test_panel'>
#> Waves: ['1', '2']
#> ID column: 'id'
#> Wave column: 'wave'
#> No mapping attached
#> 
#> '1' content:
#> # A tibble: 4 x 3
#>   id    question1 question2
#>   <chr>     <int>     <int>
#> 1 A            47         1
#> 2 B            14         0
#> 3 C            85         0
#> 4 D            84         1
#> 
#> '2' content:
#> # A tibble: 4 x 3
#>   id       q1 q2   
#>   <chr> <int> <chr>
#> 1 A        22 YES  
#> 2 B        32 NO   
#> 3 C        12 NO   
#> 4 D        59 NO
```

The panel name is “test\_panel”. Since multiple panels can be defined in
the same mapping file, a panel name must be defined to distinguish which
mapping belongs to which panel. Moreover, the panel is an
*unhomogenized* panel, which means that waves cannot yet be bound
together to form a single, long-format dataset.

The next step is to attach a panel mapping to the created panel:

``` r
panel_map <-
  panel_map_csv %>% 
  panel_mapping(waves = c(1, 2))

test_panel <-
  test_panel %>% 
  add_mapping(panel_map)

test_panel
#> <Unhomogenized Panel: 'test_panel'>
#> Waves: ['1', '2']
#> ID column: 'id'
#> Wave column: 'wave'
#> Attached mapping content:
#> # A tibble: 3 x 9
#>   name_1 description_1 coding_1 name_2 description_2 coding_2 panel
#> * <chr>  <chr>         <chr>    <chr>  <chr>         <chr>    <chr>
#> 1 id     Participant … <NA>     id     Partcipant ID <NA>     test…
#> 2 quest… The first qu… <NA>     q1     The first qu… <NA>     test…
#> 3 quest… The second q… coding(… q2     The second q… coding(… test…
#> # … with 2 more variables: homogenized_name <chr>, homogenized_coding <chr>
#> 
#> '1' content:
#> # A tibble: 4 x 3
#>   id    question1 question2
#>   <chr>     <int>     <int>
#> 1 A            47         1
#> 2 B            14         0
#> 3 C            85         0
#> 4 D            84         1
#> 
#> '2' content:
#> # A tibble: 4 x 3
#>   id       q1 q2   
#>   <chr> <int> <chr>
#> 1 A        22 YES  
#> 2 B        32 NO   
#> 3 C        12 NO   
#> 4 D        59 NO
```

`panel_mapping()` needs information about the specific waves mapped out,
so we pass the loaded CSV `data.frame` through `panel_mapping()` to
check that the CSV adheres to the panel mapping spec. Then we add the
mapping to the panel so that the homogenization process can refer to it.
Finally, we homogenize the panel.

``` r
homogenized_panel <-
  test_panel %>% 
  homogenize_panel() %>% 
  bind_waves()

homogenized_panel
#> <Homogenized Panel: 'test_panel'>
#> Waves: ['1', '2']
#> ID column: 'id'
#> Wave column: 'wave'
#> Attached mapping content:
#> # A tibble: 3 x 9
#>   name_1 description_1 coding_1 name_2 description_2 coding_2 panel
#> * <chr>  <chr>         <chr>    <chr>  <chr>         <chr>    <chr>
#> 1 id     Participant … <NA>     id     Partcipant ID <NA>     test…
#> 2 quest… The first qu… <NA>     q1     The first qu… <NA>     test…
#> 3 quest… The second q… coding(… q2     The second q… coding(… test…
#> # … with 2 more variables: homogenized_name <chr>, homogenized_coding <chr>
#> 
#> Content:
#> # A tibble: 8 x 4
#>   id    question_1 question_2 wave 
#>   <chr>      <int>      <dbl> <chr>
#> 1 A             47          1 1    
#> 2 B             14          0 1    
#> 3 C             85          0 1    
#> 4 D             84          1 1    
#> 5 A             22          1 2    
#> 6 B             32          0 2    
#> 7 C             12          0 2    
#> 8 D             59          0 2
```

To extract the underlying data, use `as.data.frame()` to convert the
homogenized panel to a `mapped_df`, a `data.frame`-like object that
carries the panel mapping as an attribute.

``` r
str(as.data.frame(homogenized_panel))
#> tibble [8 × 4] (S3: mapped_df/tbl_df/tbl/data.frame)
#>  $ id        : chr [1:8] "A" "B" "C" "D" ...
#>  $ question_1: int [1:8] 47 14 85 84 22 32 12 59
#>  $ question_2: num [1:8] 1 0 0 1 1 0 0 0
#>  $ wave      : chr [1:8] "1" "1" "1" "1" ...
#>  - attr(*, "mapping")= tibble [3 × 9] (S3: panel_mapping/tbl_df/tbl/data.frame)
#>   ..$ name_1            : chr [1:3] "id" "question1" "question2"
#>   ..$ description_1     : chr [1:3] "Participant ID" "The first question" "The second question"
#>   ..$ coding_1          : chr [1:3] NA NA "coding(code('Yes', 1), code('No', 0))"
#>   ..$ name_2            : chr [1:3] "id" "q1" "q2"
#>   ..$ description_2     : chr [1:3] "Partcipant ID" "The first question" "The second question"
#>   ..$ coding_2          : chr [1:3] NA NA "coding(code('Yes', 'YES'), code('No', 'NO'))"
#>   ..$ panel             : chr [1:3] "test_panel" "test_panel" "test_panel"
#>   ..$ homogenized_name  : chr [1:3] "id" "question_1" "question_2"
#>   ..$ homogenized_coding: chr [1:3] NA NA "coding(code('Yes', 1), code('No', 0))"
#>   ..- attr(*, "schema")=List of 5
#>   .. ..$ wave_name         : chr "name"
#>   .. ..$ wave_coding       : chr "coding"
#>   .. ..$ panel             : chr "panel"
#>   .. ..$ homogenized_name  : chr "homogenized_name"
#>   .. ..$ homogenized_coding: chr "homogenized_coding"
#>  - attr(*, "id_col")= chr "id"
#>  - attr(*, "waves_col")= chr "wave"
```

## Installation

As `panelcleaner` does not exist on CRAN yet, you can install the latest
development version of this package via:

``` r
# install.packages("remotes")
remotes::install_github("Global-TIES-for-Children/panelcleaner")
```

## Contributing

Please note that the `panelcleaner` project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.
