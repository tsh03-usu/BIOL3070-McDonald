Final Portfolio:
================
Tate McDonald
2025-10-30

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
- [STUDY QUESTION & HYPOTHESIS](#study-question--hypothesis)
  - [Question](#question)
  - [Hypothesis](#hypothesis)
  - [Predictions & Possible
    Visualizations](#predictions--possible-visualizations)
- [METHODS](#methods)
  - [Procedure](#procedure)
  - [First Analysis: Homework 10](#first-analysis-homework-10)
  - [Second Analysis:](#second-analysis)
- [DISCUSSION](#discussion)
  - [First Analysis: Interpretation](#first-analysis-interpretation)
  - [Second Analysis: Interpretation](#second-analysis-interpretation)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

# BACKGROUND

# STUDY QUESTION & HYPOTHESIS

## Question

## Hypothesis

## Predictions & Possible Visualizations

# METHODS

## Procedure

## First Analysis: Homework 10

``` r
# Load necessary packages
library(tidyverse)

# Import data
data <- read_csv("Global infant mortality rate with and without vaccines(Sheet1).csv")

# Check the column names to make sure they match
head(data)
```

    ## # A tibble: 6 × 3
    ##    Year `Vaccinated Mortality Rate` `Unvaccinated Mortality Rate`
    ##   <dbl>                       <dbl>                         <dbl>
    ## 1  1974                        10.1                          10.3
    ## 2  1975                         9.9                          10.2
    ## 3  1976                         9.8                          10  
    ## 4  1977                         9.5                           9.8
    ## 5  1978                         9.3                           9.8
    ## 6  1979                         9.1                           9.6

``` r
# Reshape data to long format
data_long <- data %>%
  pivot_longer(
    cols = c(`Vaccinated Mortality Rate`, `Unvaccinated Mortality Rate`),
    names_to = "Condition",
    values_to = "MortalityRate"
  )

# Plot both lines with linear regression fits
ggplot(data_long, aes(x = Year, y = MortalityRate, color = Condition)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  labs(
    title = "Global Infant Mortality Rates Over Time",
    x = "Year",
    y = "Infant Mortality Rate (per 1,000 births)",
    color = "Condition"
  ) +
  scale_color_manual(values = c("Vaccinated Mortality Rate" = "purple", "Unvaccinated Mortality Rate" = "green"))
```

![](TateMPortfolio_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
```

    ## <theme> List of 144
    ##  $ line                            : <ggplot2::element_line>
    ##   ..@ colour       : chr "black"
    ##   ..@ linewidth    : num 0.636
    ##   ..@ linetype     : num 1
    ##   ..@ lineend      : chr "butt"
    ##   ..@ linejoin     : chr "round"
    ##   ..@ arrow        : logi FALSE
    ##   ..@ arrow.fill   : chr "black"
    ##   ..@ inherit.blank: logi TRUE
    ##  $ rect                            : <ggplot2::element_rect>
    ##   ..@ fill         : chr "white"
    ##   ..@ colour       : chr "black"
    ##   ..@ linewidth    : num 0.636
    ##   ..@ linetype     : num 1
    ##   ..@ linejoin     : chr "round"
    ##   ..@ inherit.blank: logi TRUE
    ##  $ text                            : <ggplot2::element_text>
    ##   ..@ family       : chr ""
    ##   ..@ face         : chr "plain"
    ##   ..@ italic       : chr NA
    ##   ..@ fontweight   : num NA
    ##   ..@ fontwidth    : num NA
    ##   ..@ colour       : chr "black"
    ##   ..@ size         : num 14
    ##   ..@ hjust        : num 0.5
    ##   ..@ vjust        : num 0.5
    ##   ..@ angle        : num 0
    ##   ..@ lineheight   : num 0.9
    ##   ..@ margin       : <ggplot2::margin> num [1:4] 0 0 0 0
    ##   ..@ debug        : logi FALSE
    ##   ..@ inherit.blank: logi TRUE
    ##  $ title                           : <ggplot2::element_text>
    ##   ..@ family       : NULL
    ##   ..@ face         : NULL
    ##   ..@ italic       : chr NA
    ##   ..@ fontweight   : num NA
    ##   ..@ fontwidth    : num NA
    ##   ..@ colour       : NULL
    ##   ..@ size         : NULL
    ##   ..@ hjust        : NULL
    ##   ..@ vjust        : NULL
    ##   ..@ angle        : NULL
    ##   ..@ lineheight   : NULL
    ##   ..@ margin       : NULL
    ##   ..@ debug        : NULL
    ##   ..@ inherit.blank: logi TRUE
    ##  $ point                           : <ggplot2::element_point>
    ##   ..@ colour       : chr "black"
    ##   ..@ shape        : num 19
    ##   ..@ size         : num 1.91
    ##   ..@ fill         : chr "white"
    ##   ..@ stroke       : num 0.636
    ##   ..@ inherit.blank: logi TRUE
    ##  $ polygon                         : <ggplot2::element_polygon>
    ##   ..@ fill         : chr "white"
    ##   ..@ colour       : chr "black"
    ##   ..@ linewidth    : num 0.636
    ##   ..@ linetype     : num 1
    ##   ..@ linejoin     : chr "round"
    ##   ..@ inherit.blank: logi TRUE
    ##  $ geom                            : <ggplot2::element_geom>
    ##   ..@ ink        : chr "black"
    ##   ..@ paper      : chr "white"
    ##   ..@ accent     : chr "#3366FF"
    ##   ..@ linewidth  : num 0.636
    ##   ..@ borderwidth: num 0.636
    ##   ..@ linetype   : int 1
    ##   ..@ bordertype : int 1
    ##   ..@ family     : chr ""
    ##   ..@ fontsize   : num 4.92
    ##   ..@ pointsize  : num 1.91
    ##   ..@ pointshape : num 19
    ##   ..@ colour     : NULL
    ##   ..@ fill       : NULL
    ##  $ spacing                         : 'simpleUnit' num 7points
    ##   ..- attr(*, "unit")= int 8
    ##  $ margins                         : <ggplot2::margin> num [1:4] 7 7 7 7
    ##  $ aspect.ratio                    : NULL
    ##  $ axis.title                      : NULL
    ##  $ axis.title.x                    : <ggplot2::element_text>
    ##   ..@ family       : NULL
    ##   ..@ face         : NULL
    ##   ..@ italic       : chr NA
    ##   ..@ fontweight   : num NA
    ##   ..@ fontwidth    : num NA
    ##   ..@ colour       : NULL
    ##   ..@ size         : NULL
    ##   ..@ hjust        : NULL
    ##   ..@ vjust        : num 1
    ##   ..@ angle        : NULL
    ##   ..@ lineheight   : NULL
    ##   ..@ margin       : <ggplot2::margin> num [1:4] 3.5 0 0 0
    ##   ..@ debug        : NULL
    ##   ..@ inherit.blank: logi TRUE
    ##  $ axis.title.x.top                : <ggplot2::element_text>
    ##   ..@ family       : NULL
    ##   ..@ face         : NULL
    ##   ..@ italic       : chr NA
    ##   ..@ fontweight   : num NA
    ##   ..@ fontwidth    : num NA
    ##   ..@ colour       : NULL
    ##   ..@ size         : NULL
    ##   ..@ hjust        : NULL
    ##   ..@ vjust        : num 0
    ##   ..@ angle        : NULL
    ##   ..@ lineheight   : NULL
    ##   ..@ margin       : <ggplot2::margin> num [1:4] 0 0 3.5 0
    ##   ..@ debug        : NULL
    ##   ..@ inherit.blank: logi TRUE
    ##  $ axis.title.x.bottom             : NULL
    ##  $ axis.title.y                    : <ggplot2::element_text>
    ##   ..@ family       : NULL
    ##   ..@ face         : NULL
    ##   ..@ italic       : chr NA
    ##   ..@ fontweight   : num NA
    ##   ..@ fontwidth    : num NA
    ##   ..@ colour       : NULL
    ##   ..@ size         : NULL
    ##   ..@ hjust        : NULL
    ##   ..@ vjust        : num 1
    ##   ..@ angle        : num 90
    ##   ..@ lineheight   : NULL
    ##   ..@ margin       : <ggplot2::margin> num [1:4] 0 3.5 0 0
    ##   ..@ debug        : NULL
    ##   ..@ inherit.blank: logi TRUE
    ##  $ axis.title.y.left               : NULL
    ##  $ axis.title.y.right              : <ggplot2::element_text>
    ##   ..@ family       : NULL
    ##   ..@ face         : NULL
    ##   ..@ italic       : chr NA
    ##   ..@ fontweight   : num NA
    ##   ..@ fontwidth    : num NA
    ##   ..@ colour       : NULL
    ##   ..@ size         : NULL
    ##   ..@ hjust        : NULL
    ##   ..@ vjust        : num 1
    ##   ..@ angle        : num -90
    ##   ..@ lineheight   : NULL
    ##   ..@ margin       : <ggplot2::margin> num [1:4] 0 0 0 3.5
    ##   ..@ debug        : NULL
    ##   ..@ inherit.blank: logi TRUE
    ##  $ axis.text                       : <ggplot2::element_text>
    ##   ..@ family       : NULL
    ##   ..@ face         : NULL
    ##   ..@ italic       : chr NA
    ##   ..@ fontweight   : num NA
    ##   ..@ fontwidth    : num NA
    ##   ..@ colour       : chr "#4D4D4DFF"
    ##   ..@ size         : 'rel' num 0.8
    ##   ..@ hjust        : NULL
    ##   ..@ vjust        : NULL
    ##   ..@ angle        : NULL
    ##   ..@ lineheight   : NULL
    ##   ..@ margin       : NULL
    ##   ..@ debug        : NULL
    ##   ..@ inherit.blank: logi TRUE
    ##  $ axis.text.x                     : <ggplot2::element_text>
    ##   ..@ family       : NULL
    ##   ..@ face         : NULL
    ##   ..@ italic       : chr NA
    ##   ..@ fontweight   : num NA
    ##   ..@ fontwidth    : num NA
    ##   ..@ colour       : NULL
    ##   ..@ size         : NULL
    ##   ..@ hjust        : NULL
    ##   ..@ vjust        : num 1
    ##   ..@ angle        : NULL
    ##   ..@ lineheight   : NULL
    ##   ..@ margin       : <ggplot2::margin> num [1:4] 2.8 0 0 0
    ##   ..@ debug        : NULL
    ##   ..@ inherit.blank: logi TRUE
    ##  $ axis.text.x.top                 : <ggplot2::element_text>
    ##   ..@ family       : NULL
    ##   ..@ face         : NULL
    ##   ..@ italic       : chr NA
    ##   ..@ fontweight   : num NA
    ##   ..@ fontwidth    : num NA
    ##   ..@ colour       : NULL
    ##   ..@ size         : NULL
    ##   ..@ hjust        : NULL
    ##   ..@ vjust        : NULL
    ##   ..@ angle        : NULL
    ##   ..@ lineheight   : NULL
    ##   ..@ margin       : <ggplot2::margin> num [1:4] 0 0 6.3 0
    ##   ..@ debug        : NULL
    ##   ..@ inherit.blank: logi TRUE
    ##  $ axis.text.x.bottom              : <ggplot2::element_text>
    ##   ..@ family       : NULL
    ##   ..@ face         : NULL
    ##   ..@ italic       : chr NA
    ##   ..@ fontweight   : num NA
    ##   ..@ fontwidth    : num NA
    ##   ..@ colour       : NULL
    ##   ..@ size         : NULL
    ##   ..@ hjust        : NULL
    ##   ..@ vjust        : NULL
    ##   ..@ angle        : NULL
    ##   ..@ lineheight   : NULL
    ##   ..@ margin       : <ggplot2::margin> num [1:4] 6.3 0 0 0
    ##   ..@ debug        : NULL
    ##   ..@ inherit.blank: logi TRUE
    ##  $ axis.text.y                     : <ggplot2::element_text>
    ##   ..@ family       : NULL
    ##   ..@ face         : NULL
    ##   ..@ italic       : chr NA
    ##   ..@ fontweight   : num NA
    ##   ..@ fontwidth    : num NA
    ##   ..@ colour       : NULL
    ##   ..@ size         : NULL
    ##   ..@ hjust        : num 1
    ##   ..@ vjust        : NULL
    ##   ..@ angle        : NULL
    ##   ..@ lineheight   : NULL
    ##   ..@ margin       : <ggplot2::margin> num [1:4] 0 2.8 0 0
    ##   ..@ debug        : NULL
    ##   ..@ inherit.blank: logi TRUE
    ##  $ axis.text.y.left                : <ggplot2::element_text>
    ##   ..@ family       : NULL
    ##   ..@ face         : NULL
    ##   ..@ italic       : chr NA
    ##   ..@ fontweight   : num NA
    ##   ..@ fontwidth    : num NA
    ##   ..@ colour       : NULL
    ##   ..@ size         : NULL
    ##   ..@ hjust        : NULL
    ##   ..@ vjust        : NULL
    ##   ..@ angle        : NULL
    ##   ..@ lineheight   : NULL
    ##   ..@ margin       : <ggplot2::margin> num [1:4] 0 6.3 0 0
    ##   ..@ debug        : NULL
    ##   ..@ inherit.blank: logi TRUE
    ##  $ axis.text.y.right               : <ggplot2::element_text>
    ##   ..@ family       : NULL
    ##   ..@ face         : NULL
    ##   ..@ italic       : chr NA
    ##   ..@ fontweight   : num NA
    ##   ..@ fontwidth    : num NA
    ##   ..@ colour       : NULL
    ##   ..@ size         : NULL
    ##   ..@ hjust        : NULL
    ##   ..@ vjust        : NULL
    ##   ..@ angle        : NULL
    ##   ..@ lineheight   : NULL
    ##   ..@ margin       : <ggplot2::margin> num [1:4] 0 0 0 6.3
    ##   ..@ debug        : NULL
    ##   ..@ inherit.blank: logi TRUE
    ##  $ axis.text.theta                 : NULL
    ##  $ axis.text.r                     : <ggplot2::element_text>
    ##   ..@ family       : NULL
    ##   ..@ face         : NULL
    ##   ..@ italic       : chr NA
    ##   ..@ fontweight   : num NA
    ##   ..@ fontwidth    : num NA
    ##   ..@ colour       : NULL
    ##   ..@ size         : NULL
    ##   ..@ hjust        : num 0.5
    ##   ..@ vjust        : NULL
    ##   ..@ angle        : NULL
    ##   ..@ lineheight   : NULL
    ##   ..@ margin       : <ggplot2::margin> num [1:4] 0 2.8 0 2.8
    ##   ..@ debug        : NULL
    ##   ..@ inherit.blank: logi TRUE
    ##  $ axis.ticks                      : <ggplot2::element_blank>
    ##  $ axis.ticks.x                    : NULL
    ##  $ axis.ticks.x.top                : NULL
    ##  $ axis.ticks.x.bottom             : NULL
    ##  $ axis.ticks.y                    : NULL
    ##  $ axis.ticks.y.left               : NULL
    ##  $ axis.ticks.y.right              : NULL
    ##  $ axis.ticks.theta                : NULL
    ##  $ axis.ticks.r                    : NULL
    ##  $ axis.minor.ticks.x.top          : NULL
    ##  $ axis.minor.ticks.x.bottom       : NULL
    ##  $ axis.minor.ticks.y.left         : NULL
    ##  $ axis.minor.ticks.y.right        : NULL
    ##  $ axis.minor.ticks.theta          : NULL
    ##  $ axis.minor.ticks.r              : NULL
    ##  $ axis.ticks.length               : 'rel' num 0.5
    ##  $ axis.ticks.length.x             : NULL
    ##  $ axis.ticks.length.x.top         : NULL
    ##  $ axis.ticks.length.x.bottom      : NULL
    ##  $ axis.ticks.length.y             : NULL
    ##  $ axis.ticks.length.y.left        : NULL
    ##  $ axis.ticks.length.y.right       : NULL
    ##  $ axis.ticks.length.theta         : NULL
    ##  $ axis.ticks.length.r             : NULL
    ##  $ axis.minor.ticks.length         : 'rel' num 0.75
    ##  $ axis.minor.ticks.length.x       : NULL
    ##  $ axis.minor.ticks.length.x.top   : NULL
    ##  $ axis.minor.ticks.length.x.bottom: NULL
    ##  $ axis.minor.ticks.length.y       : NULL
    ##  $ axis.minor.ticks.length.y.left  : NULL
    ##  $ axis.minor.ticks.length.y.right : NULL
    ##  $ axis.minor.ticks.length.theta   : NULL
    ##  $ axis.minor.ticks.length.r       : NULL
    ##  $ axis.line                       : <ggplot2::element_blank>
    ##  $ axis.line.x                     : NULL
    ##  $ axis.line.x.top                 : NULL
    ##  $ axis.line.x.bottom              : NULL
    ##  $ axis.line.y                     : NULL
    ##  $ axis.line.y.left                : NULL
    ##  $ axis.line.y.right               : NULL
    ##  $ axis.line.theta                 : NULL
    ##  $ axis.line.r                     : NULL
    ##  $ legend.background               : <ggplot2::element_blank>
    ##  $ legend.margin                   : NULL
    ##  $ legend.spacing                  : 'rel' num 2
    ##  $ legend.spacing.x                : NULL
    ##  $ legend.spacing.y                : NULL
    ##  $ legend.key                      : <ggplot2::element_blank>
    ##  $ legend.key.size                 : 'simpleUnit' num 1.2lines
    ##   ..- attr(*, "unit")= int 3
    ##  $ legend.key.height               : NULL
    ##  $ legend.key.width                : NULL
    ##  $ legend.key.spacing              : NULL
    ##  $ legend.key.spacing.x            : NULL
    ##  $ legend.key.spacing.y            : NULL
    ##  $ legend.key.justification        : NULL
    ##  $ legend.frame                    : NULL
    ##  $ legend.ticks                    : NULL
    ##  $ legend.ticks.length             : 'rel' num 0.2
    ##  $ legend.axis.line                : NULL
    ##  $ legend.text                     : <ggplot2::element_text>
    ##   ..@ family       : NULL
    ##   ..@ face         : NULL
    ##   ..@ italic       : chr NA
    ##   ..@ fontweight   : num NA
    ##   ..@ fontwidth    : num NA
    ##   ..@ colour       : NULL
    ##   ..@ size         : 'rel' num 0.8
    ##   ..@ hjust        : NULL
    ##   ..@ vjust        : NULL
    ##   ..@ angle        : NULL
    ##   ..@ lineheight   : NULL
    ##   ..@ margin       : NULL
    ##   ..@ debug        : NULL
    ##   ..@ inherit.blank: logi TRUE
    ##  $ legend.text.position            : NULL
    ##  $ legend.title                    : <ggplot2::element_text>
    ##   ..@ family       : NULL
    ##   ..@ face         : NULL
    ##   ..@ italic       : chr NA
    ##   ..@ fontweight   : num NA
    ##   ..@ fontwidth    : num NA
    ##   ..@ colour       : NULL
    ##   ..@ size         : NULL
    ##   ..@ hjust        : num 0
    ##   ..@ vjust        : NULL
    ##   ..@ angle        : NULL
    ##   ..@ lineheight   : NULL
    ##   ..@ margin       : NULL
    ##   ..@ debug        : NULL
    ##   ..@ inherit.blank: logi TRUE
    ##  $ legend.title.position           : NULL
    ##  $ legend.position                 : chr "top"
    ##  $ legend.position.inside          : NULL
    ##  $ legend.direction                : NULL
    ##  $ legend.byrow                    : NULL
    ##  $ legend.justification            : chr "center"
    ##  $ legend.justification.top        : NULL
    ##  $ legend.justification.bottom     : NULL
    ##  $ legend.justification.left       : NULL
    ##  $ legend.justification.right      : NULL
    ##  $ legend.justification.inside     : NULL
    ##   [list output truncated]
    ##  @ complete: logi TRUE
    ##  @ validate: logi TRUE

## Second Analysis:

# DISCUSSION

## First Analysis: Interpretation

## Second Analysis: Interpretation

# CONCLUSION

# REFERENCES
