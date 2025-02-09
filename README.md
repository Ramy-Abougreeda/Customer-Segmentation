---
title: "Company Segmentation"
author: "Ramy Abougreeda"
output: 
    rmarkdown::github_document:
    keeb_md: true
        df_print: paged
    theme: flatly
  pdf_document: default
  # pdf_document:
  #   toc: true
  # word_document:
  #   toc: true
  # html_document:
  #   toc: true
    # highlight: tango
    # code_folding: hide
    # df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
    echo = TRUE,
    message = FALSE,
    warning = FALSE
    )
```

# Project Summary

**My organization wants to know which companies are similar to each other to help in identifying potential customers of a SAAS software solution (e.g. Salesforce CRM or equivalent) in various segments of the market. The Sales Department is very interested in this analysis, which will help them more easily penetrate various market segments.**

I will be using stock prices in this analysis. I come up with a method to classify companies based on how their stocks trade using their daily stock returns (percentage movement from one day to the next). This analysis will help my organization determine which companies are related to each other (competitors and have similar attributes).

I will use a combination of `kmeans()` to find groups and `umap()` algorithms to visualize similarity of daily stock returns.

# Objectives

Apply my knowledge on K-Means and UMAP along with `dplyr`, `ggplot2`, and `purrr` to create a visualization that identifies subgroups in the S&P 500 Index. I will specifically apply:

-   Modeling: `kmeans()` and `umap()`
-   Iteration: `purrr`
-   Data Manipulation: `dplyr`, `tidyr`, and `tibble`
-   Visualization: `ggplot2`

# Libraries

```{r}
library(tidyverse)
library(tidyquant)
library(broom)
library(umap)
```

# Data

We will be using stock prices in this analysis. The `tidyquant` R package contains an API to retrieve stock prices. The following code is shown so you can see how I obtained the stock prices for every stock in the S&P 500 index. The files are saved in the `week_6_data` directory.

```{r, eval = FALSE}
# GET ALL STOCKS IN A STOCK INDEX (E.G. SP500)
sp_500_index_tbl <- tq_index("SP500")
sp_500_index_tbl

# PULL IN STOCK PRICES FOR EACH STOCK IN THE INDEX
sp_500_prices_tbl <- sp_500_index_tbl %>%
    select(symbol) %>%
    tq_get(get = "stock.prices")


# SAVING THE DATA

sp_500_prices_tbl %>% write_rds(path = "week_6_data/week_6_data/new_sp_500_prices_tbl.rds")

sp_500_index_tbl %>% write_rds(path = 'week_6_data/week_6_data/new_sp_500_index_tbl.rds')
```

We can read in the stock prices. The data is 1.2M observations. The most important columns for our analysis are:

-   `symbol`: The stock ticker symbol that corresponds to a company's stock price
-   `date`: The timestamp relating the symbol to the share price at that point in time
-   `adjusted`: The stock price, adjusted for any splits and dividends (we use this when analyzing stock data over long periods of time)

```{r}
# STOCK PRICES
new_sp_500_prices_tbl <- read_rds('week_6_data/week_6_data/new_sp_500_prices_tbl.rds')
    
new_sp_500_prices_tbl %>% head()
```

The second data frame contains information about the stocks the most important of which are:

-   `company`: The company name
-   `sector`: The sector that the company belongs to

```{r}
# SECTOR INFORMATION
new_sp_500_index_tbl <- read_rds("week_6_data/week_6_data/new_sp_500_index_tbl.rds")
new_sp_500_index_tbl %>% head()
```

# Question

<mark> Which stock prices behave similarly? </mark>

Answering this question helps us **understand which companies are related**, and we can use clustering to help us answer it!

Let's get started.

## Step 1 - Converting stock prices to a standardized format (daily returns)

We know that in order to compare the data, it needs to be standardized or normalized. Why? Because we cannot compare values (stock prices) that are of completely different magnitudes. In order to standardize, we will convert from adjusted stock price (dollar value) to daily returns (percent change from previous day). Here is the formula.

$$ 
return_{daily} = \frac{price_{i}-price_{i-1}}{price_{i-1}}
$$

First, what do we have? We have stock prices for every stock in the [SP 500 Index](https://finance.yahoo.com/quote/%5EGSPC?p=%5EGSPC), which is the daily stock prices for over 500 stocks. The data set is over 1.2M observations.

```{r}
new_sp_500_prices_tbl %>% glimpse()

```

My first task is to convert to a tibble named `new_sp_500_daily_returns_tbl` by performing some data transformations:

```{r}
# Applying data transformations

new_sp_500_daily_returns_tbl <- new_sp_500_prices_tbl %>% 
    
    select(symbol, date, adjusted) %>%
    
    filter(date >= ymd("2018-01-01")) %>%
    
    group_by(symbol) %>%
    mutate(lag_1 = lag(adjusted)) %>%
    ungroup() %>%
    
    filter(!is.na(lag_1)) %>%
    
    mutate(diff = adjusted - lag_1) %>%
    mutate(pct_return = diff / lag_1) %>%
    
    select(symbol, date, pct_return)

new_sp_500_daily_returns_tbl %>% head()
```

## Step 2 - Converting it to User-Item Format

The next step is to convert to a user-item format with the `symbol` in the first column and every other column the value of the *daily returns* (`pct_return`) for every stock at each `date`.

Now that we have the daily returns (percentage change from one day to the next), we can convert to a user-item format. The user in this case is the `symbol` (company), and the item in this case is the `pct_return` at each `date`.

Spreading the `date` column to get the values as percentage returns. Make sure to fill an `NA` values with zeros.

```{r}
# Convert to User-Item Format

stock_date_matrix_tbl <- new_sp_500_daily_returns_tbl %>% 
    spread(date, pct_return, fill = 0)

stock_date_matrix_tbl %>% head()
```

## Step 3 - Perform K-Means Clustering

Next, we'll perform **K-Means clustering**.

```{r}
# Create kmeans_obj for 4 centers

kmeans_obj <- stock_date_matrix_tbl %>% 
    
    select(-symbol) %>% 
    
    kmeans(centers = 4, nstart = 20)
```

Applying glance() to get the `tot.withinss`.
`tot.withinss` is the sum of the squared Euclidean distances between each data point and its corresponding cluster centroid.

```{r}
# Applying glance() to get the tot.withinss for centers = 4
broom::glance(kmeans_obj)$tot.withinss
```

## Step 4 - Find the optimal value of K

We'll use this **custom function** called `kmeans_mapper()` to iterate over many values of "k" using the `centers` argument:

```{r}
kmeans_mapper <- function(center = 3) {
    stock_date_matrix_tbl %>%
        select(-symbol) %>%
        kmeans(centers = center, nstart = 20)
    }
```

Apply the `kmeans_mapper()` and `glance()` functions iteratively using `purrr`.

```{r}
# Use purrr package to map
k_means_mapped_tbl <- tibble(centers = 1:15) %>% 
    mutate(k_means = centers %>% map(kmeans_mapper)) %>% 
    mutate(glance = k_means %>% map(glance)) 
```

Next, let's visualize the "tot.withinss" from the glance output as a ***Scree Plot***.

```{r}
# Visualize Scree Plot

k_means_mapped_tbl %>% unnest(glance) %>% 
    ggplot(aes(x = centers, y = tot.withinss)) +
    
    # Geometries
     geom_point(colour = '#2c3e50', size = 4) +
    geom_line(colour = '#2c3e50', linewidth = 1) +
    
    # Label
    ggrepel::geom_label_repel(aes(label = centers), color = '#2c3e50') +
    
    # Formatting
    theme_tq() +
    labs(title = "Skree Plot")
```

We can see that the Scree Plot becomes linear (constant rate of change) between 5 and 10 centers for K.

## Step 5 - Apply UMAP

Next, let's plot the `UMAP` 2D visualization to help us investigate cluster assignments.

```{r}
# Apply UMAP

umap_results <- stock_date_matrix_tbl %>% 
    select(-symbol) %>% 
    umap()
umap_results$layout %>% head()
```

Next, we want to combine the `layout` from the `umap_results` with the `symbol` column from the `stock_date_matrix_tbl`.

```{r}
# Convert umap results to tibble with symbols

umap_results_tbl <- as_tibble(umap_results$layout) %>% 
    set_names(c('X', 'Y')) %>% 
    bind_cols(stock_date_matrix_tbl %>% select(symbol)) %>% 
    select(symbol, X, Y)
umap_results_tbl %>% head()
```

Finally, let's make a quick visualization of the `umap_results_tbl`.

```{r}
# Visualize UMAP results
umap_results_tbl %>%
    
    ggplot(aes(X, Y)) +
    geom_point(color = "#2c3e50") +
    
    theme_tq() +
    labs(title = "UMAP Projection")
```

We can now see that we have some clusters. However, we still need to combine the K-Means clusters and the UMAP 2D representation.

## Step 6 - Combine K-Means and UMAP

Next, we combine the K-Means clusters and the UMAP 2D representation

First, pull out the K-Means for 4 Centers. Use this since beyond this value the Scree Plot flattens.

```{r}
# Getting the k_means_obj from the 4th center
k_means_obj <- k_means_mapped_tbl %>% 
    filter(centers == 4) %>% 
    pull(k_means) %>% 
    pluck(1)
```

Next, I'll combine the clusters from the `k_means_obj` with the `umap_results_tbl`.

```{r}
umap_kmeans_results_tbl <- k_means_obj %>% 
    augment(stock_date_matrix_tbl) %>%
    select(symbol, .cluster) %>%
    left_join(umap_results_tbl, by = "symbol") %>%
    left_join(new_sp_500_index_tbl %>% select(symbol, company, sector),
              by = "symbol")
umap_kmeans_results_tbl %>% head()
```

Plot the K-Means and UMAP results.

```{r}
# Visualize the combined K-Means and UMAP results
umap_kmeans_results_tbl %>% 
    ggplot(aes(X, Y, colour = .cluster)) +
    geom_point() +
    
    theme_tq() +
    scale_color_tq() +
    labs(
        title = "Companies Segmentation: 2D Projection",
        subtitle = "UMAP 2D Projection with K-Means Cluster Assignment"
    )

```
