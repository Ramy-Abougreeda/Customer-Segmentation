---
title: "Customer Segmentation"
author: "Ramy Abougreeda"
output:
  html_document:
    df_print: paged
    theme: flatly
  pdf_document: default
params:
  interactive: TRUE
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
    echo = FALSE,
    message = FALSE,
    warning = FALSE,
    out.width = "100%",
    fig.align = "center")
```

```{r}
library(tidyverse)
library(tidyquant)
library(broom)
library(umap)
library(ggrepel)
library(plotly)
```

```{r}
bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")
```

```{r}
source("../00_scripts/plot_customer_segmentation.R")
```

## Problem Statement

Marketing department would like to increase email campaign engagement by segmenting the customer-base using their buying habits.

## Solution Summary

**TODO: INSERT ANALYSIS AND HEAT MAP**

## Customer Preferences

### Heat Map

Our customer-base consists of 30 bike shops. Several customers have purchasing 
preferences for road or mountain bikes based on the proportion of bikes purchased 
by category 1 and category 2.

```{r, fig.height=12}
# PLOT CUSTOMER HEAT MAP
plot_customer_heatmap(params$interactive)
```

### Customer Segmentation

This is a visual representation of customer relationships, highlighting four distinct groups that represent key segments within the customer base.     

```{r, fig.height=5, fig.width=7}
# PLOT CUSTOMER SEGMENTS
plot_customer_segments(k = 4, seed = 123, interactive = params$interactive)

```

### Customer Preferences By Segment

**TODO: INSERT ANALYSIS AND HEAT MAP**

```{r, fig.height=8}
# PLOT CUSTOMER BEHAVIOR
plot_customer_behavior_by_cluster()
```
