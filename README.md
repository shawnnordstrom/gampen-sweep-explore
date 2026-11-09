# GaMPEN Sweep Visualization

Shiny app for visualizing hyperparameter sweep of GaMPEN, a Bayesian neural network
that estimates galaxy structure (bulge-to-total light ratio B/T, effective radius R_e, flux)
with uncertainty intervals.

## The data

`data/sweep.csv` has one row per training run. Each run fine-tunes a pretrained GaMPEN model on
HSC i-band images of about 14,000 galaxies. There are three starting models
("paths"): **A** is the same redshift in the r band, **B1** the same band at higher redshift, and
**B2** the same band at lower redshift. Runs vary learning rate, momentum, Nesterov, weight decay
and augmentation. Some recipes were retrained from scratch several times (`repeat_id`, NA for
single runs) as training isn't seeded. Each run is scored against GALFIT fits on the
validation set (709 galaxies) or the test set (3,539 galaxies). *Scatter* is the NMAD of prediction minus GALFIT (absolute for B/T,
fractional for flux and R_e). *68% coverage* is the percentage of galaxies whose GALFIT value
falls inside the model's 68% interval (ideal: 68.27). *Fraction thrown out* is the share of fits
whose interval is too wide to use. One extra row (`is_released = TRUE`) is the previously
released model for this sample, the baseline to beat. Missing values are `NA`.

## Running it

In R, from this folder:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "readr"))  # once
shiny::runApp()
```

To use a different CSV, set `Sys.setenv(SWEEP_CSV = "path/to/file.csv")` before `runApp()`.
