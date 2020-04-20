# covid19-corrector

Correction of underreported Covid-19 case and death counts using a reference country based on `Lachmann et al, 2020 medRxiv` pre-print, and via a multiplicative estimator for total deaths and cases. The estimator will be turned into a posterior prediction once data becomes available.

The app is hosted publicly at [https://pharmhax.shinyapps.io/covid-corrector-shiny/](https://pharmhax.shinyapps.io/covid-corrector-shiny/).

Writeups available at [NeuroSynergy](https://www.neurosynergy.io/articles/fixingcovid-19underreporting), and [Medium](https://medium.com/@maciejewski.matt/fixing-covid-19-case-number-and-death-toll-underreporting-bd9422c88cc4?sk=c2ae4a34e1957fdcc8c0dc6b18bb725a).

The analysis in this repository can be accessed using the R Shiny app in `covid-corrector-shiny/app.R`, which can be ran from the terminal via `R -e "shiny::runApp('covid-corrector-shiny/app.R')"`, or using RStudio.

Python code in the `corrector.py` module mostly based on the `Lachmann et al, 2020 medRxiv` pre-print. Please refer to `examples.ipynb` to see how to use this module.

