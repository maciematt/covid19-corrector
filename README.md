# covid19-corrector

Correction of Covid-19 cases and deaths using a reference country based on `Lachmann et al, 2020 medRxiv` pre-print, and via a multiplicative estimator for total deaths and cases. The estimator will be turned into a posterior prediction once data becomes available.

The app is hosted publicly at [https://pharmhax.shinyapps.io/covid-corrector-shiny/](https://pharmhax.shinyapps.io/covid-corrector-shiny/).

Writeups available at [NeuroSynergy](https://www.neurosynergy.io/articles/fixingcovid-19underreporting), and [Medium](https://medium.com/@maciejewski.matt/towards-correcting-covid-19-case-numbers-and-death-toll-underreporting-via-coding-36340e0f8486).

The analysis in this repository can be accessed using the R Shiny app in `covid-corrector-shiny/app.R`, which can be ran from the terminal via `R -e "shiny::runApp('covid-corrector-shiny/app.R')"`, or using RStudio.

Python code in the `corrector.py` module mostly based on the `Lachmann et al, 2020 medRxiv` pre-print. Please refer to `examples.ipynb` to see how to use this module.

