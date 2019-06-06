# Natural Language Processor

## Overview

This app uses the Katz Backoff Model, an algorithm in natural language processing (NLP), to predict a user's next word based on previous input. The code is written in R, and its interface is a Shiny app, which can be accessed [here](https://vmprotani.shinyapps.io/text_predictor).

The app has been developed through the capstone project in [The Johns Hopkins University's Data Science specializiation on Coursera](https://www.coursera.org/specializations/jhu-data-science).

## Text Prediction

All code used to produce the app can be found in the `src` directory. Here is a brief overview of the main files:

File | Description
--- | ---
[raw_processor.R](https://github.com/vmprotani/text_predictor/blob/master/src/raw_processor.R) | Reads the raw text files (English only) and produces ngrams of 1 to 4 words
[data_explorer.Rmd](https://github.com/vmprotani/text_predictor/blob/master/src/data_explorer.Rmd) | Renders plots showing the most important ngrams coerced from the raw text
[katz_backoff.R](https://github.com/vmprotani/text_predictor/blob/master/src/katz_backoff.R) | Defines the functions used to run the Katz Backoff Model algorithm

Preliminary analysis of the ngrams from the corpus can be found [here](https://vmprotani.github.io/text_predictor/).

## Credits

The files used to check the corpuses for profane words come from the following sources:

* [English](https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/blob/master/en)

* [Finnish](https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/blob/master/fi)

* [German](https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/blob/master/de)

* [Russian](https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/blob/master/ru)

The ngram data used in this algorithm is derived from [the corpora](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip) provided by Johns Hopkins University and SwiftKey.
