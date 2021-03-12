# Disclaimer: (As of Mar. 12th I'm roughly 1/3 the way through this project by steps, probably 1/2 by time)
Some content, topics, and/or language within the data is found to be highly offensive to some/many demographics. However, the analysis contained in this work is solely for the purpose of learning and showcasing an application of natural language processing (NLP) in a context some may be interested in. 

# About the project
Currently, I'm using R for the entire project (for more details, see "About the R packages" below). This repo will contain all the information to create a Shiny app to recommend a comic to the user based on user input (comedian names, topics, and [maybe] genre). On occassion, some visuals will be provided to demonstrate key ideas in analysis. 

# About the files
We have 361 unique monologue transcripts from scriptsfromtheloft.com (there are 370 scripts, 5 are password-locked and 4 duplicates). The raw text files (14 MB) are provided until I have made ready the files for analysis.

# About the R packages (since this is in-progress, I'll whittle this down)
For web scraping, rvest; for data munging: tidyverse, grep; for text-cleaning: grep; for text-mining: tm, tidytext, openNLP; for text analysis/topic modeling: stm, lsa, lda, topicmodels, stm; for visualizations/graphs: shiny, ggplot2, wordcloud; and for the unnecessary activities/tracking/timing: pbapply.

