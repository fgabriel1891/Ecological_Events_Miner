# Ecological_event_miner
This is a work in progress to mine ecological events from published literature
---
output: html_document
---


# Welcome to Ecological Events Miner! 


To better understand large scale process it is necesary to develop of new hypothesis based on our current knowledge. A good portion of primary biodiversity knowledge (e.g. species interactions / species occurrences ) can be found in literature. Currently,  the PDF versions of published articles is the most common way to store and share literature. PDF's as they are "basically" a digital representation of the printed version posses a problem as it becomes difficult to scalate the process of a literature review. In this sence, this app have been developed with the idea to optimize the extraction of key information from published literature in ecological sciences.

# Instructions of Use 

- Clone or download the repository
- Replace the files from the '/articles/' folder with the collection of files you want to mine. Be sure to name the files appropiately so you do not loose track of which article are you mining from. 
- Provide a custom dictionary in the '/dic/' folder or use one of the thesaurus provided (Currently only the frugivory - thesaurus is available)
- Under the "Species Names" tab, you will find at the right side of the page a list of the species names recognized in the article, ordered by number of occurrences. In the left side, a wordcloud represents the 10 most frequent thesaurus terms found in the article. 
- Under the "Indexed version" tab will show the portions of the text which matches both the species names and the provided terms. A plot in the left side of the page will show the position of the matched portions of text inside the article. 

# Where to harvest articles? 

There is many ways to get articles. If you have the access to Academic Databases ( e.g. [Web of Science](https://login.webofknowledge.com/)) you can download as many articles you want and feed them to the app. If you don't have access to such databases (or just dont like the process of downloading articles one by one) you can search for articles in the following free databases: 

- Google Scholar 
- Biblat
- FreeFullText
- SciHub 

# How to create a thesaurus? 

# How does it work? 

# I have mined my data, now what? 

# Room for improvement? lets get in touch. 

There is always room for improvement, for now I have built this app using mostly spare time of my own. I'll be happy to discuss ideas and tools for improvement. I have some ideas in mind but their are not completely cooked yet... 

Still interested to get in touch? then send me an [email](mailto:fgabriel1891@gmail.com), find me on [GitHub](https://github.com/fgabriel1891) or [ResearchGate](https://www.researchgate.net/profile/Gabriel_Munoz2).

# Further reading

# Aknowledgements 

For helpful discussions and commentaries I am grateful to both [Emiel van Loon](https://staff.science.uva.nl/e.e.vanloon/index.html) and [Daniel Kissling](http://www.uva.nl/en/profile/k/i/w.d.kissling/w.d.kissling.html) from the University of Amsterdam. 

This apps uses tools from the [GNA (GlobalNamesArchitecture)](https://github.com/GlobalNamesArchitecture) implemented in the [taxize](https://github.com/ropensci/taxize) package. Credits to the developers of those initiatives. 

The development of this app comes initially from a literature review about tools mining information from literature, which I did under the supervision of [Emiel van Loon](https://staff.science.uva.nl/e.e.vanloon/index.html) as part of the requirements for my MSc degree in Ecology and Evolution at the University of Amsterdam (UvA). 







