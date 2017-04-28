
---
output: html_document
---

# Welcome to Ecological Events Miner! 
This is a work in progress to mine ecological events from published literature

To better understand large scale process in Ecology it is necessary to develop of new hypothesis based on our current knowledge. A good portion of primary biodiversity knowledge (e.g. species interactions / species occurrences ) can be found in literature. Global biodiversity research depends on [high - level](https://en.wikipedia.org/wiki/Biological_organisation) concepts abstracted and put together to synthesize a variety of information independently produced by local observers, teams and institutions  working differently all over the world (Bisby 2000). This array of sources creates an intrinsic difficulty on "knowing what is where" and "comparing like with like" (Bisby 2000), fueling the need for integration of different ecological data and information over various geographical and environmental scales (Thuiller et al. 2013; Kissling and Schleuning 2015; Poissot et al. 2016).

Currently,  the PDF versions of published articles is the most common way to store and share literature. PDF's, as they are "basically" a digital representation of a printed version, make it diffucult to scalate the process of compiling published information on a determined topic. In this sence, this app have been developed with the idea to optimize the extraction of key information from published literature in ecological sciences, focusing on the extraction of multi-species interactions.

----------

# Instructions of Use 

- Clone or download the repository
- Replace the files from the '/articles/' folder with the collection of files you want to mine. Be sure to name the files appropiately so you do not loose track of which article are you mining from. 
- Provide a custom dictionary in the '/dic/' folder or use one of the thesaurus provided (Currently only the frugivory - thesaurus is available)
- Under the "Species Names" tab, you will find at the right side of the page a list of the species names recognized in the article, ordered by number of occurrences. In the left side, a wordcloud represents the 10 most frequent thesaurus terms found in the article. 
- Under the "Indexed version" tab will show the portions of the text which matches both the species names and the provided terms. A plot in the left side of the page will show the position of the matched portions of text inside the article. 

----------





