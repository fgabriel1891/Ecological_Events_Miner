
# Welcome to Ecological Events Miner! 
This is a work in progress to mine ecological events from published literature

To better understand large scale process in Ecology it is necessary to develop of new hypothesis based on our current knowledge. A good portion of primary biodiversity knowledge (e.g. species interactions / species occurrences ) can be found in literature. Global biodiversity research depends on [high - level](https://en.wikipedia.org/wiki/Biological_organisation) concepts abstracted and put together to synthesize a variety of information independently produced by local observers, teams and institutions  working differently all over the world (Bisby 2000). This array of sources creates an intrinsic difficulty on "knowing what is where" and "comparing like with like" (Bisby 2000), fueling the need for integration of different ecological data and information over various geographical and environmental scales (Thuiller et al. 2013; Kissling and Schleuning 2015; Poissot et al. 2016).

Currently,  the PDF versions of published articles is the most common way to store and share literature. PDF's, as they are "basically" a digital representation of a printed version, make it diffucult to scalate the process of compiling published information on a determined topic. In this sence, this app have been developed with the idea to optimize the extraction of key information from published literature in ecological sciences, focusing on the extraction of multi-species interactions.

----------

# Instructions of Use 

- Clone or download the repository
- Replace the files from the '/articles/' folder with the collection of files you want to mine. Be sure to name the files appropiately so you do not loose track of which article are you mining from. 
- Provide a custom dictionary in the '/dic/' folder or use one of the thesaurus provided (Currently only the frugivory - thesaurus is available)
- Open the RContainer Folder and Run the App. 
- Under the "Species Names" tab, you will find at the right side of the page a list of the species names recognized in the article, ordered by number of occurrences. In the left side, a wordcloud represents the 10 most frequent thesaurus terms found in the article. 
- Under the "Indexed version" tab will show the portions of the text which matches both the species names and the provided terms. A plot in the left side of the page will show the position of the matched portions of text inside the article. 

----------

# How does it work? 

Ecological Events Miner makes use of R packages designed for text mining and base R functions. 
Functions from `fulltext` are used to perform the OCR. The `taxize` package is used to establish the API connection to the [Global Names Recognition and Discovery (GNRF)](rdrr.io) tool. The `stringr`is used for string manipulation and `wordcloud` to create the wordcloud of terms. 

----------

# Room for improvement? lets get in touch. 

There is always room for improvement, for now I have built this app using mostly spare time of my own. I'll be happy to discuss ideas and tools for improvement. I have some ideas in mind but their are not completely cooked yet... 

Still interested to get in touch? then send me an [email](mailto:fgabriel1891@gmail.com), find me on [GitHub](https://github.com/fgabriel1891) or [ResearchGate](https://www.researchgate.net/profile/Gabriel_Munoz2).

----------

# References and Further reading



Bisby, F. A. (2000). The quiet revolution: biodiversity informatics and the internet. Science, 289(5488), 2309-2312.

Haddaway, N. R., Collins, A. M., Coughlin, D., & Kirk, S. (2017). A rapid method to increase transparency and efficiency in web-based searches. Environmental Evidence, 6(1), 1.

Kissling, W. D., & Schleuning, M. (2015). Multispecies interactions across trophic levels at macroscales: retrospective and future directions. Ecography, 38(4), 346-357.


Poisot, T., Stouffer, D. B., & Gravel, D. (2015). Beyond species: why ecological interaction networks vary through space and time. Oikos, 124(3), 243-251.

Thuiller, W., Münkemüller, T., Lavergne, S., Mouillot, D., Mouquet, N., Schiffers, K., & Gravel, D. (2013). A road map for integrating eco‐evolutionary processes into biodiversity models. Ecology letters, 16(s1), 94-105.




