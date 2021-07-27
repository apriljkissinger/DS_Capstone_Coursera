# DS_Capstone_Coursera
Text Prediction algorithm and app built for the Capstone Project of the Coursera: John Hopkins Data Science Certification. Utilizes a quadri-gram model 
with Kneser-Ney smoothing and Good Turning Frequency Estimation.


The text prediction model TxT.lt was built using a corpus sampled from a corpora of news articles, blogs, and Twitter feeds. The sample corpus was cleaned, 
tokenized into n-grams where n={1:4}, and turned into document term matrices. The Kneser-Ney algorithm was built for smoothing and discounted using Good Turing 
frequency estimation. This was built for the Data Science Specialization Capstone Project given by the Johns Hopkins University on Coursera.

The corpora used to train and validate TxT.lt is a specific instance of the HC Corpora given to the class; the newest instance of the data set can be found here: 
http://corpora.epizy.com/corpora.html. This corpora is a set of web-scraped news articles, blogs and Twitter feeds. It is a well thought-out source as it covers 
the breadth of the English language; news articles tend to be non-stylistic and cover a particular set of jargon, blogs are somewhat stylistic, but are indicative 
of the way many write and include jargon from many sources, Twitter feeds are highly stylistic and will capture much of the common shorthand not captured by the 
news articles and blogs.

The model, validate and Shiny app scripts for this project are found in this repository
