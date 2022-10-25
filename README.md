# Reporting-of-the-First-Intifada-by-the-NY-Times

In this report, we will try to study how the First Intifada was reported in the New York Times. The first Intifada was a period of protests by Palestinians against Israel’s occupation of Gaza and West Bank. It was informally spread across December 1987 and September 1993. This paper attempts to take forward a study done by Holly Jackson(https://www.holly-jackson.com/projects/nyt-content-analysis) that uses Machine Learning techniques to classify the systematic bias within NYTimes articles during first and second Intifadas. However, I will be more descriptive and will make use of text analysis techniques to gauge some idea about how news was reported from a paper of extreme importance.

We will scrape 2000 most popular new york times articles(articles are sorted in url through popularity) containing the word ‘Palestine’ for this analysis. The final dataset containts headlines, article abstracts, lead paragraphs, name of authors, publishing date and keywords. We will make use of the article api of NYtimes for scrapping. The dataset is stored in the repository as ‘articles.csv’. Since the keywords are stored as lists within each row of dataframe, ie keyword column contains a list for every article, we will first transform it so that only the top ranked keyword remains and store that in the df ‘keywords.csv’. The code for scrapping is provided below, however, owing to the large size of articles, the process of scrapping is commented out to ease out knitting the markdown file.
The main results are displayed in the 'visualizations' folder.

Credits to https://rpubs.com/hmgeiger/373949 for the process of scrapping articles and headlines by a term.
