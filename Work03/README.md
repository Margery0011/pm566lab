Assignment3
================
yutian
2022-11-11

``` r
library(stringr)
library(rvest)
library(httr)
library(tidyverse)
library(xml2)
```

# API

``` r
# Downloading the website
website <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2+trial+vaccine")

# Finding the counts
counts <- xml2::xml_find_first(website, "/html/body/main/div[9]/div[2]/div[2]/div[1]/div[1]")

# Turning it into text
counts <- as.character(counts)
```

## Q1: How many papers were you able to find?

``` r
# Extracting the data using regex
stringr::str_extract(counts, "[0-9,]+")
```

    ## [1] "4,040"

There are 3997 papers under the term “sars-cov-2 trial vaccine.

## Q2: Using the list of pubmed ids you retrieved, download each papers’ details using the query parameter rettype = abstract. If you get more than 250 ids, just keep the first 250.

``` r
query_ids <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
  query = list(
    db     = "pubmed", 
    term   = "sars-cov-2 trial vaccine", 
    retmax = 250
    )
)
# Extracting the content of the response of GET
ids <- httr::content(query_ids)
```

``` r
ids <- as.character(ids)
ids <- stringr::str_extract_all(ids, "<Id>[0-9]+</Id>")[[1]]
ids <- stringr::str_remove_all(ids, "<Id>|</Id>")
```

``` r
publications <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
  query = list(
    db      = "pubmed",
    id      = paste(ids, collapse = ","),
    rettype = "abstract"
    )
)
publications <- httr::content(publications)
publications_txt <- as.character(publications)
```

``` r
pub_char_list <- xml2::xml_children(publications)
pub_char_list <- sapply(pub_char_list, as.character)
```

``` r
titles <- str_extract(pub_char_list, "<ArticleTitle>(\\n|.)+</ArticleTitle>")
titles <- str_remove_all(titles, "</?[[:alnum:]]+>")
titles <- str_replace_all(titles, "\\s+"," ")
```

``` r
abstracts <- str_extract(pub_char_list, "<Abstract>[[:print:][:space:]]+</Abstract>")
abstracts <- str_remove_all(abstracts, "</?[[:alnum:]- =\"]+>")
abstracts <- str_replace_all(abstracts, "[[:space:]]+", " ")
```

``` r
journals <- str_extract(pub_char_list, "<Title>(\\n|.)+</Title>")
journals<- str_remove_all(journals, "</?[[:alnum:]]+>")
journals<- str_replace_all(journals, "\\s+"," ")
```

``` r
dates <- str_remove_all(pub_char_list, "\\n") %>%
    str_extract("<PubDate>.*</PubDate>")
dates <-str_remove_all(dates, "</?[[:alnum:]]+>")
#remove new line and spaces
dates <- str_replace_all(dates, "\\s+"," ")
```

Finally, the database

``` r
database <- data.frame(
  PubMedId = ids,
  Title    = titles,
  Journal = journals,
  Abstract = abstracts,
  Date = dates
)
knitr::kable(database[1:2,], caption = "Some papers about sars-cov-2 trial vaccine")
```

| PubMedId | Title                                                                                                              | Journal                 | Abstract                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | Date        |
|:---------|:-------------------------------------------------------------------------------------------------------------------|:------------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:------------|
| 36342104 | SARS-CoV-2 Vaccine Immunogenicity in Patients with Gastrointestinal Cancer Receiving Systemic Anti-Cancer Therapy. | The oncologist          | Patients with gastrointestinal (GI) cancers have an increased risk of serious complications and death from SARS-CoV-2 infection. The immunogenicity of vaccines in patients with GI cancers receiving anti-cancer therapies is unclear. We conducted a prospective study to evaluate the prevalence of neutralizing antibodies in a cohort of GI cancer patients receiving chemotherapy following SARS-CoV-2 vaccination. Between September 2020 and April 2021, patients with cancer undergoing chemotherapy were enrolled. At baseline (day 0), days 28, 56, and 84, we assessed serum antibodies to SARS-CoV-2 spike (anti-S) and anti-nucleocapsid (anti-NP) and concomitantly assessed virus neutralization using a pseudovirus neutralization assay. Patients received either the Pfizer/BioNTech BNT162b2, or the Oxford/AstraZeneca ChAdOx1 vaccine. All 152 patients enrolled had a prior diagnosis of cancer; colorectal (n = 80, 52.6%), oesophagogastric (n = 38, 25.0%), and hepato pancreatic biliary (n = 22, 12.5%). Nearly all were receiving systemic anti-cancer therapy (99.3%). Of the 51 patients who did not receive a vaccination prior to, or during the study, 5 patients had detectable anti-NP antibodies. Ninety-nine patients received at least one dose of vaccine prior to, or during the study. Within 19 days following the first dose of vaccine, 30.0% had anti-S detected in serum which increased to 70.2% at days 20-39. In the 19 days following a second dose, anti-S positivity was 84.2% (32/38). However, pseudovirus neutralization titers (pVNT80) decreased from days 20 to 39. Despite the immunosuppressive effects of chemotherapy, 2 doses of SARS-CoV-2 vaccines are able to elicit a protective immune response in patients’ ongoing treatment for gastrointestinal cancers. Decreases in pseudoviral neutralization were observed after 20-39 days, re-affirming the current recommendation for vaccine booster doses. NCT04427280. © The Author(s) 2022. Published by Oxford University Press. | 2022 Nov 07 |
| 36341425 | Humoral and cellular immune responses to CoronaVac up to one year after vaccination.                               | Frontiers in immunology | Coronavac is a widely used SARS-CoV-2 inactivated vaccine, but its long-term immune response assessment is still lacking. We evaluated SARS-CoV-2-specific immune responses, including T cell activation markers, antigen-specific cytokine production and antibody response following vaccination in 53 adult and elderly individuals participating in a phase 3 clinical trial. Activated follicular helper T (Tfh), non-Tfh and memory CD4\<sup>+\</sup> T cells were detected in almost all subjects early after the first vaccine dose. Activated memory CD4\<sup>+\</sup> T cells were predominantly of central and effector memory T cell phenotypes and were sustained for at least 6 months. We also detected a balanced Th1-, Th2- and Th17/Th22-type cytokine production that was associated with response over time, together with particular cytokine profile linked to poor responses in older vaccinees. SARS-CoV-2-specific IgG levels peaked 14 days after the second dose and were mostly stable over one year. CoronaVac was able to induce a potent and durable antiviral antigen-specific cellular response and the cytokine profiles related to the response over time and impacted by the senescence were defined. Copyright © 2022 Costa, Correia, Marmorato, Dias, Thomazella, Cabral da Silva, de Oliveira, Gusmão, Ferrari, Freitas, Patiño, Grifoni, Weiskopf, Sette, Scharf, Kallás and Silveira.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | 2022        |

Some papers about sars-cov-2 trial vaccine

# Text Mining

A new dataset has been added to the data science data repository
<https://github.com/USCbiostats/data-science-data/tree/master/03_pubmed>.
The dataset contains 3241 abstracts from articles across 5 search terms.
Your job is to analyse these abstracts to find interesting insights.

``` r
library(tidytext)
library(dplyr)
library(ggplot2)
```

``` r
if (!file.exists("pubmed.csv")){
  download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/03_pubmed/pubmed.csv", "pubmed.csv", method="libcurl", timeout = 60)
}
pmb <- read.csv("pubmed.csv")
pmb <- as_tibble(pmb)
```

## 1. Tokenize the abstracts and count the number of each token. Do you see anything interesting? Does removing stop words change what tokens appear as the most frequent? What are the 5 most common tokens for each search term after removing stopwords?

``` r
pmb %>%
  unnest_tokens(token, abstract) %>%
  count(token, sort = TRUE) %>%
  top_n(5, n) %>%
  ggplot(aes(n, fct_reorder(token, n))) +
  labs(x = "Frequecy", y = "Top 5 tokens",title= "Top 5 tokens with stop words") +
  geom_col()
```

![](README_files/figure-gfm/top%205%20tokens%20with%20stop%20words-1.png)<!-- -->
ALL 5 most frequent words are stop words, so remove them to see the
difference

``` r
pmb %>%
  unnest_tokens(token, abstract) %>%
  anti_join(stop_words, by = c("token" = "word")) %>%
  count(token, sort = TRUE) %>%
  filter(!grepl(pattern = "^[0-9]+$", x = token)) %>%
  top_n(5, n) %>%
  ggplot(aes(n, fct_reorder(token, n))) +
  labs(x = "Frequency", y = "Top 5 tokens without stop words") +
  geom_col()
```

![](README_files/figure-gfm/Top%205%20tokens%20without%20stop%20words-1.png)<!-- -->

As a result, there is not any stop words and the Top5 most frequent
words are covid, patients, cancer , prostate and disease.

## 2. Tokenize the abstracts into bigrams. Find the 10 most common bigram and visualize them with ggplot2.

``` r
pmb %>%
    unnest_ngrams(bigram, abstract, n = 2) %>%
    separate(bigram, into = c("first", "second"), sep = " ",remove = FALSE) %>%
    anti_join(stop_words, by = c(first = "word")) %>%
    anti_join(stop_words, by = c(second = "word")) %>%
   filter(str_detect(first, "[a-z]") &
         str_detect(second, "[a-z]"))%>%
    unite(bigram, c("first", "second"), sep = " ") %>%
    count(bigram, sort = TRUE) %>%
    top_n(10, n) %>%
    ggplot(aes(x = n, y = fct_reorder(bigram, n))) + geom_col() +
    labs(y = "bigram", title = "Top 10 Most Frequent Bigrams without stopwords") 
```

![](README_files/figure-gfm/top%2010%20bigram-1.png)<!-- -->

As before, remove the stop words, there are top 10 most frequent bigrams

## 3. Calculate the TF-IDF value for each word-search term combination. (here you want the search term to be the “document”) What are the 5 tokens from each search term with the highest TF-IDF value? How are the results different from the answers you got in question 1?

``` r
pmb %>%
    unnest_tokens(token, abstract) %>%
    anti_join(stop_words, by = c(token = "word")) %>%
    filter(!grepl(pattern = "^[0-9]+$", x = token)) %>%
    group_by(term) %>%
    count(token, sort = TRUE) %>%
  bind_tf_idf(token, term, n) %>%
  arrange(term, desc(tf_idf)) %>%
  top_n(5, tf_idf) %>%
knitr::kable()
```

| term            | token           |    n |        tf |       idf |    tf_idf |
|:----------------|:----------------|-----:|----------:|----------:|----------:|
| covid           | covid           | 7275 | 0.0721733 | 1.6094379 | 0.1161585 |
| covid           | pandemic        |  800 | 0.0079366 | 1.6094379 | 0.0127734 |
| covid           | coronavirus     |  647 | 0.0064187 | 1.6094379 | 0.0103305 |
| covid           | sars            |  372 | 0.0036905 | 1.6094379 | 0.0059397 |
| covid           | cov             |  334 | 0.0033135 | 1.6094379 | 0.0053329 |
| cystic fibrosis | cf              |  625 | 0.0242089 | 0.9162907 | 0.0221823 |
| cystic fibrosis | fibrosis        |  867 | 0.0335825 | 0.5108256 | 0.0171548 |
| cystic fibrosis | cystic          |  862 | 0.0333889 | 0.5108256 | 0.0170559 |
| cystic fibrosis | cftr            |   86 | 0.0033311 | 1.6094379 | 0.0053613 |
| cystic fibrosis | sweat           |   83 | 0.0032149 | 1.6094379 | 0.0051742 |
| meningitis      | meningitis      |  429 | 0.0172462 | 1.6094379 | 0.0277567 |
| meningitis      | meningeal       |  219 | 0.0088040 | 1.6094379 | 0.0141695 |
| meningitis      | pachymeningitis |  149 | 0.0059899 | 1.6094379 | 0.0096405 |
| meningitis      | csf             |  206 | 0.0082814 | 0.9162907 | 0.0075882 |
| meningitis      | meninges        |  106 | 0.0042613 | 1.6094379 | 0.0068583 |
| preeclampsia    | eclampsia       | 2005 | 0.0266803 | 1.6094379 | 0.0429403 |
| preeclampsia    | preeclampsia    | 1863 | 0.0247907 | 1.6094379 | 0.0398992 |
| preeclampsia    | pregnancy       |  969 | 0.0128944 | 0.5108256 | 0.0065868 |
| preeclampsia    | maternal        |  797 | 0.0106056 | 0.5108256 | 0.0054176 |
| preeclampsia    | gestational     |  191 | 0.0025416 | 1.6094379 | 0.0040906 |
| prostate cancer | prostate        | 3832 | 0.0576943 | 1.6094379 | 0.0928554 |
| prostate cancer | androgen        |  305 | 0.0045921 | 1.6094379 | 0.0073906 |
| prostate cancer | psa             |  282 | 0.0042458 | 1.6094379 | 0.0068333 |
| prostate cancer | prostatectomy   |  215 | 0.0032370 | 1.6094379 | 0.0052098 |
| prostate cancer | castration      |  148 | 0.0022283 | 1.6094379 | 0.0035863 |

IN question 1, the top 5 common words are covid, patients, cancer,
prostate and disease After giving the weigh to the words, the tokens are
covid, pandemic, coronavirus, sars and cov which means word
“patient”,“prostate”, and “disease” are been replaced. It seems like
making more sense since them are more common words in all bio medical
fields instead of this specific filed.
