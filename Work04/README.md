Assignment 4
================
Yutian Liu
11/19/2021

The learning objectives are to conduct data scraping and perform text
mining.

# HPC

## Problem 1: Make sure your code is nice

Rewrite the following R functions to make them faster. It is OK (and
recommended) to take a look at Stackoverflow and Google

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}
fun1alt <- function(mat) {
  rowSums(mat)
}
# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}
fun2alt <- function(mat) {
  t(apply(mat,1,cumsum))
}
# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)
```

``` r
# Test for the first
tf1 <- microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), check = "equivalent"
)
summary(tf1, unit = "relative")
```

    ##           expr      min       lq     mean   median       uq      max neval
    ## 1    fun1(dat) 1.689131 1.698926 1.688105 1.702043 1.705783 1.094114   100
    ## 2 fun1alt(dat) 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000   100

``` r
# Test for the second
tf2 <- microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat),check = "equivalent"
)
summary(tf2, unit = "relative")
```

    ##           expr      min       lq     mean   median      uq       max neval
    ## 1    fun2(dat) 1.637227 1.536388 1.413507 1.491129 1.47104 0.3893056   100
    ## 2 fun2alt(dat) 1.000000 1.000000 1.000000 1.000000 1.00000 1.0000000   100

The last argument, check = “equivalent”, is included to make sure that
the functions return the same result.

## Problem 2: Make things run faster with parallel computing

The following function allows simulating PI

``` r
sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

    ## [1] 3.132

In order to get accurate estimates, we can run this function multiple
times, with the following code:

``` r
# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##   9.538   0.081   9.624

Rewrite the previous code using parLapply() to make it run faster. Make
sure you set the seed using clusterSetRNGStream():

``` r
# Detect the number of available cores and create cluster
cl <- parallel::makeCluster(detectCores())
# Run parallel computation
 system.time({
  ans <- unlist(parLapply(cl,1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.141232

    ##    user  system elapsed 
    ##   0.006   0.000   1.314

``` r
# Close cluster
parallel::stopCluster(cl)
```

# SQL

Setup a temporary database by running the following chunk

``` r
# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```

## Question 1

How many many movies is there avaliable in each rating catagory.

``` sql
SELECT rating,
COUNT(*) AS "Films Count"
FROM film
GROUP BY rating 
```

| rating | Films Count |
|:-------|------------:|
| G      |         180 |
| NC-17  |         210 |
| PG     |         194 |
| PG-13  |         223 |
| R      |         195 |

5 records

It shows how many movies are available for each category.

## Question 2

What is the average replacement cost and rental rate for each rating
category.

``` sql
SELECT rating,
AVG(replacement_cost) AS "Average Replacement Cost",
AVG(rental_rate) AS "Average Rental Rate"
FROM film
GROUP BY rating
```

| rating | Average Replacement Cost | Average Rental Rate |
|:-------|-------------------------:|--------------------:|
| G      |                 20.12333 |            2.912222 |
| NC-17  |                 20.13762 |            2.970952 |
| PG     |                 18.95907 |            3.051856 |
| PG-13  |                 20.40256 |            3.034843 |
| R      |                 20.23103 |            2.938718 |

5 records

It shows that the average replacement cost and average rental rate for
each rating category

## Question 3

Use table film_category together with film to find the how many films
there are witth each category ID

``` sql
SELECT category_id,
COUNT(title) as "Films Count"
FROM film
INNER JOIN film_category
on film.film_id = film_category.film_id
GROUP BY category_id 
```

| category_id | Films Count |
|:------------|------------:|
| 1           |          64 |
| 2           |          66 |
| 3           |          60 |
| 4           |          57 |
| 5           |          58 |
| 6           |          68 |
| 7           |          62 |
| 8           |          69 |
| 9           |          73 |
| 10          |          61 |

Displaying records 1 - 10

It shows that number of filmes at each category id

## Question 4

Incorporate table category into the answer to the previous question to
find the name of the most popular category.

``` sql
SELECT ca.name,fc.category_id,
COUNT(title) as "Films Count"
FROM film_category as fc
JOIN film as fi
ON fc.film_id = fi.film_id
INNER JOIN category ca
on ca.category_id = fc.category_id
GROUP BY name
ORDER BY "Films Count" DESC
```

| name        | category_id | Films Count |
|:------------|------------:|------------:|
| Sports      |          15 |          74 |
| Foreign     |           9 |          73 |
| Family      |           8 |          69 |
| Documentary |           6 |          68 |
| Animation   |           2 |          66 |
| Action      |           1 |          64 |
| New         |          13 |          63 |
| Drama       |           7 |          62 |
| Sci-Fi      |          14 |          61 |
| Games       |          10 |          61 |

Displaying records 1 - 10

It shows that the most popular category is the Sports category with 74
films.

``` r
dbDisconnect(con)
```
