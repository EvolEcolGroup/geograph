# Pairwise FST matrix for HGDP populations

A matrix of pairwise \\F\_{ST}\\ values between the HGDP populations,
used in the geoGraph case studies to relate genetic differentiation to
geographic and least-cost distances.

## Format

A numeric matrix with one row and one column per population. Row and
column names are population identifiers matching those in
[`hgdp`](https://evolecolgroup.github.io/geograph/reference/hgdp.md).

## Examples

``` r
data(fst_hgdp)
dim(fst_hgdp)
#> [1] 51 52
fst_hgdp[1:5, 1:5]
#> # A tibble: 5 × 5
#>   ...1      Karitiana   Maya Piapoco   Pima
#>   <chr>         <dbl>  <dbl>   <dbl>  <dbl>
#> 1 Karitiana     0     0.082   0.116  0.132 
#> 2 Maya          0.082 0       0.0489 0.0593
#> 3 Piapoco       0.116 0.0489  0      0.104 
#> 4 Pima          0.132 0.0593  0.104  0     
#> 5 Surui         0.191 0.122   0.154  0.173 
```
