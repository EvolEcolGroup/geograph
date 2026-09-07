# Pairwise FST matrix for HGDP populations

A matrix of pairwise \\F\_{ST}\\ values between the HGDP populations,
used in the geoGraph case studies to relate genetic differentiation to
geographic and least-cost distances.

## Format

A numeric matrix with one row and one column per population. Row and
column names are population identifiers matching those in
[`hgdp`](https://evolecolgroup.github.io/geograph/dev/reference/hgdp.md).

## Examples

``` r
data(fst_hgdp)
dim(fst_hgdp)
#> [1] 51 51
fst_hgdp[1:5, 1:5]
#>           Karitiana   Maya Colombian   Pima  Surui
#> Karitiana    0.0000 0.0820    0.1161 0.1317 0.1906
#> Maya         0.0820 0.0000    0.0489 0.0593 0.1220
#> Colombian    0.1161 0.0489    0.0000 0.1038 0.1536
#> Pima         0.1317 0.0593    0.1038 0.0000 0.1729
#> Surui        0.1906 0.1220    0.1536 0.1729 0.0000
```
