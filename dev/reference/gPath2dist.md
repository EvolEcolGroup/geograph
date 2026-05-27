# Extract distances from a gPath object

This function extracts distances from a `gPath` object returned by
[`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBetween.md)
or
[`dijkstraFrom()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md).
Depending on `res.type`, it returns either a
[`dist`](https://rdrr.io/r/stats/dist.html) object or a numeric vector
of distances.

## Usage

``` r
gPath2dist(m, diag = FALSE, upper = FALSE, res.type = c("dist", "vector"))
```

## Arguments

- m:

  a `gPath` object obtained by
  [`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBetween.md)
  or
  [`dijkstraFrom()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md).

- diag:

  unused parameter added for consistency with
  [`as.dist()`](https://rdrr.io/r/stats/dist.html).

- upper:

  unused parameter added for consistency with
  [`as.dist()`](https://rdrr.io/r/stats/dist.html).

- res.type:

  a character string indicating what type of result should be returned:
  a `dist` object ('dist'), or a vector of distances ('vector'). Note
  that 'dist' should only be required for pairwise data, as output by
  dijkstraBetween (as opposed to dijkstraFrom).

## Value

Either a [`dist`](https://rdrr.io/r/stats/dist.html) object containing
pairwise distances between nodes when `res.type = "dist"`, or a numeric
vector of distances when `res.type = "vector"`.

## See also

Other dijkstra_methods:
[`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBetween.md),
[`dijkstraBuffer()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBuffer.md),
[`dijkstraFrom()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md)

## Examples

``` r
## for pairwise distances between multiple nodes you can use res.type = "dist"
# select a few populations from the HGDP dataset
hgdp.sub <- hgdp[getData(hgdp)$Population %in%
  c("Balochi", "BantuKenya", "Papuan", "Pima")]
hgdp.path <- dijkstraBetween(hgdp.sub) # compute shortest path
gPath2dist(hgdp.path, res = "dist") # extract as dist object
#>         1       2       3
#> 2  91.035                
#> 3  85.310 140.005        
#> 4  33.810 118.175  95.640
## for distances from a single origin node to multiple we set res.type = "vector"
#' # choose an origin node
start <- "24988"
hgdp.path <- dijkstraFrom(hgdp.sub, start) # compute shortest path from origin
gPath2dist(hgdp.path, res = "vector") # extract as vector of distances
#> 24988:16798  24988:7348 24988:40768 24988:30164 
#>      30.490     114.855      65.500      34.360 
```
