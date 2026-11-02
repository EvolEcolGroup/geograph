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
gPath2dist(m, diag = FALSE, upper = FALSE, res.type = NULL)
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

  deprecated parameter that is now ignored; the function automatically
  detects whether the input is from
  [`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBetween.md)
  or
  [`dijkstraFrom()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md)
  and returns the appropriate output type.

## Value

Depending on how the gPath object was constructed:

- a [`dist`](https://rdrr.io/r/stats/dist.html) object of pairwise
  distances, for
  [`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBetween.md)
  between all pairs of a set of nodes (e.g. on a
  [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object);

- a matrix with one row per `from` node and one column per `to` node,
  for
  [`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBetween.md)
  with distinct `from` and `to` (e.g.
  [`polygonBetween()`](https://evolecolgroup.github.io/geograph/dev/reference/polygonBetween.md));

- a named numeric vector, for
  [`dijkstraFrom()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md).

## See also

Other dijkstra_methods:
[`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBetween.md),
[`dijkstraBuffer()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBuffer.md),
[`dijkstraFrom()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md)

## Examples

``` r
## for pairwise distances between multiple a "dist" object is returned
# select a few populations from the HGDP dataset
hgdp.sub <- hgdp[getData(hgdp)$Population %in%
  c("Balochi", "BantuKenya", "Papuan", "Pima")]
hgdp.path <- dijkstraBetween(hgdp.sub) # compute shortest path
gPath2dist(hgdp.path) # extract as dist object
#>     1   2   3
#> 2 137        
#> 3 135 249    
#> 4  55 189 185
## for distances from a single origin node to multiple the output is a vector of distances
#' # choose an origin node
start <- "24988"
hgdp.path <- dijkstraFrom(hgdp.sub, start) # compute shortest path from origin
gPath2dist(hgdp.path) # extract as vector of distances
#> 24988:16798  24988:7348 24988:40768 24988:30164 
#>          53         189         148          75 
```
