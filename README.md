# geoGraph

<!-- badges: start -->
[![R-CMD-check
main](https://img.shields.io/github/checks-status/EvolEcolGroup/geograph/main?label=main&logo=GitHub)](https://github.com/EvolEcolGroup/geograph/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check
dev](https://img.shields.io/github/checks-status/EvolEcolGroup/geograph/dev?label=dev&logo=GitHub)](https://github.com/EvolEcolGroup/geograph/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/EvolEcolGroup/geograph/branch/dev/graph/badge.svg?token=NflUsWlnQR)](https://app.codecov.io/gh/EvolEcolGroup/geograph)
<!-- badges: end -->

`geoGraph` aims at implementing graph approaches for geographic data.
In `geoGraph`, a given geographic area is modelled by a fine regular grid, where each vertex
has a set of spatial coordinates and a set of attributes, which can be for instance habitat
descriptors, or the presence/abundance of a given species.
'Travelling' within the geographic area can then be easily modelled as moving between connected vertices.
The cost of moving from one vertex to another can be defined according to attribute values, which
allows for instance to define friction routes based on habitat.

`geoGraph` harnesses the full power of graph algorithms implemented in R by the *graph*
and *RBGL* (R Boost Graph Library) packages.
In particular, RBGL is an interface between R and the comprehensive *Boost Graph Library* in C++,
which provides fast and efficient implementations of a wide range of graph algorithms.
Once we have defined frictions for an entire geographic area, we can easily, for instance, find the least
costs path from one location to another, or find the most parsimonious way of connecting a set of locations.


Interfacing spatial data and graphs can be a complicated task.
The purpose of `geoGraph` is to provide tools to achieve and simplify this 'preliminary' step.
This is achieved by defining new classes of objects which are essentially geo-referenced graphs
with node attributes (`gGraph` objects), and interfaced spatial data (`gData` objects).

## Installation

You can install the development version of `geoGraph` from [GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("EvolEcolGroup/geograph/")
```

## How the package works

A detailed introduction to the functionalities of `geoGraph` is found in the
overview article of its website.

