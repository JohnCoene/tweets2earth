[![Build Status](https://travis-ci.org/JohnCoene/tweets2earth.svg?branch=master)](https://travis-ci.org/JohnCoene/tweets2earth)
[![Build status](https://ci.appveyor.com/api/projects/status/qw77mjh79yjeakos/branch/master?svg=true)](https://ci.appveyor.com/project/JohnCoene/tweets2earth/branch/master)

![gif](http://john-coene.com/img/thumbnails/tweets2earth.gif)

## tweets2earth ##

This package eases plotting tweets' location on Google Earth. See [site](http://john-coene.com/tweets2earth) for all details.

### Install ###

```R
devtools::install_github("JohnCoene/tweets2earth")
```

### Functions ###

* `plot2earth`: plot location of tweets, typically returned from the twitteR package, onto Google Earth. 
* `stream2earth`: Streams tweets' location on Google Earth using streamR package.