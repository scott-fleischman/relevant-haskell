# relevant-haskell
Haskell code for https://github.com/o19s/relevant-search-book

[![Build Status](https://travis-ci.org/scott-fleischman/relevant-haskell.svg?branch=master)](https://travis-ci.org/scott-fleischman/relevant-haskell)

## Build & Run

* Git clone including submodules.
```
git clone --recurse-submodules git@github.com:scott-fleischman/relevant-haskell.git
```

* Get docker image. See official instructions [here](https://www.elastic.co/guide/en/elasticsearch/reference/current/docker.html).
```
sudo docker pull docker.elastic.co/elasticsearch/elasticsearch:5.5.1
```
*  Start the image. This disables [X-Pack security](https://www.elastic.co/guide/en/x-pack/current/installing-xpack.html#xpack-enabling) to avoid `401 Unauthorized` error.
```
docker run -p 9200:9200 -e "http.host=0.0.0.0" -e "transport.host=127.0.0.1" -e "xpack.security.enabled=false"
```

* Install the Haskell tool Stack. See instructions [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

* Build code
```
stack setup
stack build
```

* Run code
```
stack exec relevant
```
