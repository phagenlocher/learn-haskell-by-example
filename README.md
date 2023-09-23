# Haskell Bookcamp

This repository contains the code for the projects in the book [Haskell Bookcamp](https://shortener.manning.com/lRp6). The code is organized by chapter, each subdirectory containing a README with further comments.

## Discussion

If anything about the code or book seems to be unclear or you spot an error, you can always join in the discussion in the [liveBook discussion form](https://livebook.manning.com/book/haskell-bookcamp/discussion) or open an issue in this repository!

## Requirements

To compile and run the provided projects you need to have [stack](https://docs.haskellstack.org/) installed. Installing it is easiest with the tool [GHCup](https://www.haskell.org/ghcup/).

### Using docker

If you don't want to install any parts of the Haskell toolchain on your computer locally, then don't panic! This repository contains a docker file that can be used to build a docker image that has the Haskell toolchain with GHCup already installed!

When located in this repository you can build an image from the docker file like so:

```
docker build -t haskell-bookcamp .
```

After building the image you can get a container running that mounts the repository into `/work` where the code can be run. You could also mount your own Haskell projects into the container for ease of use!

```
docker run \
  -ti \
  --mount type=bind,source="$(pwd)",target=/work \
  haskell-bookcamp \
  bash
```
