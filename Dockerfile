# syntax=docker/dockerfile:1
FROM ubuntu:22.04

RUN apt-get update \
    && apt-get upgrade -y \
    && apt-get install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 libnuma1 git libnuma-dev llvm zlib1g-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
RUN useradd -m haskell
USER haskell
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_ADJUST_BASHRC=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    sh

ENV PATH=$PATH:/home/haskell/.ghcup/bin
RUN ghcup install stack
WORKDIR /work
