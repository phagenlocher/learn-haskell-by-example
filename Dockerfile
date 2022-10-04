# syntax=docker/dockerfile:1
FROM ubuntu:22.04

WORKDIR /root

RUN apt update
RUN apt upgrade -y
RUN apt install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 libnuma1
RUN sh -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
ENV PATH="${PATH}:/root/.ghcup/bin"

RUN apt install -y git libnuma-dev llvm

