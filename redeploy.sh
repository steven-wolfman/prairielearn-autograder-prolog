#!/bin/sh

DEFAULT_DOCKER_ID=cswolf
DOCKER_ID=${1-$DEFAULT_DOCKER_ID}

docker build . -t "${DOCKER_ID}/haskell-prairielearn:latest" && \
    docker push "${DOCKER_ID}/haskell-prairielearn:latest"
