#!/bin/bash
#/bin/bash
branch=${1:-"master"}
 DOCKER_BUILDKIT=1 docker build --squash --ssh default --no-cache --build-arg BRANCH=${branch} -t multilayer-cpg-php-squashed .