#!/bin/bash

docker container prune
docker image rm multilayer-cpg-php:latest
docker image prune