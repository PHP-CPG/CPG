FROM patchedphp:latest as multilayer-php-cpg

ARG BRANCH
WORKDIR /
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan github.com >> ~/.ssh/known_hosts
RUN mkdir project
RUN mkdir out
ENV CLONE_URL="git@github.com:simkoc/php-cpg.git"
RUN --mount=type=ssh git clone ${CLONE_URL}
WORKDIR php-cpg
RUN git checkout ${BRANCH}
COPY main.conf .
RUN sbt "project bytecode" test
RUN sbt stage
RUN sbt publishLocal
RUN rm -rf /php-cpg/layerByteCode/src && rm -rf /php-cpg/.git