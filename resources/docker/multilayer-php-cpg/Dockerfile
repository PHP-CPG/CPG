FROM patchedphp:latest as multilayer-php-cpg

ARG BRANCH
WORKDIR /
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan github.com >> ~/.ssh/known_hosts
RUN mkdir project
RUN mkdir out
ENV CLONE_URL="git@github.com:PHP-CPG/cpg.git"
RUN --mount=type=ssh git clone ${CLONE_URL}
WORKDIR cpg
RUN git checkout ${BRANCH}
COPY main.conf .
RUN sbt "project bytecode" test
RUN sbt stage
RUN sbt publishLocal
RUN rm -rf /cpg/layerByteCode/src && rm -rf /cpg/.git
WORKDIR /out/
ENTRYPOINT ["/cpg/php2cpg","/project/","-c","/cpg/main.conf","bytecode","8"]
