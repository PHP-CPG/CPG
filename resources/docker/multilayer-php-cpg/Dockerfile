FROM patchedphp:latest AS multilayer-php-cpg

ARG BRANCH
WORKDIR /
RUN mkdir project
RUN mkdir out
ENV CLONE_URL="https://github.com/PHP-CPG/cpg.git"
RUN git clone ${CLONE_URL}
WORKDIR /cpg
RUN git checkout ${BRANCH}
COPY main.conf .
RUN sbt "project bytecode" test
RUN sbt stage
RUN sbt publishLocal
RUN rm -rf /cpg/layerByteCode/src && rm -rf /cpg/.git
WORKDIR /out/
ENTRYPOINT ["/cpg/php2cpg","/project/","-c","/cpg/main.conf","bytecode","8"]
