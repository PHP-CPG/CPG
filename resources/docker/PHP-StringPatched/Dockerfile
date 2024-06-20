FROM ubuntu:latest as patchedphp

ENV SCALA_VERSION="2.13.6"
ENV SBT_VERSION="1.7.0"

RUN apt-get update && apt-get -y install gnupg ubuntu-keyring && apt-key update && apt-get update && apt-get install -y  \
    locales  \
    pkg-config build-essential autoconf bison re2c libxml2-dev libsqlite3-dev \
    apt-transport-https curl \
    git \
    openjdk-17-jdk openjdk-17-jre \
    && localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8 \
    && echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list \
    && echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list \
    && curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import \
    && chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg \
    && apt-get update \
    && apt-get install sbt \
    && rm -rf /var/lib/apt/lists/* \
ENV LANG en_US.utf8

WORKDIR /
RUN git clone --depth 1 https://github.com/PHP-CPG/php-src.git -b PHP-7.4.27 php-src-7 \
  && cd php-src-7 \
  && ./buildconf \
  && ./configure --prefix /opt/php-7/ \
  && make -j$(nproc) install \
  && mkdir /opt/php-7/etc/ \
  && cp php.ini-production /opt/php-7/etc/php.ini \
  && sed -i 's/;extension_dir = ".\/"/extension_dir = "\/opt\/php-7\/lib\/php\/extensions\/no-debug-non-zts-20190902\/"/' /opt/php-7/etc/php.ini \
  && sed -i 's/;zend_extension/zend_extension/' /opt/php-7/etc/php.ini \
  && rm -rf /php-src-7

WORKDIR /
RUN git clone --depth 1 https://github.com/PHP-CPG/php-src.git -b PHP-8.2.2 php-src-8 \
  && cd php-src-8 \
  && ./buildconf \
  && ./configure --prefix /opt/php-8/ \
  && make -j$(nproc) install \
  && mkdir /opt/php-8/etc/ \
  && cp php.ini-production /opt/php-8/etc/php.ini \
  && sed -i 's/;extension_dir = ".\/"/extension_dir = "\/opt\/php-8\/lib\/php\/extensions\/no-debug-non-zts-20220829\/"/' /opt/php-8/etc/php.ini \
  && sed -i 's/;zend_extension/zend_extension/' /opt/php-8/etc/php.ini \
  && rm -rf /php-src-8
