FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8


RUN \
  --mount=type=cache,target=/var/lib/apt/lists \
  --mount=type=cache,target=/var/cache/apt/archives \
  apt-get update \
  && apt-get -y install \
       git \
       build-essential \
       automake \
       libcurl4-openssl-dev  \
       ca-certificates \
       curl \
       wget \
       zlib1g-dev \
       libzstd-dev

RUN git clone -b release https://github.com/roswell/roswell.git \
    && cd roswell \
    && sh bootstrap \
    && ./configure \
    && make \
    && make install \
    && ros setup

RUN ros install sbcl

RUN mkdir /app
WORKDIR /app
COPY . /app
RUN mkdir -p /root/.config/common-lisp/source-registry.conf.d \
     && echo '(:tree "/app/")' > /root/.config/common-lisp/source-registry.conf.d/10-app.conf

RUN ros -L sbcl -e '(ros:quicklisp)' \
    && ros -L sbcl -e '(ql:quickload :kabotan)'

CMD ["make"]
