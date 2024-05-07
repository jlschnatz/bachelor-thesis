FROM eddelbuettel/r2u:20.04 AS base
RUN apt-get update -qq &&     apt-get install -y --no-install-recommends     cmake  \
    curl  \
    gdebi-core  \
    git  \
    libcurl4-openssl-dev  \
    libfontconfig1-dev  \
    libfreetype6-dev  \
    libfribidi-dev  \
    libharfbuzz-dev  \
    libicu-dev  \
    libjpeg-dev  \
    libpng-dev  \
    libssl-dev  \
    libtiff-dev  \
    libxml2-dev  \
    make  \
    pandoc  \
    snakemake  \
    zlib1g-dev \     &&     apt-get clean &&     rm -rf /var/lib/apt/lists/*

WORKDIR /project 
COPY . /project 
RUN mkdir renv/.cache 
ENV RENV_PATHS_CACHE renv/.cache


RUN R -e 'install.packages("remotes")' 
RUN R -e 'remotes::install_version("renv", version = "1.0.3")' 
RUN R -e 'renv::restore()'
    

RUN curl -o quarto-linux-amd64.deb -L https://github.com/quarto-dev/quarto-cli/releases/download/v0.9.522/quarto-0.9.522-linux-amd64.deb
RUN gdebi --non-interactive quarto-linux-amd64.deb

RUN quarto tools install tinytex

FROM base
WORKDIR /project
COPY --from=base /project .

CMD ["snakemake", "-s", "Snakefile", "--cores", "1"]
