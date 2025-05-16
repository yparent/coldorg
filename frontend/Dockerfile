FROM rocker/r-base:4.3.3
RUN apt-get update && apt-get install -y  libstdc++6 libcurl4-openssl-dev libssl-dev libx11-dev make pandoc zlib1g-dev  libpq-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
#RUN apt-get update && apt-get install -y libcairo2-dev libfribidi-dev libharfbuzz-dev libpng-dev libtiff-dev libv8-dev libssh2-1-dev cmake git-core libcurl4-openssl-dev libgit2-dev libicu-dev libssl-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*

# Install java and rJava
RUN apt-get -y update && apt-get install -y  default-jdk  r-cran-rjava libicu-dev  && apt-get clean  && rm -rf /var/lib/apt/lists/

ENV JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64
RUN R CMD javareconf

RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

RUN R -e 'install.packages(c("rlang","stringi"))'
#RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.3")'
#RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.25")'
#RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.8.0")'
#RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.31")'
#RUN Rscript -e 'remotes::install_version("waiter",upgrade="never", version = "0.2.5")'
#RUN Rscript -e 'remotes::install_version("shinymanager",upgrade="never", version = "1.0.410")'
#RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "2.1.5")'
#RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "1.0.2")'
#RUN Rscript -e 'remotes::install_version("httr2",upgrade="never", version = "1.0.0")'
#RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.4")'
#RUN Rscript -e 'remotes::install_version("bs4Dash",upgrade="never", version = "2.3.3")'
#RUN mkdir /build_zone
#ADD . /build_zone
#WORKDIR /build_zone
#RUN R -e 'remotes::install_local(upgrade="never")'
#RUN rm -rf /build_zone
# RUN rm -r build/
