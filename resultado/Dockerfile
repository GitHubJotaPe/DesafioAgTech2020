FROM rocker/rstudio:4.0.3

RUN apt-get -y update \
	&& apt-get install -y python3-pip \
						gdal-bin \
						libpng-dev \
						libudunits2-0 \
						libudunits2-dev \
						libgdal-dev \
						libfontconfig1-dev \
						libcairo2-dev \
						openjdk-8-jdk \
						libgit2-dev \
						texlive-latex-base \
						texlive-latex-recommended \
						texlive-latex-extra \
	&& apt-get clean all

# Biliotecas en R
RUN R -e "install.packages(c('rmarkdown','Rcpp','rprojroot','reticulate'), INSTALL_opts=c('--no-help', '--no-html'), repos='http://cran.r-project.org')" 
RUN R -e "install.packages(c('readr', 'tidyverse', 'reshape2', 'leaflet', 'caret'), INSTALL_opts=c('--no-help', '--no-html'), repos='http://cran.r-project.org')" 
RUN R -e "install.packages(c('geodist', 'geosphere'), INSTALL_opts=c('--no-help', '--no-html'), repos='http://cran.r-project.org')" 
RUN R -e "install.packages(c('RCurl'), INSTALL_opts=c('--no-help', '--no-html'), repos='http://cran.r-project.org')" 

# para la metrica utilizada en el concurso (bacc)
RUN R -e "install.packages(c('devtools'), INSTALL_opts=c('--no-help', '--no-html'), repos='http://cran.r-project.org')" 
RUN R -e 'library("devtools");install_github("ELToulemonde/dataPreparation", quiet= TRUE)'
RUN R -e "install.packages(c('mlr3measures'), INSTALL_opts=c('--no-help', '--no-html'), repos='http://cran.r-project.org')" 

# Instalaciones para python
RUN pip3 install --no-cache-dir pandas \
								earthengine-api \
								numpy 

# Copiado de la credencial del GEE
COPY EEAuthentication.txt /home/rstudio/.config/earthengine/credentials

#Instalacion de h2o
RUN wget http://h2o-release.s3.amazonaws.com/h2o/rel-zermelo/1/R/src/contrib/h2o_3.32.0.1.tar.gz -O /tmp/h2o_3.32.0.1.tar.gz \
	&& R -e 'install.packages("/tmp/h2o_3.32.0.1.tar.gz", repos = NULL, type="source")' \
	&& rm -rf /tmp/h2o_3.32.0.1.tar.gz

#RUN chmod 777 -R /home/rstudio/.config/