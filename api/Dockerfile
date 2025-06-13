FROM public.ecr.aws/lambda/python:3.8 AS lambda-image

FROM ubuntu:noble

ARG R_VERSION=4.3.2
ARG LAMBDA_TASK_ROOT=/var/task/
ARG DEBIAN_FRONTEND=noninteractive
ENV LAMBDA_RUNTIME_DIR=/var/task
USER root

RUN mkdir /var/task
WORKDIR /var/task

COPY --from=lambda-image /var/runtime /var/runtime
COPY --from=lambda-image /usr/local/bin/aws-lambda-rie /usr/local/bin/aws-lambda-rie

RUN apt update && apt -y install curl libssl-dev libcurl4-openssl-dev pandoc perl tar gfortran libglpk-dev libxml2-dev libicu-dev libdigest-md5-file-perl

#RUN apt update && apt install -y r-base
RUN curl -O https://cdn.rstudio.com/r/ubuntu-2404/pkgs/r-${R_VERSION}_1_amd64.deb \
 && apt-get update \
 && apt-get install -y ./r-${R_VERSION}_1_amd64.deb \
 && ln -s /opt/R/${R_VERSION}/bin/R /usr/local/bin/R \
 && ln -s /opt/R/${R_VERSION}/bin/Rscript /usr/local/bin/Rscript

# Setting repos with dates to freeze packages
RUN echo "options(repos= c(RSPM = 'https://packagemanager.posit.co/cran/__linux__/noble/2024-05-02', BioC = 'https://packagemanager.posit.co/bioconductor/__linux__/noble/2024-05-02'))" >> /opt/R/${R_VERSION}/lib/R/etc/Rprofile.site

# Install {diagramLambda}
RUN Rscript -e 'install.packages("remotes", dependencies = TRUE)'
COPY src /diagramLambda/
RUN Rscript -e 'remotes::install_local("/diagramLambda/", dependencies = TRUE)'

# Install TinyTeX with {tinytex}
RUN Rscript -e 'tinytex::install_tinytex(force = TRUE, dir = "/opt/TinyTeX")'
# Make sure the TinyTeX install can be found
ENV PATH="/opt/TinyTeX/bin/x86_64-linux:${PATH}"
# Install required LaTeX packages
RUN Rscript -e '\
tinytex::tlmgr_install( \
  readLines( \
    system.file( \
      "dependencies", "dependencies-tex.txt", package = "diagramLambda", \
      mustWork = TRUE \
    ), \
  ) \
)'

# (https://github.com/mdneuzerling/diamonds-via-lambda/blob/main/Dockerfile)
ENV HOME $LAMBDA_TASK_ROOT

# Copy the bootstrap file from package into image
RUN Rscript -e '\
file.copy( \
  system.file( \
    "extdata", "runtime", "bootstrap.sh", package = "diagramLambda", \
    mustWork = TRUE \
  ), \
  "/var/runtime/bootstrap", overwrite = TRUE \
)'

RUN chmod +x /var/runtime/bootstrap

RUN Rscript -e '\
file.copy( \
  system.file( \
    "extdata", "runtime", "runtime.R", package = "diagramLambda", \
    mustWork = TRUE \
  ), \
  Sys.getenv("LAMBDA_TASK_ROOT") \
)'

ENTRYPOINT ["/var/runtime/bootstrap"]

CMD ["handler"]
