FROM public.ecr.aws/lambda/python:3.8 AS lambda-image

FROM ubuntu:jammy

ARG LAMBDA_TASK_ROOT=/var/task/
USER root

RUN mkdir /var/task
WORKDIR /var/task

COPY --from=lambda-image /var/runtime /var/runtime

# System requirements for R packages
RUN apt update && apt -y install curl

#RUN apt update && apt install -y r-base
ARG R_VERSION=4.3.2
RUN curl -O https://cdn.rstudio.com/r/ubuntu-2204/pkgs/r-${R_VERSION}_1_amd64.deb \
 && apt-get update \
 && apt-get install -y ./r-${R_VERSION}_1_amd64.deb \
 && ln -s /opt/R/${R_VERSION}/bin/R /usr/local/bin/R \
 && ln -s /opt/R/${R_VERSION}/bin/Rscript /usr/local/bin/Rscript

RUN echo "options(repos='https://packagemanager.posit.co/all/__linux__/jammy/latest')" >> /opt/R/${R_VERSION}/lib/R/etc/Rprofile.site

# System requirements for R packages
RUN apt -y install libssl-dev libcurl4-openssl-dev pandoc perl tar  gfortran 
  
# Install {diagramLambda}
RUN Rscript -e 'install.packages("remotes")'
COPY src /diagramLambda/
RUN Rscript -e 'remotes::install_version("bnlearn", version = "4.9.0")'
RUN Rscript -e 'remotes::install_local("/diagramLambda/", dependencies = TRUE)'

RUN Rscript -e 'require("diagramLambda")'

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

RUN Rscript -e '\
file.copy( \
  system.file( \
    "extdata", "runtime", "runtime.R", package = "diagramLambda", \
    mustWork = TRUE \
  ), \
  Sys.getenv("LAMBDA_TASK_ROOT") \
)'

COPY --from=lambda-image /usr/local/bin/aws-lambda-rie /usr/local/bin/aws-lambda-rie
ENV LAMBDA_TASK_ROOT=/var/task
ENV LAMBDA_RUNTIME_DIR=/var/task
ENTRYPOINT ["/var/runtime/bootstrap"]

RUN chmod +x /var/runtime/bootstrap

CMD ["handler"]


