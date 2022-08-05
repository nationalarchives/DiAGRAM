FROM public.ecr.aws/lambda/provided:al2

# Enable the Extra Packages for Enterprise Linux (EPEL) repository
RUN yum -y install https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm
# Enable the optional repository from Red Hat Update Infrastructure (RHUI)
RUN yum -y install yum-utils \
  && yum-config-manager --enable "rhel-*-optional-rpms" \
  && yum clean all \
  && rm -rf /var/cache/yum

# Fetch and install desired R version
ENV R_VERSION=4.1.3
RUN curl -O https://cdn.rstudio.com/r/centos-7/pkgs/R-${R_VERSION}-1-1.x86_64.rpm \
  && yum -y install R-${R_VERSION}-1-1.x86_64.rpm \
  && rm R-${R_VERSION}-1-1.x86_64.rpm
ENV PATH="${PATH}:/opt/R/${R_VERSION}/bin/"
# Set repos for package installation
RUN echo "\
options( \
  repos = c( \
    RSPM = 'https://packagemanager.rstudio.com/all/__linux__/centos7/latest', \
    BioC = 'http://www.bioconductor.org/packages/release/bioc' \
  ) \
)" >> /opt/R/${R_VERSION}/lib/R/library/base/R/Rprofile

# System requirements for R packages
COPY dependencies/dependencies-sys.yum /dependencies-sys.yum
RUN yum -y install $(cat /dependencies-sys.yum)

# Install packaged application code
RUN Rscript -e "install.packages('remotes')"
COPY src /diagramLambda/
RUN Rscript -e "remotes::install_local('/diagramLambda')"

# Install TinyTeX with {tinytex}
RUN Rscript -e "tinytex::install_tinytex(force = TRUE, dir = '/opt/TinyTeX')"
# Make sure the TinyTeX install can be found
ENV PATH="/opt/TinyTeX/bin/x86_64-linux:${PATH}"
# Install required LaTeX packages
RUN Rscript -e "tinytex::tlmgr_install( \
  readLines( \
    system.file( \
      'extdata', 'dependencies', 'dependencies-tex.txt', package = 'diagramLambda', mustWork = TRUE \
    ) \
  ) \
)"

# (https://github.com/mdneuzerling/diamonds-via-lambda/blob/main/Dockerfile)
ENV HOME $LAMBDA_TASK_ROOT

# Copy the bootstrap file from package into /var/runtime
RUN Rscript -e "\
file.copy( \
  system.file( \
    'extdata', 'runtime', 'bootstrap.sh', package = 'diagramLambda', mustWork = TRUE \
  ), \
  '/var/runtime/bootstrap' \
)"

RUN Rscript -e "\
file.copy( \
  system.file( \
    'extdata', 'runtime', 'runtime.R', package = 'diagramLambda', mustWork = TRUE \
  ), \
  Sys.getenv('LAMBDA_TASK_ROOT') \
)"
RUN chmod +x /var/runtime/bootstrap

CMD ["handler"]
