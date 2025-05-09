FROM amazonlinux:2023
RUN yum groupinstall -y "Development Tools"  && \
	yum install -y perl gcc gcc-fortran make which tar gzip gcc-c++ readline-devel zlib-devel bzip2-devel xz-devel pcre2-devel curl-devel libpng-devel libsodium-devel libxml2-devel openssl-devel which && \
	mkdir -p /var/task/bin && \
	cp /usr/bin/which /var/task/bin/ && \
	yum remove -y which
ENV PATH=/var/task/bin:$PATH
RUN curl -O https://cran.r-project.org/src/base/R-4/R-4.3.2.tar.gz && \
	tar -xzf R-4.3.2.tar.gz && \
	cd R-4.3.2 && \
	./configure --prefix=/var/task --enable-R-shlib --without-x --disable-java && \
 	make && make install && \
	/var/task/bin/Rscript -e 'install.packages("remotes", dependencies = TRUE, repos="https://cloud.r-project.org")' && \
	/var/task/bin/Rscript -e 'install.packages("https://cran.r-project.org/src/contrib/Archive/bnlearn/bnlearn_4.8.1.tar.gz", repos = NULL, type = "source")'
COPY src /diagramLambda/
RUN /var/task/bin/Rscript -e 'remotes::install_local("/diagramLambda/", dependencies = TRUE)' && \
	cp /lib64/libgfortran.so.* /var/task/lib64/ && \
	cp /lib64/libquadmath.so.* /var/task/lib64/ && \
	cp /usr/lib64/libgomp.so.* /var/task/lib64/ && \
	cp /usr/lib64/libc.so.* /var/task/lib64/ && \
	cp /usr/lib64/ld-linux-x86-64.so.* /var/task/lib64/ && \
	find /var/task -type f -name "*.so" -exec strip --strip-unneeded {} \; || true && \
	rm -rf /var/task/lib64/R/doc && \
	rm -rf /var/task/lib64/R/library/*/html && \
	rm -rf /var/task/lib64/R/library/*/help && \
	rm -rf /var/task/lib64/R/library/*/doc && \
	rm -rf /var/task/lib64/R/library/*/tests && \
	rm -rf /var/task/lib64/R/library/*/examples && \
	rm -rf /var/task/share/man && \
	rm -rf /var/task/lib64/R/share/locale && \
	cp /diagramLambda/inst/extdata/runtime/runtime.R /var/task
CMD sleep infinite
