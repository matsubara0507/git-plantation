FROM matsubara0507/ubuntu-for-haskell:git
ARG local_bin_path
WORKDIR /work
COPY ${local_bin_path} /usr/local/bin
COPY script /usr/local/bin/
COPY static /work/static

CMD ["run-app.sh"]
