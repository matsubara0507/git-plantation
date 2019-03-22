FROM git-plantation-bin
RUN apt-get update && apt-get install -y \
    ca-certificates \
    git \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /work
COPY script /usr/local/bin/
COPY static /work/static

CMD ["run-app.sh"]
