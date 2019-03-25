FROM git-plantation-bin
RUN apt-get update && apt-get install -y \
    ca-certificates \
    git \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN git config --global user.email "bot@example.com" \
 && git config --global user.name "Bot"
WORKDIR /work
COPY script /usr/local/bin/
COPY static /work/static

CMD ["run-app.sh"]
