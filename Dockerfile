FROM git-plantation-bin
WORKDIR /work
COPY script /usr/local/bin/
COPY static /work/static

CMD ["run-app.sh"]
