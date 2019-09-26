

app:
	stack --docker --local-bin-path=./bin install

.PHONY: elm-src
elm-src:
	stack --docker test
	elm-format --yes elm-src/Generated

image: app elm-src
	docker build -t ${tag} . --build-arg local_bin_path=./bin