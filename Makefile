
.PHONY: elm-src image
elm-src:
	stack test --skip spec
	elm-format --yes elm-src/Generated

app:
	stack --docker --local-bin-path=./bin install

image:
	stack --docker build --test --skip spec --copy-bins --local-bin-path=./bin
	elm-format --yes elm-src/Generated
	docker build -t ${tag} . --build-arg local_bin_path=./bin
