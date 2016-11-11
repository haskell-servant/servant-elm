.PHONY: all
all: test examples

.PHONY: test
test:
	stack test

.PHONY: examples
examples: build-with-examples-flag books-example e2e-tests-example giphy-example readme-example

.PHONY: build-with-examples-flag
build-with-examples-flag:
	stack build --flag servant-elm:examples


.PHONY: books-example
books-example: examples/books/elm/Generated/BooksApi.elm

examples/books/elm/Generated/BooksApi.elm: examples/books/generate.hs
	cd examples/books && stack runghc generate.hs


.PHONY: e2e-tests-example
e2e-tests-example: examples/e2e-tests/elm/Generated/Api.elm

examples/e2e-tests/elm/Generated/Api.elm: examples/e2e-tests/generate.hs
	cd examples/e2e-tests && stack runghc generate.hs


.PHONY: giphy-example
giphy-example: examples/giphy/elm/Generated/GiphyApi.elm

examples/giphy/elm/Generated/GiphyApi.elm: examples/giphy/generate.hs
	cd examples/giphy && stack runghc generate.hs


.PHONY: readme-example
readme-example: examples/readme-example/my-elm-dir/Generated/MyApi.elm

examples/readme-example/my-elm-dir/Generated/MyApi.elm: examples/readme-example/generate.hs
	cd examples/readme-example && stack runghc generate.hs


.PHONY: clean
clean:
	rm -f examples/books/elm/Generated/BooksApi.elm \
	      examples/e2e-tests/elm/Generated/Api.elm \
	      examples/giphy/elm/Generated/GiphyApi.elm \
	      examples/readme-example/my-elm-dir/Generated/MyApi.elm
