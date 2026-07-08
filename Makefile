.PHONY: test repl lint example

test:
	clojure -M:test

repl:
	clojure -M:nrepl

lint:
	clojure -M:kondo --lint src test

example:
	clojure -M examples/basic_usage.clj
