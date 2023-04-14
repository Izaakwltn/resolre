LISP ?= sbcl

build:
	$(LISP) --load resolre.asd \
	--eval '(ql:quickload :resolre)' \
		--eval '(asdf:make :resolre)' \
		--eval '(quit)'
