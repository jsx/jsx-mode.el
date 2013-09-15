TARGET=src/jsx-mode
ifdef DIR
	OPT=-L $(DIR)
endif

all: $(TARGET).elc

clean:
	@rm -f $(TARGET).elc $(TARGET).el~

test:
	@sh ./test/run.sh

.el.elc:
	emacs $(OPT) -batch -f batch-byte-compile $<

.PHONY: clean test
