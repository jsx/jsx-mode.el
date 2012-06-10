TARGET=jsx-mode

all: $(TARGET).elc

clean:
	@rm -f $(TARGET).elc $(TARGET).el~

test:
	@emacs --script test/font-face-test.el

.el.elc:
	@emacs -batch -f batch-byte-compile $<

.PHONY: clean test
