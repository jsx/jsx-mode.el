TARGET=jsx-mode

all: $(TARGET).elc

clean:
	@rm -f $(TARGET).elc $(TARGET).el~

test:
	@echo === font-face-test.el ===
	-@emacs --script test/font-face-test.el
	@echo === indent-test.el ===
	-@emacs --script test/indent-test.el

.el.elc:
	@emacs -batch -f batch-byte-compile $<

.PHONY: clean test
