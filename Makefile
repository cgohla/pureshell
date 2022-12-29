help:
	echo read the Makefile

## compile the example purs files
.PHONY: examples
examples : 
	$(MAKE) -C purs-examples
