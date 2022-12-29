
purs-files = $(wildcard *.purs)

corefn-files = $(wildcard $(output-dir)/*/corefn.json)

output-dir = output

pureshell-exe = $(shell cabal list-bin exe:pureshell)

examples : $(purs-files)
	purs compile -g corefn -o $(output-dir) $^

debug : $(corefn-files)
	for i in $^ ; do $(pureshell-exe) debug $${i} ; done

clean :
	rm -rfv $(output-dir)
