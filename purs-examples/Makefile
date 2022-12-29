
purs-files = $(wildcard *.purs)

output-dir = output

examples : $(purs-files)
	purs compile -g corefn -o $(output-dir) $^

clean :
	rm -rfv $(output-dir)
