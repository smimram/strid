RCP = rsync -rLtCPz --delete --stats
VERSION = $(shell cat ../trunk/VERSION)

upload:
	make -C ../trunk dist
	ln -s ../../trunk/strid-$(VERSION).tar.gz htdocs/strid-snapshot.tar.gz
	-$(RCP) . shell.sf.net:/home/groups/s/st/strid
	rm -f htdocs/strid-snapshot.tar.gz
