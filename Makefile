
indexer = dist/build/curryIndexer/curryIndexer
server  = dist/build/curryServer/curryServer
pidfile = server.pid
logfile = log/production.log

.PHONY: compile
compile:
	cabal-dev install-deps
	cabal-dev configure
	cabal-dev build

.PHONY: index
index: $(indexer)
	$(indexer) example/test.txt --n

.PHONY: restart
restart: stop start

.PHONY: start
start: $(server)
	nohup $(server) -p 1337 > $(logfile) 2>&1 &


.PHONY: stop
stop:
	if [ -r $(pidfile) ]; then ps -p `cat $(pidfile)` && kill -9 `cat $(pidfile)`; fi

$(indexer): compile
$(server): compile