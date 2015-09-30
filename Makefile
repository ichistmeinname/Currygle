bindir   = .cabal-sandbox/bin/
indexer  = $(bindir)/curryIndexer
server   = $(bindir)/curryServer
pidfile  = server.pid
logfile  = log/production.log
cdoc_dir = cdoc
cdoc_uri = http://www-ps.informatik.uni-kiel.de/kics2/lib/CDOC/

# development only
.PHONY: compile
compile:
	git submodule init
	git submodule update
	cabal sandbox init
	cabal install happy
	cabal sandbox add-source holumbus
	cabal install --dependencies-only
	cabal configure
	cabal build

.PHONY: index
index:
	$(indexer) $(cdoc_dir) $(cdoc_uri) --n

.PHONY: update-index
update-index:
	$(indexer) $(cdoc_dir) $(cdoc_uri) --u

.PHONY: start
start:
	nohup $(server) -b 127.0.0.1 -p 1337 > $(logfile) 2>&1 &

.PHONY: stop
stop:
	@if [ -s `readlink -f $(pidfile)` ]; then \
	  echo "Stopping Currygle"; \
	  (ps -p `cat $(pidfile)` && kill -9 `cat $(pidfile)`) || exit 0 ; \
	  > $(pidfile) ; \
	else \
	  echo "Currygle is not running." ; \
	fi

.PHONY: restart
restart: stop start

.PHONY: status
status:
	@if [ -s `readlink -f $(pidfile)` ] ; then \
	  echo 'Curryle is running with PID '`cat $(pidfile)`'.' ; \
	else \
	  echo 'Curryle is not running.' ; \
	fi
