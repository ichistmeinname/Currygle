
indexer  = dist/build/curryIndexer/curryIndexer
server   = dist/build/curryServer/curryServer
pidfile  = server.pid
logfile  = log/production.log
cdoc_dir = /srv/www/htdocs/kics2/lib/CDOC
cdoc_uri = http://www.informatik.uni-kiel.de/kics2/lib/CDOC/

# development only
.PHONY: compile
compile:
	cabal-dev install-deps
	cabal-dev configure
	cabal-dev build

.PHONY: index
index:
	$(indexer) $(cdoc_dir) $(cdoc_uri) --n

.PHONY: update-index
update-index:
	$(indexer) $(cdoc_dir) $(cdoc_uri) --u

.PHONY: start
start:
	nohup $(server) -p 1337 > $(logfile) 2>&1 &

.PHONY: stop
stop:
	@if [ -r $(pidfile) ]; then \
	  echo "Stopping Currygle";
	  ps -p `cat $(pidfile)` && kill -9 `cat $(pidfile)`; \
	else \
	  echo "Currygle is not running." ; \
	fi

.PHONY: restart
restart: stop start

.PHONY: status
status:
	@if [ -r $(pidfile) ] ; then \
	  echo 'Curryle is running with PID '`cat $(pidfile)`'.' ; \
	else \
	  echo 'Curryle is not running.' ; \
	fi
