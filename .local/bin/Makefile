PYTHON = python3
PIPX_INSTALL = pipx install --system-site-packages --python /usr/bin/python3.10

.PHONY: all
all:

all: yq xq tomlq
yq xq tomlq &:
	$(PIPX_INSTALL) yq

all: markdown2ctags
markdown2ctags:
	$(PIPX_INSTALL) markdown2ctags

# webmention/pingback
all: pushl
pushl:
	$(PIPX_INSTALL) pushl

all: timezonefinder
timezonefinder:
	$(PIPX_INSTALL) timezonefinder

all: isort isort-identify-imports
isort isort-identify-imports &:
	$(PIPX_INSTALL) 'isort ~= 5.9'

all: pipenv pipenv-resolver
pipenv pipenv-resolver &:
	$(PIPX_INSTALL) pipenv

# docker registry client
all: dxf
dxf:
	$(PIPX_INSTALL) python-dxf

all: strava-offline
strava-offline:
	cd ~/src-python/strava-offline && $(PIPX_INSTALL) --editable .

all: strava-gear strava-gear-sync
strava-gear strava-gear-sync &:
	cd ~/src-python/strava-gear && $(PIPX_INSTALL) --editable .

all: pandoc-plantuml
pandoc-plantuml:
	cd ~/src-python/pandoc-plantuml-filter && $(PIPX_INSTALL) --editable .

all: foursquare-swarm-ical
foursquare-swarm-ical:
	cd ~/src-python/foursquare-swarm-ical && $(PIPX_INSTALL) --editable .

all: liscopridge liscopridge-cli
liscopridge liscopridge-cli &:
	cd ~/src-python/liscopridge && $(PIPX_INSTALL) --editable .

.PHONY: clean
clean:
	-pipx uninstall-all

include ~/_gitignore.mk
