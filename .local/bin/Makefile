# XXX: wipe ~/.local/pipx when updating python
PYTHON = python3.11
PIPX_INSTALL = pipx install --system-site-packages --python /usr/bin/$(PYTHON)

.PHONY: all
all:

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

# docker registry client
all: dxf
dxf:
	$(PIPX_INSTALL) python-dxf

all: gdbgui
gdbgui:
	$(PIPX_INSTALL) gdbgui

all: git-when-merged
git-when-merged:
	$(PIPX_INSTALL) git-when-merged

all: pandoc-plantuml
pandoc-plantuml:
	$(PIPX_INSTALL) 'pandoc-plantuml-filter >= 0.1.5'

all: strava-offline
strava-offline:
	cd ~/src-python/strava-offline && $(PIPX_INSTALL) --editable .

all: strava-gear strava-gear-sync
strava-gear strava-gear-sync &:
	cd ~/src-python/strava-gear && $(PIPX_INSTALL) --editable .

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
