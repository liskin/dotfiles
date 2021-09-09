PYTHON = python3
PIPX_INSTALL = pipx install --system-site-packages --python /usr/bin/python3.9

.PHONY: all
all:

all: yq xq tomlq
yq xq tomlq &:
	$(PIPX_INSTALL) yq

all: markdown2ctags
markdown2ctags:
	$(PIPX_INSTALL) markdown2ctags

all: protontricks
protontricks:
	$(PIPX_INSTALL) protontricks

all: pushl
pushl:
	$(PIPX_INSTALL) pushl

all: timezonefinder
timezonefinder:
	$(PIPX_INSTALL) timezonefinder

all: isort isort-identify-imports
isort isort-identify-imports &:
	$(PIPX_INSTALL) 'isort ~= 5.9'

all: pylsp
pylsp:
	$(PIPX_INSTALL) 'python-lsp-server[all] ~= 1.2.2'

all: strava-offline
strava-offline:
	cd ~/src-python/strava-offline && $(PIPX_INSTALL) --editable .

all: strava-gear strava-gear-sync
strava-gear strava-gear-sync &:
	cd ~/src-python/strava-gear && $(PIPX_INSTALL) --editable .

all: pandoc-plantuml
pandoc-plantuml:
	cd ~/src-python/pandoc-plantuml-filter && $(PIPX_INSTALL) --editable .

.PHONY: clean
clean:
	-pipx uninstall-all

include ~/_gitignore.mk
