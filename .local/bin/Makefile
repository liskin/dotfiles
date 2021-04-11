.PHONY: all
all:

all: pipx
pipx:
	pipsi install $@

all: strava-offline
strava-offline: | pipx
	cd ~/src/strava-offline && pipx install --system-site-packages .

all: yq xq
yq xq &: | pipx
	pipx install --system-site-packages yq

all: markdown2ctags
markdown2ctags: | pipx
	pipx install --system-site-packages $@

all: protontricks
protontricks: | pipx
	pipx install --system-site-packages $@

all: pandoc-plantuml
pandoc-plantuml: | pipx
	cd ~/src/pandoc-plantuml-filter && pipx install --system-site-packages .

all: pushl
pushl: | pipx
	pipx install --system-site-packages $@

all: supermercado
supermercado: | pipx
	pipx install --system-site-packages $@

all: geojsonio
geojsonio: | pipx
	pipx install --system-site-packages $@

include ~/_gitignore.mk
