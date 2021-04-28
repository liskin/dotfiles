PYTHON = python3
PIPX_INSTALL = pipx install --system-site-packages --python /usr/bin/python3.9

.PHONY: all
all:

all: pipx
pipx:
	$(PYTHON) -m venv --system-site-packages --without-pip --clear .venvs/$@
	.venvs/$@/bin/python -m pip install 'pipx == 0.16.2.0'
	ln -srf .venvs/$@/bin/$@ $@

all: strava-offline
strava-offline: | pipx
	cd ~/src/strava-offline && $(PIPX_INSTALL) .

all: yq xq tomlq
yq xq tomlq &: | pipx
	$(PIPX_INSTALL) yq

all: markdown2ctags
markdown2ctags: | pipx
	$(PIPX_INSTALL) markdown2ctags

all: protontricks
protontricks: | pipx
	$(PIPX_INSTALL) protontricks

all: pandoc-plantuml
pandoc-plantuml: | pipx
	cd ~/src/pandoc-plantuml-filter && $(PIPX_INSTALL) .

all: pushl
pushl: | pipx
	$(PIPX_INSTALL) pushl

.PHONY: clean
clean:
	-pipx uninstall-all
	$(RM) -r pipx .venvs/pipx

include ~/_gitignore.mk
