.PHONY: all
all: pipx strava-offline

pipx:
	pipsi install pipx

strava-offline: | pipx
	cd ~/src/strava-offline && pipx install --system-site-packages .
