define GITHUB_CHECK_LATEST_RELEASE
	{ \
		v="$$(gh api --jq '.tag_name' repos/$($(1))/releases/latest)"; \
		if [ "$$v" != "$($(2))" ]; then echo "$(2) := $$v"; fi; \
	};
endef

define GITHUB_CHECK_LATEST_TAG
	{ \
		v="$$(gh api --jq '.[].ref' repos/$($(1))/git/refs/tag | sed -ne 's|refs/tags/||p' | sort --version-sort | tail -1)"; \
		if [ "$$v" != "$($(2))" ]; then echo "$(2) := $$v"; fi; \
	};
endef

.PHONY: update-github
## Check for updates of %_VERSION/%_GITHUB
update-github:
	@$(foreach repo_var,$(filter %_GITHUB,$(.VARIABLES)),\
		$(call GITHUB_CHECK_LATEST_RELEASE,$(repo_var),$(repo_var:_GITHUB=_VERSION)))
	@$(foreach repo_var,$(filter %_GITHUB_LASTTAG,$(.VARIABLES)),\
		$(call GITHUB_CHECK_LATEST_TAG,$(repo_var),$(repo_var:_GITHUB_LASTTAG=_VERSION)))
