define GITLAB_CHECK_LATEST_RELEASE
	{ \
		v="$$(glab api projects/$(shell echo "$($(1))" | jq -R -r @uri)/releases/permalink/latest | jq -r .tag_name)"; \
		if [ "$$v" != "$($(2))" ]; then echo "$(2) := $$v"; fi; \
	};
endef

.PHONY: update-gitlab
## Check for updates of %_VERSION/%_GITLAB
update-gitlab:
	@$(foreach repo_var,$(filter %_GITLAB,$(.VARIABLES)),\
		$(call GITLAB_CHECK_LATEST_RELEASE,$(repo_var),$(repo_var:_GITLAB=_VERSION)))
