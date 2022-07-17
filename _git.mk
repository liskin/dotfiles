define GIT_DESCRIBE
	unset GIT_DIR && git -C $(1) describe --tags --long --dirty=-dirty-"$$(git -C $(1) diff --patch-with-raw | sha1sum | head -c7)"
endef
