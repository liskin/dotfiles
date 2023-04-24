---
title: 'GITHOOKS-LISKIN(5) liskin/dotfiles manpages'
author: Tomáš Janoušek
---

# NAME

githooks-liskin - Extra global git hooks

# SYNOPSIS

`git config --edit`:

```
[liskin]
	trailersJiraBase = https://company.atlassian.net/browse/
	ensureSignedPush = origin
	ensureBranchJira = origin
```

# DESCRIPTION

`liskin.trailersJiraBase` - base URI for Jira issues (any XYZ-123 in branch name).

`liskin.ensureSignedPush` - remote names (may be specified multiple times)
where all commits pushed must be signed.

`liskin.ensureBranchJira` - remote names (may be specified multiple times)
where all branches must have a Jira reference in their name.
