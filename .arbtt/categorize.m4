-- inactivity
$screensaver ==> tag inactive,
$idle > 60 && $desktop == "1:irc" ==> tag inactive,
$idle > 60 && current window $program == "google-chrome" && current window $title =~ m|^chrome://newtab| ==> tag inactive,

-- generic, for filtering purposes
tag Program:$current.program,
any window $active ==> tag Desktop:$desktop,

-- unambiguous activities (general)
$desktop == "1:irc" && current window $title =~ /^t\[N\] / ==> tag Activity:Chat,
$desktop == "1:irc" && current window $title =~ /^(t\[m\]|m\[[A-Z]\])[  ]/ ==> tag Activity:Mail,
$desktop == "1:irc" && current window $title =~ m|~/taskwiki.* - VIM$| ==> tag Activity:TaskWiki,
current window $program == "liferea" ==> tag Activity:Web-RSS,
current window $program == "google-chrome" ==> {
	current window $title =~ m|:: https?://meet\.google| ==> tag Activity:Call,
	current window $title =~ m|:: https?://.*muni.*/discussion/| ==> tag Activity:Web-Plkarna,
	current window $title =~ m|:: https?://news\.ycombinator| ==> tag Activity:Web-HN,
	current window $title =~ m!( / Twitter|\bFacebook) (::|-) ! ==> tag Activity:Web-Social,
	current window $title =~ m!\bYouTube (::|-) ! ==> tag Activity:Watch,
	current window $title =~ m!\bTwitch (::|-) ! ==> tag Activity:Watch,
	current window $title =~ m|:: https?://www\.tesco\.com| ==> tag Activity:Web-Shopping,
	current window $title =~ m|:: https?://(www\.)?amazon\.| ==> tag Activity:Web-Shopping,
	current window $title =~ m|^shopping list - Google Sheets| ==> tag Activity:Web-Shopping,
	$desktop == "12:watch" ==> tag Activity:Watch,
},
current window $program == "gl" ==> {
	current window $title =~ m/(?i)\bS\d\dE\d\d\b.* - mpv$/ ==> tag Activity:Watch-Series,
	current window $title =~ /- mpv$/ ==> tag Activity:Watch,
},
current window $program == ["app.element.io", "discord.com__app", "www.messenger.com"] ==> tag Activity:Chat,
current window $program =~ /\.slack\.com$/ ==> tag Activity:Chat,
current window $program == "zoom" ==> tag Activity:Call,
any window $active && $desktop =~ /^W?\d+:steam$/ ==> tag Activity:Games,
any window $active && $desktop == ["1", "11"] && any window $program == "Steam" ==> tag Activity:Games,

include(`categorize-priv.m4')dnl

-- unambiguous activities (projects)
current window $program == "google-chrome" ==> {
	current window $title =~ m|:: https?://github.*/xmonad| ==> tag Activity:Proj-XMonad,
	current window $title =~ m|:: https?://.*reddit.*/xmonad| ==> tag Activity:Proj-XMonad,
},
any window $active && $desktop =~ /^W?\d+:\.?xmonad/ ==> tag Activity:Proj-XMonad,

-- possibly ambiguous fallback activities
any window $active && !( $desktop == ["1:irc", "2:web", "12:watch"] ) && $desktop =~ m|^W?\d+:([^:]*)| ==> tag Activity:Proj-$1ⁱ,
current window $program == "google-chrome" && $desktop == "2:web" ==> {
	-- assume that browsing while a project-related terminal window is visible
	-- on another monitor means that browsing is related to that project
	any window (
		! $hidden
		&& !( $wdesktop == ["1:irc", "2:web", "12:watch"] )
		&& $program == ["urxvt", "x-terminal-emulator"]
		&& $wdesktop =~ m|^W?\d+:([^:]*)|
	) ==> tag Activity:Proj-$1ⁱ,
	tag Activity:Web-otherⁱ,
},

-- vim:set ft=haskell noet:
