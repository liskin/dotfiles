---
document-css: true
css: [style.css]
lang: en

title-prefix: CV
title: Tomáš Janoušek
subtitle: |-
    %contact% \
    <https://lisk.in/> |
    [![github](github.svg){width=1em}&thinsp;liskin](https://github.com/liskin) |
    [![linkedin](linkedin.svg){width=1em}&thinsp;pivnik](https://www.linkedin.com/in/pivnik)
    <!-- icons downloaded from https://icons.getbootstrap.com/ -->
---

<!--
Tomáš is a polyglot programmer with almost two decades of experience in the industry. He's worked with databases (including a columnar database startup), networks, CI infrastructure, package management, operating systems (contributed fixes to the Linux kernel), digital typesetting, and more. Tomáš is a free and open-source software enthusiast, maintainer of the XMonad window manager, and frequent contributor to many other projects.
-->

### Skills

Languages
: <!-- -->
Bash,
C++,
C,
Elixir,
Erlang,
Haskell,
JavaScript,
Perl,
Python,
SQL,
Scala,
TeX

Technologies
: <!-- -->
Linux,
PostgreSQL,
HTML,
CSS,
WebExtensions,
X11,
x86 ASM,
IRC

Tools
: <!-- -->
Vim,
Git,
GNU Make,
GitHub Actions,
Jenkins,
QuickCheck,
Docker,
Ansible

### Experience

::: {.entry}
_**sabbatical**_
[(Guildford, UK)]{}
**2020–2021**

* Joined the [XMonad](https://xmonad.org/) project as a core maintainer. Contributed fixes, features. Reviewed pull requests and helped 2 other people join the project. Directed a release. Launched crowdfunding.

* Implemented CI for [taskwiki](https://github.com/tools-life/taskwiki/) (taskwarrior integration for vimwiki) using Docker and GitHub Actions. Contributed a number of fixes and became a co-maintainer.

* Improved Language Server Protocol support (rename, code actions) in [Async Lint Engine for Vim](https://github.com/dense-analysis/ale).

* Upped my Python skills, released [strava-offline](https://pypi.org/project/strava-offline/), [strava-gear](https://pypi.org/project/strava-gear/), [arbtt-chart](https://pypi.org/project/arbtt-chart/), [foursquare-swarm-ical](https://pypi.org/project/foursquare-swarm-ical/).
:::

::: {.entry}
**Software Engineer**
[[Altworx](https://www.altworx.com/) (Brno, CZ)]{}
**2019–2020**

* Collaborated on a real-time event processing system with physical and cyber security applications.

* Prototyped a 100× faster storage engine for the reality network (time-travelling graph database).

* Discovered, investigated and fixed several fundamental issues in the event processing pipeline.

* Addressed technical debt using static analysis tools (linting, types) and a custom testing framework.

* _Tech:_ Elixir, PostgreSQL, Kafka, Docker, Jenkins, BitBucket Pipelines
:::

::: {.entry}
**Principal Software Engineer**
[[GoodData](https://www.gooddata.com/) (Brno, CZ)]{}
**2017–2019**

* Ran continuous integration and deployment infrastructure and build tooling for 100 engineers.

* Reviewed and merged hundreds of pull requests across multiple repositories.

* Sped up our JIRA deployment so much that I got Thank-you emails from the grumpiest engineers.

* Contributed patches to [jenkins-job-builder](https://docs.openstack.org/infra/jenkins-job-builder/) and [python-jenkins](https://pypi.org/project/python-jenkins/). Became a co-maintainer of both.

* Made builds of dozens of components more reproducible, secure and scalable using Docker.

* Set up a merge queue for the legacy monolith with 4-hour integration tests, saving its developers hours of wasted time every week.

* Built a tool for tracking individual test failures in JIRA, helping developers of shared codebases.

* _Tech:_ Python, Perl, Bash, Jenkins, Zuul, Groovy, Java, Docker, Puppet, RPM
:::

<!--
::: {.entry}
**Software Engineer**
[[iXperta](https://www.ixperta.com/) (Brno, CZ)]{}
**2016–2017**

* Developed a modern solution for business telephony.

* Ported the desktop agent to Windows.

* Implemented OAuth2 for the backend.

* _Tech:_ Haskell, Asterisk, Jenkins, Ansible
:::
-->

::: {.entry}
**Backend Software Engineer**
[[SQLdep](https://sqldep.com/) (Brno, CZ)]{}
**2015–2016**

* Collaborated on a multi-dialect SQL parser and visual data flow (lineage) analyzer.

* Administered the CI infrastructure and introduced continuous deployment for the backend.

* Scaled the analyzer using parallel workers and a job queue. Set up logging and monitoring.

* Prototyped on-premise deployment of the cloud-first product for customers in regulated industries.

* Tackled tech debt: refactored the codebase into independent components, sped up tests and builds.

* _Tech:_ Scala, Perl, SQL, Jenkins, Ansible
:::

::: {.entry}
**Technical Co-founder**
[[Briskat](https://www.briskat.com/) (Brno, CZ)]{}
**2014–2014**

* Developed a prototype of a high-performance interactive database primarily for Online Analytical Processing, capable of processing millions of rows in milliseconds on commodity hardware.

* Used caching and data compression (with some pre-/post-processing tricks to improve compression effectiveness) to reduce CPU work and improve memory bandwidth utilization.

* Designed data structures and algorithms for efficient data storage, sorting, joining and querying.

* _Tech:_ Erlang, C, x86 ASM
:::

::: {.entry}
**Senior Software Engineer**
[[GoodData](https://www.gooddata.com/) (Brno, CZ)]{}
**2011–2014**

* Developed a second-generation Extensible Analytics Engine (XAE), the core of GoodData platform.

* Built an optimizing compiler from Multi-Dimension Analytical Query Language to SQL.

* Tested the SQL backend against a reference implementation of the semantics using QuickCheck.

* Set up regression testing infrastructure using both synthetic and production data. This ensured correctness, prevented performance regressions and allowed us to run performance experiments.

* Pioneered CI (test results within a minute of git push) and CD (using Erlang hot code reloading).

* Had my sleep interrupted due to production incidents less than 5 times: XAE uses small workers (microservices) with almost no moving parts, serving millions of reports daily.

* Promoted to Senior Software Engineer after we fully migrated to the second generation XAE.

* _Tech:_ Perl, Erlang, Haskell, PostgreSQL, Jenkins, Puppet, Splunk
:::

::: {.entry}
**Software Engineer**
[[Red Hat](https://www.redhat.com/en) (Brno, CZ)]{}
**2006–2008**

* Maintained RHEL and Fedora packages for Bash, Dovecot, Cyrus IMAP, BRLTTY.

* Fixed Xorg freezing after 49.7 days (Windows 95, 98 famously had that issue a few years earlier).

* Added the boot-based timer into the Linux kernel to fix uptime and process start times after sleep.

* Fixed a hard to reproduce race in nss\_ldap which led to users seeing another user's data (e-mails).

* Promoted from Associate Software Engineer to Software Engineer within 6 months.

* _Tech:_ C, Bash, RPM, CVS, Git
:::

::: {.entry}
**Programmer**
[QNet CZ (Brno, CZ)]{}
**2002–2006**

* Built an Internet Service Provider portal backend which configured iptables and traffic control (shaping) and showed live per-customer traffic statistics.

* Implemented a flexible role-based access control framework for the frontend.

* Shadowed the network administrator and technicians, later used the knowledge while volunteering for the [CZFree.Net community network](https://translate.google.com/translate?sl=cs&tl=en&u=https://cs.wikipedia.org/wiki/CZFree.Net).

* _Tech:_ C++, PHP, Perl, iptables, Asterisk, CVS, Subversion

<!-- * Developed a call pricing plugin for the Asterisk VoIP gateway. -->
:::

### Education

::: {.entry}
**Computer Science**
[[Masaryk University](https://www.fi.muni.cz/index.html.en) (Brno, CZ)]{}
**2006–2013**

* Specialized in Parallel and Distributed Systems. Member of the [ParaDiSe lab](https://paradise.fi.muni.cz/).

* Redesigned the e-learning software for typesetting exam test sheets: improved layout to avoid page/column breaks in multiple-choice; added support for advanced formatting, images and mathematical formulae. The system is still in use today. _Tech:_ LaTeX, Haskell

* Typeset the [Brisk Guide to Mathematics](https://is.muni.cz/publication/1122631/cs/Matematika-drsne-a-svizne/Slovak-Panak-Bulant?lang=en), a textbook for the undergraduate math curriculum.

* Teaching assistant for Introduction to Functional Programming; Automata and Grammars.
:::

::: {.entry}
**Scientific Computing**
[Universität Wien (Vienna, AT)]{}
**2010**
:::

<!--
::: {.entry}
**Summer School on Bioinformatics**
[Comenius University (Bratislava, SK)]{}
**2011**
:::
-->

### Personal

**[Technical blog](https://lisk.in/).**
Popular articles:
[Linux, media keys and multiple players](https://work.lisk.in/2020/05/06/linux-media-control.html){.ul};
[Even faster bash startup](https://work.lisk.in/2020/11/20/even-faster-bash-startup.html){.ul}

::: {}
**[Free and open-source software](https://work.lisk.in/about/#my-foss-contributions).**
Contributed hundreds of patches, released dozens of small projects, joined
several mid-sized projects as a (co-)maintainer.

* Maintainer of the [XMonad](https://xmonad.org/) X11 window manager. Ex-committer to the [Fluxbox](https://github.com/fluxbox/fluxbox) WM.

* [Prototyped](https://github.com/liskin/hybrid-screenclone) [Linux support for external monitors on dual-GPU laptops](https://gitlab.freedesktop.org/xorg/driver/xf86-video-intel/-/commit/8067255dc9185e85b110254ffbea4d9682d3aa2d) (Nvidia Optimus).
:::

::: {}
**Cycling.**
Won 3 [alleycat races](https://en.wikipedia.org/wiki/Alleycat_race) (and a few more podium finishes).
Built several cycling-related tools:

* [strava-map-switcher](https://github.com/liskin/strava-map-switcher){.ul} is a browser extension for Strava with over 4000 users adding better maps.

* [strava-offline](https://github.com/liskin/strava-offline){.ul} keeps a local mirror (backup) of Strava activities for further analysis.

* [strava-gear](https://github.com/liskin/strava-gear){.ul} is a rule-based tracker of gear and component wear.

* [locus-rflkt-addon](https://github.com/liskin/locus-rflkt-addon){.ul} connects Locus Map (Android app) with Wahoo RLFKT (cycling computer).
:::

<footer><p>%footer%</p></footer>

<!--
::: {.chrome-load-fonts}
sš **sš** _sš_ _**sš**_
:::
-->
