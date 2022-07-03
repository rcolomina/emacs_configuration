# About this

This is dedicated to configuration of emacs in order to customize its behaviour thus increase your productivity.

The main improvements provided in this repo, are related to the setup of better colours for back/fore grounds, and reconfigure UI addubg more space to your working area. 

Emacs configuration file `.emacs` has to be copied onto your home folder so it can be read by emacs at launch. Notice that this configuration file is written in scheme, which is a lisp dialet.

The first time emacs is started loading the provided configuration file, the `el-get` package will be installed.

# Cosmetic Changes

In order to change background and foreground colores

```
(set-background-color "black")
(set-foreground-color "white")
(set-cursor-color "yellow")
```

Gaining more working space removing unused tools

```
(tool-bar-mode -1) ;; remove tool bar 
(menu-bar-mode -1) ;; remove menu bar
(scroll-bar-mode -1) ;; remove scrolling bar
(setq inhibit-startup-message t) ;; remove initial logo
```
