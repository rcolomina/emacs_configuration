# About this

This is a project dedicated to emacs configuration files to help on its configuration in order to improve your productivity.

The main improvements are related to setup better colours for the back/fore grounds, and also adding more space to your working area. 

Emacs configuration file `.emacs` has to be copied over your home folder to be read by emacs. The configuration file is written in scheme, a lisp dialet.

The first time emacs is started using this configuration file, `el-get` package is installed.

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
