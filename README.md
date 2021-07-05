# About this

This is a project dedicated to emacs configuration files. These are tricks on its configuration to improve emacs productivity.


# Cosmetic Changes

In order to change background and foreground colores

(set-background-color "black")
(set-foreground-color "white")
(set-cursor-color "yellow")

Gaining more working space removing unused tools

(tool-bar-mode -1) ;; remove tool bar 
(menu-bar-mode -1) ;; remove menu bar
(scroll-bar-mode -1) ;; remove scrolling bar

(setq inhibit-startup-message t) ;; remove initial logo
