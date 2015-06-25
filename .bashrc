#
# ~/.bashrc
#

alias magicdraw='/opt/magicdraw/bin/magicdraw'

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export EDITOR="emacs -nw"

alias ls='ls --color=auto'
alias grep='grep --color=auto'

PATH=${PATH}:~/scripts:/opt/magicdraw/bin
export PATH

#PS1='[\u@\h \W]\$ '
PS1='\[\e[38;5;208m\][\u@\h \W]\$\[\e[0m\] '
