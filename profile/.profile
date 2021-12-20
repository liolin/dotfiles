PATH="$HOME/.cargo/bin:$PATH"
PATH="$HOME/.rbenv/bin:$PATH"
PATH="$HOME/.dotnet/tools:$PATH"
PATH="$HOME/bin:$HOME/.local/bin:$PATH"

if [ -d "/var/lib/flatpak/exports/share/applications" ] ; then
    XDG_DATA_DIRS="/var/lib/flatpak/exports/share/applications:$XDG_DATA_DIRS"
fi

if [ -d "$HOME/.local/share/flatpak/exports/share/applications" ] ; then
    XDG_DATA_DIRS="$HOME/.local/share/flatpak/exports/share/applications:$XDG_DATA_DIRS"
fi

export PATH
export XDG_DATA_DIRS
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"                  # $EDITOR opens in terminal
export VISUAL="emacsclient -t -a emacs"         # $VISUAL opens in GUI mode
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export LEDGER_FILE=~/finance/2021.journal
export LANG=en_US.UTF-8
export GPG_TTY="$(tty)"
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

gpgconf --launch gpg-agent
