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

export EDITOR="emacsclient -t"                  # $EDITOR opens in terminal
export VISUAL="emacsclient -t -a emacs"         # $VISUAL opens in GUI mode
export ALTERNATE_EDITOR=""
export LANG=en_US.UTF-8

export PATH
export XDG_DATA_DIRS
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export LEDGER_FILE=~/finance/$(date +%Y).journal.gpg
