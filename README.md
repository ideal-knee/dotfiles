Dotfiles
========

To overwrite `~/.emacs.d/`, `~/.lein/`, `~/.tmux.conf`, `~/.vim/`, and `~/.vimrc`:

    rake activate

To build vim with command-t:

    rbenv shell <ruby-version-you-like>
    brew install vim
    rake compile_command_t

`~/.lein/profiles.clj` assumes you have run:

    brew install android-sdk

Credits
-------

[Bedra](https://github.com/abedra/emacs.d),
[Drew](https://github.com/drewolson/vim_dotfiles),
[Pilat](https://github.com/mikepilat/dotfiles), et al.

