Dotfiles
========

To overwrite `~/.emacs.d/`, `~/.lein/`, `~/.sbclrc`, `~/.tmux.conf`, `~/.vim/`, and `~/.vimrc`:

    rake activate

To build vim with command-t:

    rbenv shell <ruby-version-you-like>
    brew install vim
    rake compile_command_t

`~/.lein/profiles.clj` assumes you have run:

    brew install android-sdk

`~/.emacs.d/init.el` and `~/.sbclrc` assume you have run:

    brew install sbcl
    curl -O http://beta.quicklisp.org/quicklisp.lisp
    sbcl --load quicklisp.lisp
    # then `(quicklisp-quickstart:install)` in SBCL

Credits
-------

[Bedra](https://github.com/abedra/emacs.d),
[Drew](https://github.com/drewolson/vim_dotfiles),
[Pilat](https://github.com/mikepilat/dotfiles), et al.

