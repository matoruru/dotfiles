# dotfiles
my dotfiles repository, on gentoo linux

## For WSL/Orbstack

1. Execute `mkdir -p ~/GitHub/matoruru/`.
2. Clone this repository at `~/GitHub/matoruru/`.
3. And execute this:

    ```bash
    cd
    ln -srf ~/GitHub/matoruru/dotfiles/.config ~/
    ln -srf ~/GitHub/matoruru/dotfiles/.gitignore ~/
    ln -srf ~/GitHub/matoruru/dotfiles/.npmrc ~/
    ln -srf ~/GitHub/matoruru/dotfiles/.nvimrc ~/
    cp ~/GitHub/matoruru/dotfiles/.gitconfig ~/

    ```
