# dotfiles
my dotfiles repository, on gentoo linux

## For WSL/Orbstack

Execute:

```bash
mkdir -p ~/GitHub/matoruru/
(cd ~/GitHub/matoruru && gh repo clone dotfiles)
cd
ln -srf ~/GitHub/matoruru/dotfiles/.config/* ~/.config/
ln -srf ~/GitHub/matoruru/dotfiles/.gitignore ~/
ln -srf ~/GitHub/matoruru/dotfiles/.npmrc ~/
ln -srf ~/GitHub/matoruru/dotfiles/.bashrc ~/
rm -rf .config/nvim
cp ~/GitHub/matoruru/dotfiles/.gitconfig ~/
```

## Install useful tools

```
sudo apt install fish neovim -y
```

## Install `n`

```
curl -L https://bit.ly/n-install | bash
```

## Install `starship`

```
curl -sS https://starship.rs/install.sh | sh
```

## Install `kubectl`

```
curl -fsSL https://pkgs.k8s.io/core:/stable:/v1.30/deb/Release.key | sudo gpg --dearmor -o /etc/apt/keyrings/kubernetes-apt-keyring.gpg && \
echo 'deb [signed-by=/etc/apt/keyrings/kubernetes-apt-keyring.gpg] https://pkgs.k8s.io/core:/stable:/v1.30/deb/ /' | sudo tee /etc/apt/sources.list.d/kubernetes.list && \
sudo apt-get update && sudo apt-get upgrade -y && sudo apt-get install -y containerd apt-transport-https kubectl
kubectl completion fish > ~/.config/fish/completions/kubectl.fish
```

## Install `git-split-diffs`

```
npm install -g git-split-diffs
```

## Install az CLI

```
curl -sL https://aka.ms/InstallAzureCLIDeb | sudo bash
```
