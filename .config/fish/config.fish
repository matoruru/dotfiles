#!/bin/fish

set fish_greeting

set -x PATH \
    ~/.npm-global/bin \
    ~/.nodebrew/current/bin \
    ~/.yarn-global/bin \
    ~/.bun/bin \
    (go env GOPATH)/bin \
    ~/bin \
    ~/.krew/bin \
    ~/.istioctl/bin \
    $PATH

eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)

# Set alias
alias nvimtutor="nvim -c Tutor"
alias k="kubectl"

if command -sq docker
  set -x GITHUB_USER_NAME matoruru

  # Run the `useful` image as a single-use container.
  alias runuo="docker run --rm -it -v $HOME:$HOME useful"

  # Run the `useful` image as a daemon.
  alias runud="docker run --rm -itd -v $HOME:$HOME useful"

  # Enter the container with its ID.
  function econ
    set -l container_id $argv[1]
    if echo $container_id | string match -r '^ *+$' >/dev/null
      echo Specify the container ID that you want to enter!
    else
      docker exec -it $container_id bash
    end
  end

  # List active docker containers and images.
  function dls
    set -l green (tput setaf 2)
    set -l default (tput sgr0)
    echo $green"[Images]"$default
    docker image ls
    echo
    echo $green"[Running containers]"$default
    docker container ls
  end

  function dla
    set -l green (tput setaf 2)
    set -l default (tput sgr0)
    echo $green"[All images (including intermediates)]"$default
    docker image ls --all
    echo
    echo $green"[All containers]"$default
    docker container ls --all
  end
end

function helm-diff
  set -x file1 (mktemp)
  set -x file2 (mktemp)
  git stash > /dev/null
  helm template $argv > $file1
  git stash pop > /dev/null
  helm template $argv > $file2
  diff -u $file1 $file2 | git-split-diffs --color
  rm $file1 $file2
end

function kustomize-diff
  set -x file1 (mktemp)
  set -x file2 (mktemp)
  git stash > /dev/null
  kubectl kustomize $argv > $file1
  git stash pop > /dev/null
  kubectl kustomize $argv > $file2
  diff -u $file1 $file2 | git-split-diffs --color
  rm $file1 $file2
end

function decode-secrets
  cat - | yq '.data | map_values(@base64d)'
end

# Set tab step
tabs 3

if command -sq starship
  # https://starship.rs/#fish
  starship init fish | source
end
