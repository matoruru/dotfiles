# Gentoo Linux UEFI boot on LG gram without using GRUB
- date (fit to UTC time)
- gdisk /dev/sda
    - o
    - n
    - Enter
    - Enter
    - +256M
    - EF00
    - n
    - Enter
    - Enter
    - +4G
    - 8200
    - n
    - Enter
    - Enter
    - +50G
    - Enter
    - w
- mkfs.vfat -F 32 /dev/sda1
- mkfs.ext4 /dev/sda3
- mkswap /dev/sda2
- swapon /dev/sda2
- mount /dev/sda3 /mnt/gentoo
- mkdir /mnt/gentoo/boot
- mount /dev/sda1 /mnt/gentoo/boot
- cd /mnt/gentoo
- (download stage3 tarball (i did it using firefox), Do not select no-multilib!!) [here](http://ftp.iij.ad.jp/pub/linux/gentoo/releases/amd64/autobuilds/)
- (extract tarball)
    - tar -xvjpf stage3* --xattrs --numeric-owner (in case of bz2)
    - tar -xvJpf stage3* --xattrs --numeric-owner (in case of xz)
- nano -w /mnt/gentoo/etc/portage/make.conf
    - CFLAGS
    - CXXFLAGS
    - MAKEOPTS
- mirrorselect -io >> /mnt/gentoo/etc/portage/make.conf
- cp -L /etc/resolv.conf /mnt/gentoo/etc/
- mount -t proc /proc /mnt/gentoo/proc
- mount --rbind /dev /mnt/gentoo/dev
- mount --rbind /sys /mnt/gentoo/sys
- chroot /mnt/gentoo /bin/bash
- source /etc/profile
- emerge-webrsync
- echo "Asia/Tokyo" > /etc/timezone
- emerge --config sys-libs/timezone-data
- eselect profile list
- eselect profile set XX
- nano -w /etc/fstab
    - /dev/sda1 /boot vfat noauto,noatime 0 2
    - /dev/sda3 / ext4 noatime 0 1
- nano -w /etc/locale.gen
- locale-gen
- eselect locale list
- eselect locale set YY
- env-update && source /etc/profile
- emerge -avuDN @world gentoo-sources genkernel efibootmgr wireless-tools wpa_supplicant linux-firmware sudo dev-vcs/git app-arch/lz4 layman ntp
- cd
- git clone https://github.com/matoruru/dotfiles.git
- cd /usr/src/linux
- make mrproper
- cp ~/dotfiles/.config ./
- make -j8 && make -j8 modules_install && make install && genkernel --kernel-config=.config initramfs
- cd /boot
- mkdir -p efi/boot
- cp vmlinuz*-gentoo efi/boot/bootx64.efi
- mount -o remount,rw /sys/firmware/efi/efivars
- efibootmgr -v
- efibootmgr -b X -B (if you want to delete any entry point)
- efibootmgr -c --part 1 -d /dev/sda -L "Gentoo 1 (/dev/sda1)" -l "\efi\boot\bootx64.efi" -u "root=/dev/sda3 rw initrd=/initramfs-genkernel-x86_64.X.XX.XX-gentoo"
- nano -w /etc/conf.d/net
    - module_wlp2s0="dhcp"
    - modules="wpa_supplicant"
    - wpa_supplicant_wlp2s0="-Dwext"
- cd /etc/init.d
- ln -s net.lo net.wlp2s0
- rc-update add net.wlp2s0 default
- wpa_passphrase "SSID" "PASSPHRASE" > /etc/wpa_supplicant/wpa_supplicant.conf
- vim /etc/rc.conf
  - rc_parallel="YES"
- useradd -m -G wheel,portage,audio,video XXX
- passwd XXX
- visudo ( nopasswd )
- Ctrl D
- reboot
- mkdir -p ~/repositories/matoruru
- cd ~/repositories/matoruru
- git clone https://github.com/matoruru/dotfiles.git
- cd dotfiles
- bash install.sh
- git remote set-url origin git@github.com:matoruru/[repositoriy's name].git
- git config --global user.email "----------@----.---"
- git config --global user.name "matoruru"
- git config --global core.editor vim

## How to setup mozc for japanese input
- ibus-setup
  - General -> Next input method -> (none)
  - Input Method -> Add -> Japanese -> Mozc
  - Input Method -> Japanese - Mozc -> Preferences -> General -> Keymap style -> Custmize -> Edit -> Import from File -> gentoo-tools/keymapfile
  
##  If you saw a message like that "xmobar hGetContents: invalid argument (invalid byte sequence...." from xmonad
Didn't you forget set LANG?, eselect locale set YY

## Useful tools
- app-office/libreoffice
- dev-python/spyder
- chromium
  - get theme [here](https://chrome.google.com/webstore/detail/material-incognito-dark-t/ahifcnpnjgbadkjdhagpfjfkmlapfoel?hl=en)
  - try middle click on top border and check "Use system title bar and borders"! 

## How to create purescript projects
### create project
- mkdir project1
- cd project1
- pulp init

### If you need some package
execute commands, fg. bower install --save purescript-console

## How to install Android Studio
- sudo emerge -av android-studio
- android-studio
- if you see white window, close it and then push "restart ..." button

## How to install miniconda and make environments using fish shell
### How to install miniconda
- see https://conda.io/miniconda.html and download and execute miniconda installer for Linux
- in installer, not to edit .bashrc
- add config.fish ~/miniconda3/etc/fish/conf.d/conda.fish

### How to make environments
- conda create -n ENVIRONMENT_NAME
- conda activate ENVIRONMENT_NAME
- conda install PACKAGE_NAME1 PACKAGE_NAME2 ...

## How to generate ssh-keys
- ssh-keygen -t rsa -b 4096 -C "my email address"
