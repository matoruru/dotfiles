# explain installation gentoo into LG gram with systemrescueCD

- start GUI mode

- set up network

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
  - +1G
  - 8200
  - n
  - Enter
  - Enter
  - Enter
  - Enter
  - w
- mkfs.vfat -F 32 /dev/sda1
- mkfs.ext4 /dev/sda3

- mount /dev/sda3 /mnt/gentoo
- mkdir /mnt/gentoo/boot
- mount /dev/sda1 /mnt/gentoo/boot

- date

- download stage3 tarball (no systemd) from Firefox
- extract tarball
  - tar -xvjpf stage3-* --xattrs --numeric-owner (in case of bz2)
  - tar -xvJpf stage3-* --xattrs --numeric-owner (in case of zx )

- nano -w /mnt/gentoo/etc/portage/make.conf
  - set CFLAGS, CXXFLAGS, MAKEOPTS
  - USE="X wifi networkmanager"
  - GRUB_PLATFORMS="efi-64"
  - LINGUAS="en ja"
  - L10N="ja"
- mirrorselect -io >> /mnt/etc/portage/make.conf

- cp -L /etc/resolv.conf /mnt/gentoo/etc/

- mount -t proc /proc /mnt/gentoo/proc
- mount --rbind /dev /mnt/gentoo/dev
- mount --rbind /sys /mnt/gentoo/sys

- chroot /mnt/gentoo /bin/bash

- source /etc/profile

- emerge-webrsync
- emerge --sync

- eselect profile list
- eselect profile set XX

- nano -w /etc/fstab
  - /dev/sda1  /boot  vfat  noauto,noatime  1 2
  - /dev/sda3  /      ext4  noatime         0 1
  - /dev/sda2  none   swap  sw              0 0

- nano /etc/locale.gen
- locale-gen
- eselect locale list
- eselect locale set XX

- env-update
- source /etc/profile

- emerge -avuDN @world
- emerge -avD gentoo-sources genkernel wireless-tools wpa_supplicant linux-firmware sudo grub:2 efibootmgr && genkernel all

- nano -w /etc/conf.d/net
  - module_wlp2s0="dhcp"
  - modules="wpa_supplicant"
  - wpa_supplicant_wlp2s0="-Dwext"
- cd /etc/init.d
- ln -s net.lo net.wlp2s0
- rc-update add net.wlp2s0 default
- wpa_passphrase "SSID" "PASSPHRASE" > /etc/wpa_supplicant/wpa_supplicant.conf

- useradd -m -G wheel,portage XXX
- passwd XXX
- passwd
- visudo

- mkdir -p /boot/efi
- mount -o remount,rw /sys/firmware/efi/efivars
- grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=gentoo_grub /dev/sda
- grub-mkconfig -o /boot/grub/grub.cfg

- exit
- reboot
