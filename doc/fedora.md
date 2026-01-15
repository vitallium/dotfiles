# Fedora installation guide

## Contents

- [Post installation steps](#post-installation-steps)
  - [Install updates](#install-updates)
  - [Change the hostname to serenity](#change-the-hostname-to-serenity)
  - [Reboot the system to install and apply updates](#reboot-the-system-to-install-and-apply-updates)
  - [Remove redundant stuff](#remove-redundant-stuff)
  - [Enable fstrim](#enable-fstrim)
  - [Change default user shell to fish](#change-default-user-shell-to-fish)
  - [Install Gnome extensions applications](#install-gnome-extensions-applications)
  - [Enable RPM Fusion repositories](#enable-rpm-fusion-repositories)
  - [Enable Terra repository](#enable-terra-repository)
  - [Install Ghostty terminal emulator](#install-ghostty-terminal-emulator)
  - [Install drivers for Intel GPU](#install-drivers-for-intel-gpu)
  - [Configure system settings](#configure-system-settings)
  - [Install Development Tools](#install-development-tools)
  - [Install other packages I use](#install-other-packages-i-use)
  - [Install packages for Yubikey](#install-packages-for-yubikey)
- [Configure user environment](#configure-user-environment)
  - [Enable flathub](#enable-flathub)
  - [Install flatpaks](#install-flatpaks)
  - [Clean up unused directories and bookmarks](#clean-up-unused-directories-and-bookmarks)
  - [Install docker](#install-docker)
  - [Install 1password CLI](#install-1password-cli)
- [Configure Gnome](#configure-gnome)
  - [Install packages](#install-packages)
  - [User interface](#user-interface)
  - [Peripherals](#peripherals)
  - [Key bindings](#key-bindings)
  - [Enable minimize,maximize buttons](#enable-minimizemaximize-buttons)
  - [Install phinger-cursors](#install-phinger-cursors)
  - [Set Caps Lock as additional Ctrl](#set-caps-lock-as-additional-ctrl)
- [Framework Laptop specific configuration](#framework-laptop-specific-configuration)
  - [Configure power saving](#configure-power-saving)

## Post installation steps

### Install updates

```bash
sudo dnf upgrade --refresh
```

### Change the hostname to `serenity`

```bash
sudo hostnamectl set-hostname serenity
```

### Reboot the system to install and apply updates

### Remove redundant stuff

```bash
sudo dnf remove -y cheese rhythmbox totem orca \
              gnome-contacts \
              gnome-shell-extension-* \
              libreoffice-* gnome-characters \
              gnome-photos simple-scan virtualbox-guest-additions \
              gnome-tour gnome-connections firefox
sudo dnf autoremove -y
```

### Enable `fstrim`

```bash
sudo systemctl enable fstrim.timer
```

### Change default user shell to `fish`

```bash
sudo dnf install -y fish
sudo usermod -s $(which fish) vslobodin
```

### Install Gnome extensions applications

```bash
sudo dnf install -y gnome-tweaks
```

### Enable RPM Fusion and other repositories

```bash
sudo dnf install -y https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm
sudo dnf install -y https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
sudo dnf upgrade --refresh
sudo dnf group upgrade -y core
sudo dnf install -y rpmfusion-free-release-tainted dnf-plugins-core
sudo dnf copr enable dusansimic/themes
sudo dnf swap ffmpeg-free ffmpeg --allowerasing
```

### Enable Terra repository

```bash
sudo dnf install --nogpgcheck --repofrompath 'terra,https://repos.fyralabs.com/terra$releasever' terra-release
```

### Install Ghostty terminal emulator

```bash
sudo dnf install -y ghostty
```

### Install drivers for Intel GPU

See also https://github.com/ai/environment/blob/main/Install.md

```bash
sudo dnf config-manager setopt fedora-cisco-openh264.enabled=1
sudo dnf group upgrade multimedia --allowerasing --setopt="install_weak_deps=False" --exclude=PackageKit-gstreamer-plugin
sudo dnf install -y intel-media-driver libva \
               libva-utils gstreamer1-vaapi \
               intel-gpu-tools mesa-dri-drivers
```

### Configure system settings

```bash
echo 'fs.inotify.max_user_watches = 524288' | sudo tee -a /etc/sysctl.conf
echo 'vm.swappiness = 10' | sudo tee -a /etc/sysctl.conf
sudo sysctl -p
```

### Install `Development Tools`

```bash
sudo dnf install -y make gcc-c++ gcc make bzip2 openssl \
               openssl-devel libyaml-devel libffi-devel \
               readline-devel zlib-ng-devel gdbm-devel ncurses-devel \
               m4 ncurses-devel autoconf re2 re2-devel \
               libcurl-devel libuuid-devel \
               libvterm-devel gpgme-devel icu krb5-devel gtk4-devel \
               libusb1-devel rpm-devel

sudo dnf group install -y "development-tools"
```

### Install other packages I use

```bash
sudo dnf install -y git git-lfs git-delta \
               bat fzf ripgrep \
               editorconfig \
               kernel-tools wl-clipboard \
               cmake ninja-build jq \
               ImageMagick \
               git-extras htop \
               GraphicsMagick pipewire-codec-aptx \
               helix morewaita-icon-theme
```

### Install packages for `Yubikey`

```bash
sudo dnf install -y gnupg2 dirmngr cryptsetup gnupg2-smime gnupg2-scdaemon pcsc-tools pcsc-lite pgp-tools
sudo systemctl enable --now pcscd
```

### Install GitHub CLI

```bash
sudo dnf config-manager addrepo --from-repofile=https://cli.github.com/packages/rpm/gh-cli.repo
sudo dnf install gh --repo gh-cli
```

## Configure user environment

### Enable [flathub](https://flatpak.org/setup/Fedora)

```bash
sudo flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
sudo flatpak remote-modify flathub --enable
```

### Install `flatpaks`

```bash
flatpak install -y flathub com.discordapp.Discord \
                           org.telegram.desktop \
                           com.slack.Slack \
                           com.github.tchx84.Flatseal \
                           com.belmoussaoui.Obfuscate \
                           com.obsproject.Studio \
                           com.mattjakeman.ExtensionManager \
                           org.mozilla.firefox \
                           com.google.Chrome \
                           com.github.maoschanz.drawing \
                           org.localsend.localsend_app \
                           com.github.PintaProject.Pinta \
                           md.obsidian.Obsidian \
                           com.fastmail.Fastmail \
                           io.github.celluloid_player.Celluloid \
                           io.github.realmazharhussain.GdmSettings \
                           app.zen_browser.zen
```

### Install docker

```bash
sudo dnf -y install dnf-plugins-core
sudo dnf config-manager --add-repo https://download.docker.com/linux/fedora/docker-ce.repo
sudo dnf install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
```

Configure rootless.

```bash
dockerd-rootless-setuptool.sh install
```

Enable and start `systemd` services.

```bash
systemctl enable --now docker.service
```

### Install 1password CLI

```bash
sudo rpm --import https://downloads.1password.com/linux/keys/1password.asc
sudo sh -c 'echo -e "[1password]\nname=1Password Stable Channel\nbaseurl=https://downloads.1password.com/linux/rpm/stable/\$basearch\nenabled=1\ngpgcheck=1\nrepo_gpgcheck=1\ngpgkey=\"https://downloads.1password.com/linux/keys/1password.asc\"" > /etc/yum.repos.d/1password.repo'
sudo dnf install -y 1password 1password-cli
```

## Configure Gnome

### Install packages

```bash
sudo dnf install -y dconf-editor \
               webp-pixbuf-loader \
               avif-pixbuf-loader \
               heif-pixbuf-loader \
               gthumb
```

### User interface

Set the application on the dash.

```bash
gsettings set org.gnome.shell favorite-apps "['org.mozilla.firefox.desktop', 'com.mitchellh.ghostty.desktop', 'org.gnome.Nautilus.desktop', 'org.telegram.desktop.desktop']"
```

#### Adjust search locations

```bash
gsettings set org.gnome.desktop.search-providers disabled "['org.gnome.clocks.desktop']"
```

#### Nautilus

Sort directories first.

```bash
gsettings set org.gtk.Settings.FileChooser sort-directories-first true
```

### Peripherals

Enable blazingly fast keyboard repeat.

```bash
gsettings set org.gnome.desktop.peripherals.keyboard delay 150
```

### Key bindings

```bash
#!/bin/bash
gsettings set org.gnome.mutter.keybindings toggle-tiled-left "['<Super>h']"
gsettings set org.gnome.mutter.keybindings toggle-tiled-right "['<Super>l']"

gsettings set org.gnome.settings-daemon.plugins.media-keys screensaver "['<Super>Return']"

gsettings set org.gnome.desktop.wm.keybindings minimize "['<Super>apostrophe']"
gsettings set org.gnome.desktop.wm.keybindings maximize "['<Super>k']"
gsettings set org.gnome.desktop.wm.keybindings unmaximize "['<Super>j']"
gsettings set org.gnome.desktop.wm.keybindings close "['<Super><Shift>q']"

END=9; for num in $(seq 1 $END); do
  gsettings set org.gnome.shell.keybindings switch-to-application-$num "[]";
  gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-$num "['<Super>$num']"
  gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-$num "['<Super><Shift>$num']"
done
```

### Install [phinger-cursors](https://github.com/phisch/phinger-cursors)

```bash
sudo wget -cO- https://github.com/phisch/phinger-cursors/releases/latest/download/phinger-cursors-variants.tar.bz2 | sudo tar xfj - -C /usr/share/icons
```

And enable them.

```bash
gsettings set org.gnome.desktop.interface cursor-theme "phinger-cursors-dark"
```

### Set Caps Lock as additional Ctrl

```bash
gsettings set org.gnome.desktop.input-sources xkb-options "['caps:ctrl_modifier']"
```

## Framework Laptop specific configuration

### Configure power saving

```bash
sudo grubby --update-kernel=ALL --args="nvme.noacpi=1"
```

### Enable GuC

```bash
sudo grubby --update-kernel=ALL --args="i915.enable_guc=3"
```
