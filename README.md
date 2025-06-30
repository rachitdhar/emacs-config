# emacs-config
My configuration for emacs (as a backup)

## Config file structure

Emacs stores its configuration files at a location denoted by "~/". In Windows, this is "C:\Users\<UserName>\AppData\Roaming\".

- **.emacs** : This is the default config file.
- **.emacs.d/** : This is a directory that gets created by itself. We do not need to touch this. Files are automatically included in this as part of installation of packages, in accordance with the config. (Optionally, one may add an **init.el** file in this, and in that case the init.el file is treated by emacs as the main config file. But here I am using .emacs only)
- **.emacs.custom.el** : This file is created to contain any config that emacs adds by itself (for example, for themes). In order to specify this ability, the command "(setq custom-file "~/.emacs.custom.el")" is added in .emacs.

Additionally, I have added two other folders:

- **.emacs.rc/** : For certain run commands. In particular, the **rc.el** file is present in this (contains certain definitions, such as for require, etc.)
- **.emacs.local/** : For additional plugins that have some extensive configuration. For example, **simpc-mode**.

**Note:** The rc.el and simpc-mode.el files are authored by Tsoding / rexim (https://github.com/rexim/dotfiles).

## Acknowledgments / Resources

Much of this configuration is not written by me. For most of this configuration, I have referred to this brilliant config building Youtube video from Tsoding:
Tsoding Daily (Youtube): Configuring Emacs on My New Laptop, https://www.youtube.com/watch?v=81MdyDYqB-A&t=7014s

Also, some of the files (such as for rc.el and simpc-mode) and configuration details (.emacs) have been obtained from his repository:
https://github.com/rexim/dotfiles
