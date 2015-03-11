# init-loader.el [![travis badge][travis-badge]][travis-link] [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]


## What is it ?

`init-loader.el` is a loader of configuration files.  `init-loader.el`
loads configuration files from specified directory.  It enables you to
categorize your configurations and separate them into multiple files.

The original code is
[here](http://coderepos.org/share/browser/lang/elisp/init-loader/init-loader.el).
The current version of `init-loader.el` is based on
[tarao's fork version](https://gist.github.com/tarao/4362564), which
have some issues fixed and some essential features added compared with
the original one.


## Sample code

You only have to call `init-loader-load` with a directory where your
configuration files are located.

```lisp
(require 'init-loader)

;; Load configuration files in '/path/to/init-directory'.
(init-loader-load "/path/to/init-directory")

;; If you omit arguments, then `init-loader-directory' is used
(init-loader-load)
```

## Features

### Files to be loaded

Note that not all files in the directory are loaded.  Each file is
examined that if it is a .el or .elc file and, it has a valid name
specified by `init-loader-default-regexp` or it is a platform specific
configuration file.

By default, valid names of configuration files start with two
digits.  For example, the following file names are all valid:
- 00_util.el
- 01_ik-cmd.el
- 21_javascript.el
- 99_global-keys.el

Files are loaded in the lexicographical order.  This helps you to
resolve dependency of the configurations.

A platform specific configuration file has a prefix corresponds to
the platform.  The following is the list of prefixes and platform
specific configuration files are loaded in the listed order after
non-platform specific configuration files.

Platform |  Subplatform       | Prefix         |  Example
---------|--------------------|----------------|-----------------------------
Windows  |                    | `windows-`     |  windows-fonts.el
         |  Meadow            | `meadow-`      |  meadow-commands.el
Mac OS X |  Carbon Emacs      | `carbon-emacs-`|  carbon-emacs-applescript.el
         |  Cocoa Emacs       | `cocoa-emacs-` |  cocoa-emacs-plist.el
GNU/Linux|                    | `linux-`       |  linux-commands.el
All      |  Non-window system | `nw-`          |  nw-key.el

### Byte-compilation

If `init-loader-byte-compile` is non-nil, each configuration file is
byte-compiled when it is loaded.  If you modify the .el file, then it
is recompiled next time it is loaded.

### Log

Loaded files and errors during the loading process are recorded.  If
`init-loader-show-log-after-init` is non-nil, the record is shown
after the overall loading process.  You can do this manually by `M-x
init-loader-show-log`.

## Reference

### Interfaces

#### `init-loader-load (&optional INIT-DIR)`

Function to load configuration files in `INIT-DIR`.

#### `init-loader-show-log ()`

Interactive command to show log messages.

### Customization

#### `init-loader-directory` : `directory` (default: `"~/.emacs.d/inits"`)

Default directory of configuration files.

#### `init-loader-show-log-after-init` : `boolean` (default: `t`)

Show log message after initializing if this value is `t`.
If this value is `error-only`, log buffer is shown only
errors occured.

#### `init-loader-byte-compile` : `boolean` (default: `nil`)

Byte-compile configuration files if this value is non-nil.

### Hooks

#### `init-loader-before-compile-hook`

Abnormal hook run before byte-compiling a configuration file when
`init-loader-byte-compile` is non-nil.  Each function in the hook
takes one argument, which is the name of the configuration file to be
loaded.

[travis-badge]: https://travis-ci.org/emacs-jp/init-loader.svg
[travis-link]: https://travis-ci.org/emacs-jp/init-loader
[melpa-link]: http://melpa.org/#/init-loader
[melpa-stable-link]: http://stable.melpa.org/#/init-loader
[melpa-badge]: http://melpa.org/packages/init-loader-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/init-loader-badge.svg
