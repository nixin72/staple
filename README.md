# Staple

**Note:** this package is under development and isn't usable yet. This README
jumps the gun on how fleshed out the package actually is and it's in really
early development.

This package allows you to "staple" file buffers to specific windows so that
you don't end up with a bunch of unorganized buffers all over the place. 
Instead, all file buffers are opened in specific windows, and then stay stapled 
to that windows. If that window gets killed, so do all the buffers. 

Non-file buffers are stapled to their own special window. You can open and close 
this window while preserving the state of the buffers or move this window around 
to wherever it's convenient for you, but it means that your normal file buffers 
aren't going to end up getting mixed in with your special compilation buffers
or your shell and other stuff like that.

## Installing
Currently this isn't available on MELPA, but once I get it to a good point, 
then I'll try adding it to MELPA. For now, you'll have to download from source
or use a package manager that allows you to install right from GitHub. 

`straight.el` for example allows you to do this:
```
(straight-use-package 
  '(staple :host github :repo "nixin72/staple.el"))
```

## Usage


## Goal
The goal is to give Emacs a more modern window/buffer management strategy so 
that it behaves more similarly to what you'd see in vscode or IntelliJ.

### If you like how VSCode does it, why not use VSCode? 
Cause I love Emacs, but I also think there's things that 40+ years of hindsight
allow us to improve upon. I'll never go back to vscode, but I think it'd be 
great to have Emacs be more approachable to vscode users.

## Contributing
If you'd like to contribute, just send a PR, I'd be very happy to get help with
this. If you spot a bug, just open up an issue and I'll get to fixing it ASAP.
