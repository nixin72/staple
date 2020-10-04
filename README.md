# window-bind-buffers

This package allows you to bind file buffers to the window that they're opened 
in so that you don't end up with a bunch of unorganized buffers all over the 
place. Instead, all file buffers are opened in specific windows, and then stay
bound to that windows. If that window gets killed, so do all the buffers. 

Non-file buffers are bound to their own special window. You can open and close 
this window will preserving the state of the buffers or move this window around 
to wherever it's convenient for you, but it means that your normal file buffers 
aren't going to end up getting mixed in with your special compilation buffers
or your shell and other stuff like that.
