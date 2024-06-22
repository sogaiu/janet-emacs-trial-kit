# Misc Issues

## Various Buffers at Startup

If the `*Warnings*` buffer shows up, this may be harmless.  Pressing
`q` may dismiss it, but it might pop back open and I don't know a good
way to cope with this other than to close it again.  After a while it
may give up (^^;

The `*elpaca-log*` buffer can also be dismissed by pressing `q` if its
presence is undesired.

Note that subsequent starting of this Emacs setup (`janet jetk`)
should not go through most of the setup steps so should be much faster
and the buffers mentioned above may not come into existence.

## Windows Note

For reasons unknown, sometimes there may be a failure for elpaca
itself.  It appears that quitting emacs and trying again (i.e. invoke
`janet jetk`) is a work-around for this.

