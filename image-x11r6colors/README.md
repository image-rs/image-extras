## X11R6 Color names

This mini-library contains the color names database (often referred to as 
`rgb.txt`) from X11R6, as a sorted array with names case folded. It has been 
separated out into a distinct crate because it is possible that databases of
colors with names are copyrightable and therefore subject to the X11 license.
Splitting out the list makes it easier for users of the `image-extras`, which
uses this library if its `xpm` feature is enabled, to keep track of licensing
information.

Only the color database itself (from 1994) is X11 licensed; everything since
then is offered under `MIT OR Apache-2.0`.
