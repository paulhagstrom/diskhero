DISKHERO

A game for the Apple ///

Paul Hagstrom, July 2022

This game was originally written as the basis for a presentation at KansasFest 2022.

It is mostly a tech demo and a way to learn how the special modes of the Apple /// work.

## Compilation ##

I use ca65 to compile this, and AppleCommander to get it onto a disk image.
My steps are:

```
ca65 diskhero.s
ld65 -o diskhero.bin -C apple3big.cfg diskhero.o
ac -d diskhero.po SOS.INTERP
ac -p diskhero.po SOS.INTERP bin < diskhero.bin
```

## Play ##

Boot to play.
The idea is that you are running around the space to collect the disks.
The disks come in 4 types.  The inventory is listed up at the top.
To the left of each type is an indicator of how many you have.
To the right of each type is an indicator of how many are left on the field.
Your antagonists are the "hoarders" who run around and try to grab the disks.

The way it is supposed to work eventually is that hoarders chase after the
most valuable disks, so if a hoarder is heading for a disk you want to
collect, you can distract it if you have a higher value disk by dropping it.

Hoarders and you have different ideas of value, so disks that get you the
most points are the ones the hoarders find boring, but will still collect
and take off the board if they come across them.

There is currently no win/lose condition, though if the number of disks
remaining on the map reaches zero, there is nothing further to be done
(apart from dropping disks to attract hoarders).  So that would be the
condition where the next level begins.  Levels might be differentiated
by speed of hoarders and size of map, at some point.

## Keys ##

Movement keys:

```
U I O
J K L <- K stops movement
M , .
```

1234 drop a disk of type 1, 2, 3, 4 if you have one

S to toggle the terrible background soundtrack

Q to toggle the sound effects

E allegedly exits, which really means "hang more or less on purpose"

