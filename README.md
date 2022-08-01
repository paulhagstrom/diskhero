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

This can be played in a browser (though possibly without sound) here:
https://archive.org/details/a3-diskhero

Boot to play.
The idea is that you are running around the space to collect the disks.
The disks come in 4 types.  The inventory is listed up at the top.
To the left of each type is an indicator of how many you have.
To the right of each type is an indicator of how many are left on the field.
Your antagonists are the "hoarders" who run around and try to grab the disks.

Hoarders chase after the
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

S to toggle sound on/off

E allegedly exits, which really means "hang more or less on purpose"

## Development notes ##

This is pretty liberally commented.  Some comments may be out of date.
Some cycle counts may be out of date.  Some things of interest:

- Font uploading: buildfont.s implements defining and uploading a custom character set
- Repointing ZP into graphics: play-text40.s and map-hires3.s both use this technique.
- Repointing stack into graphics: play-text40.s uses this when setting up playfield initially.
- Interrupt handling: interrupts.s has this
- Sound generation: buildsound.s defines sounds, interrupts.s plays them
- Color text mode: used in play-text40.s and status-text40.s
- Medium resolution mode: reg-medres.s
- Super hires graphics mode: reg-superhires.s
- Keyboard handling: diskhero.s
- Extended addressing: all over, see buildmap.s or play-text40.s

## Bugs ##

Bugs I am presently aware of.

- Occasionally with sound on, motion stops (but not music) on real hardware until sound is toggled off and on. Still not sure what the mechanism of this failure mode is, what could lead to that. 
- Some flicker still in upper map region on real hardware when scrolling, easy fix attempts have not worked.
- Also seems to flicker more in IA-MAME (but not local MAME?) than it used to?  Difference now is that I am timing updates with HBL regions passing?
- Quit dies with a system failure $01
- Compasses seem not quite accurate, distinguishing "near" from "far" also seems not to be working right
- Medres disk colors do not quite match text disk colors
- Getting kind of slow on real hardware, probably due to stalling for HBL region to passing (stalls for up to a full vertical refresh).

## Enhancements ##

Things I have on the list of things to try to do:

- Change drawing strategy to: blit on VBL tickover, then buffer, to try to speed up game and waste fewer cycles waiting for blanking, even if spending some cycles writing bits first to one place and then to another.
- Possibly shrink map for lower levels
- Make level end when no disks are left
- Allow player to push hoarders (so as not to be trapped, or to defend a disk they're heading for)?
- Add trucks to collect disks (if pushing is possible, trucks can push you? Though eventually will reverse and leave, even if they push you into a wall.)
- Make sounds nicer, longer, quieter?, sequence into a song
- Generate sound buffers during other game computations for longer music sequences
- Add title screen
- Improve colors on real hardware
- Make disks much more evident on the big map
- Maybe use the medres or shgr area to illustrate progress (different view on inventory)?
