# Landmarks

This package provides so called "landmarks" to emacs.

Landmarks are a fast navigation facility for emacs, very similar to
bookmarks, but faster to use. Similarly to an emacs register, a
landmark is a location where you might want to come back later. Unlike
bookmarks it is either:

- a buffer (jumping back to a \"buffer landmark\" jumps to the buffer
  at its current point position)
- or a precise location in a buffer (\"position landmark\").

Like registers, each landmark is identified uniquely by a character
but this is anecdotical. More importantly it is also attached to a
**key of your keyboard** (any keystroke actually but it is
particularly fast if this keystroke is simple). Jumping to the
landmark is done by hitting that key (typically a numpad key or an "f"
key).

Unlike bookmarks, (position) landmarks are visible. This is
configurable using `landmark-face`, `landmark-show-landmark-position`
and `landmark-show-landmark-fringe`.

See the end of this file for a comparison with the excellent `bm.el`.

## Installation

Load the file `landmarks.el` and put it in a directory known to emacs.
Then add the following line to your init file:

```elisp
(require 'landmark)
```

Then you can add one or several of the following lines, depending on which kind of keyboard keys you want to dedicate to landmarks:

```elisp
(landmark-assign-kp-n-config) ;; for kp-0 to kp-9 numpad keys
(landmark-assign-fn-config)  ;; for f5 to f9 keys
(landmark-assign-kp-cycle-config) ;; for cycling buffers with C-<kp-enter/add>
                                  ;; and positions with  C-S-<kp-enter/add>
(landmark-assign-mwheel-config) ;; for cycling buffers with C-M-mwheel
```

(Be aware that it loads predefined **global** keybindings).

You can customize keybindings with functions described in below
("Refine your configuration").

## Tutorial

Let's give an example with numpad keys. We suppose you have put
`(landmark-assign-kp-n-config)` in your init file and you have a
numpad on your keyboard.

Open a file, go somewhere and hit `C-S-kp-1` (control + shift + numpad
1), from now on hitting `kp-1` (numpad 1) jumps back to this location
(even from another buffer).

Switch to another buffer, hit `C-kp-2` (control + numpad 2, notice the
absence of `shift`). From now on hitting `kp-2` switch back to this
buffer. Switching back and forth between the two landmarks is
performed by hitting `kp-1` and `kp-2` alternatively.

Mnemomic: the `C-` prefix means *store buffer to landmark* and `C-S-`
means *store the location in the buffer to landmark*. The longer is
the prefix, the more precise is the landmark.

## Refine your configuration

If you are not happy with the predefined configurations
(`(landmark-assign-kp-n-config)`, `(landmark-assign-fn-config) `...),
you can map keys differently. For that you have two possible levels of
configuration:

1. `(landmark-assign-three-standard-keys CHAR KEY-SYMB)` assigns the
   three variants of the keystrokes `KEY-SYMB` (i.e. `KEY-SYMB`
   itself, `C-KEY-SYMB` and `C-S-KEY-SYMB`) to landmark CHAR. Example:
```elisp
(landmark-assign-three-standard-keys ?0 'kp-0)
```
2. `landmark-assign-keys` for mapping three independent keystrokes to
a landmark. For instance the following call is equivalent to
`(landmark-assign-three-standard-keys ?0 'kp-0)` (note the subtle use
of `kp-insert`, see the remark below):

```elisp
(landmark-assign-keys ?0 [(control kp-0)] [(control shift kp-insert)] [(kp-0)])
```

The following binds `shift + numpad-0` (instead of `numpad-0`) to
jumping to landmark `?0`: ```elisp (landmark-assign-keys ?0 [(control
kp-0)] [(control shift kp-insert)] [(shift kp-insert)]) ``` By the
way: be careful with `shift` and numpad keys: here `shift numpad 0` is
recognized by emacs as `(shift kp-insert)` instead of `(shift kp-0)`.

## Removing a landmark

Buffer landmarks are not removable.

Position landmarks can be removed by hitting the creating shortcut
(typically C-S-...) when the point is at its location. Hence if point
is not at its location **hitting the shorcut twice** will remove it.

# Implementation

Technically quite simple using markers, markers and overlays.

# Similar packages

## `bm.el`

The very nice `bm.el` (https://github.com/joodland/bm) provides a
similar feature. However there a subtle differences (classified by
opinionated + and -):

+ \+ Each landmark is linked to a different key, so navigating is not
  done by cycling but rather by hitting the landmark key.
+ \+ Position landmarks store a location whereas they store a line in
  `bm.el`.
+ \+ Buffer landmarks allow to simply switch to a given buffer exactly
  like `C-x b`(`switch-to-buffer`) but faster.
- \- you need one "free" key of your keyboard per landmark.
- \- There is currently no persistent landmarks whereas `bm.el` provides
  persistent bookmarks.
- \- Only global landmarks can be defined currently whereas `bm.el`
  provides global or local bookmarks.

All in all, and except for persistent missing landmarks, this makes
landmarks a bit  faster and more flexible:

- flexible because it mixes navigating inside buffers and
  from buffer to buffer.
- It is faster because you jump directly to the landmark you want
  instead of cycling but the drawback is that you need one "free" key
  (or keystroke) of your keyboard per landmark.


# Known bugs

- Several landmarks on the same line is perfectly fine, but only one
  of them is signaled in the fringe. You can jump to each of them
  anyways.
- Undoing after editing at landmark position restores the landmark's
  position and, in principle its fringe indicator.
