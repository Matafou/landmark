# Landmarks

This package provides so called "landmarks" to emacs.

Landmarks are a fast navigation facility for emacs, based on emacs
"registers", but faster to use. Similarly to an emacs register, a
landmark is a location where you might want to come back later. Unlike
registers it is either

- a buffer (jumping back to a \"buffer landmark\" jumps to the buffer
  at its current point position)
- or a precise location in a buffer (\"position landmark\").

Like registers, each landmark is identified uniquely by a character
but this is anecdotical. More importantly it is also attached to a
**key of your keyboard**. Jumping to the landmark is done by hitting
that key (typically a numpad key or a "f" key).

Unlike registers, (position) landmarks are visible. This is
configurable using `landmark-face`, `landmark-show-landmark-position`
and `landmark-show-landmark-fringe`.

## Installation

Load the file `landmarks.el` and put it in a directory known to emacs.
Then add the following line to your init file:

```elisp
(require 'landmark)
```

Then if you have a numeric pad on your keyboard you should try this
predefined configuration, which maps a landmark to each numpad number
keys `kp-0` to `kp-9`:

```
(landmark-assign-kp-n-config) ;; if you have a numpad
```

(Be aware that it loads predefined **global** keybindings).

If you don't have a numpad you should experiment with the predefined
configuration which maps a landmark to each `F5` to `F9` keys using
"F" keys:


```elisp
(landmark-assign-fn-config)  ;; if you have "fn" keys
```

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

If you are not happy with the two predefined configurations
(`(landmark-assign-kp-n-config)` or `(landmark-assign-fn-config) `),
you can map keys differently. For that you have two possible levels of
configuration:

1- `(landmark-assign-three-standard-keys CHAR \'KEY)` assigns the
three variants of the keystrokes `KEY` to landmark CHAR. See below for
details.

    ```elisp
    (landmark-assign-three-standard-keys ?0 'kp-0)
    (landmark-assign-three-standard-keys ?0 'kp-insert)
    ```

2- `landmark-assign-keys` for mapping three independent keystrokes to
a landmark. For instance the follwong binds `shift + numpad-1` to
jumping to landmark `?1` instead of `numpad-1`:

    ```elisp
    (landmark-assign-keys ?1 [(control kp-1)] [(control shift kp-end)] [(shift kp-end)])
    ```

By the way: be careful with shift and numpad, as `shift numpad 1` is
recognized by emacs as `(shift kp-end)` instead of `(shift kp-1)`.

# Implementation

Technically this is a simple wapper on registers.

