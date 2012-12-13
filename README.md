# TURIPONG

Turipong is inspired by Befunge and Pong for the Dec2012 PLT Games.
Befunge is too straightforward and useful.  Enter Turipong:

* Balls are state, each contains a stack
* Walls are operations
* Flow control is literally redirecting the ball
* Parallelism is multiple balls

Walls are placed about a field.  To keep things simple for version
1.0, there aren't player-controlled paddles.  Input is via a text
file.  Operations are as follows:

* `" "` (blank, space): The ball passes through.
* `[a-zA-Z0-9_]`: Append an alphanumeric character to the top of the
  stack, and bounce. "_" appends a space character.
* `,`: Push a new blank onto the stack, if there is not already a
  _blank_ value. (Can separate other non-blank false values.)  Think
  of this as a "stack separator".  Bounce.
* ``"`"`` (backtick): Pop a value from the stack.  It gets lost.
  Bounce.
* ``:``: Duplicate the value on the stack.  Bounce.
* ``;``: Swap the top two values on the stack.  Bounce.
* `>`: Output the top of the stack to stdout, and bounce.  If the top
  of the stack is a _blank_ (the blank string, via `,`, not a space),
  a newline is printed.
* `<`: Input one char, appending to the top value on the stack, as
  per alphanumerics.  If there is no input, nothing is appended.  Bounce.
* `?`: Test.  If the top of the stack is a blank, space, or zero, the
  ball will pass through as if the "?" is a " ".  Otherwise, bounce.
* `!`: Not.  If the state of the ball is other than false, the state
  is set to 0.  If the state is false, the state is set to "1".  In
  any case, bounce.
* `+`: Add the top two numbers on the stack, or concatenate the top
  two strings on the stack.  Bounce.
* `-`: Subtract the top two numbers on the stack.  Note this subtracts
  the topmost _from_ the next-topmost; e.g., if you push 2, then 1,
  and subtract, you get 1, not -1.  Bounce.
* `@`: The ball.  Balls bounce off each other.  When doing so, they
  exchange the top of their stack.  Balls always start off moving at a
  45° angle in the southeast direction.  When a ball leaves the field,
  its state is removed and it is considered to have exited.  When all
  balls have exited, the program finishes.
* Other: Bounce.  The stack is unchanged.

# Usage

This is written in Common Lisp and will thus require a Common Lisp to
run.  Put `turipong.asd` somewhere your CL can find it, and do the
following:

```lisp
(asdf:load-system :turipong)
```

You can run Turipong files in one of the following ways:

```lisp
;; Basic load and run:
(turipong:run-program-file "/PATH/TO/FILE.tp")

;; Load and run with ANSI output, only works on a term:
(turipong.visual:run-program-file "/PATH/TO/FILE.tp")

;; Load and print a lot, not pretty, but possibly useful:
(let ((program (turipong:read-program-file "/PATH/TO/FILE")))
  (turipong:print-program program)
  (loop while (turipong:runningp program) do
    (turipong:run-iteration program)
    (turipong:print-program program)))
```

Note that your CL will probably default to `$HOME` as the current
working directory.

# Types

Turipong is weakly-typed.  The stack always contains a string, but
this may be interpreted as a number if it contains only digits.  The
space character or zero are considered false, as is an actual blank
(i.e., zero-length string), which can be obtained using ",".

# Bounces

Understanding the exact method for bouncing is critical.  A ball has a
given motion vector `#(ROW COLUMN)`.  However, bounces happen on
surfaces first:

```
   @X
```

If this were a roguelike, @ could probably move past X diagonally with
no problem.  However, in Turipong, this is a bounce (slashes used for
illustration):

```
  \
   @X
  /
```

Likewise:

```
 \ /
  @
  X
```

Of course, if we do hit edges, the ball will bounce back directly:

```
 \
  @
   X
```

The ball will return the direction it came from.  However, what
happens in corner cases, such as the following?

```
 \
  @X
  YZ
```

You might predict that the ball returns in the direction it comes
from, and you'd be right, but what is the state of the ball after
this?  The state is actually _randomly_ "XY" or "YX".  But not Z!
This is due to how bounces are processed:

* First, _randomly_ (50/50) check up/down or left/right
* Update the vector
* Check the opposite (left/right or up/down)
* Update the vector
* Check the diagonal

In this case, we hit `X` or `Y`, then `Y` or `X`, and the vector is
now `#(-1 -1)`.  Since there's nothing there (our "\" is just for
illustration!), the ball will be moving away with a random value.

This is, of course, useful for producing random values.  Like
everything else, actually making use of this may prove to be a bit of
work.

# Conflicts

One type of conflict may happen if two balls attempt to move onto the
same square:

```
 @   @
        1
        1
        1
 >
 >
 >
 >
 >
```

After a few steps, the balls will be moving in opposite directions,
and try and collide (the "*" is a marker only):

```
          1
          1
      @ @ 1
   >   *
   >
   >
   >
   >
```

These will both try and occupy the space with the "*".  Motion happens
in parallel, but Turipong will abort _one_ of the balls' intended
motion, and it will be evaluated again the following iteration.

Note that _effects_ will happen on both rounds.  However, in this
case, no effects happened on the round both balls tried to occupy the
"*", because the space is blank.

# Examples

## hello world

Doing some basic things is fairly simple:

```
  @   eee lll ___ ooo lll >>>



    hhh lll ooo www rrr ddd
```

This prints "hello world" and exits.  Of course, walls need not be
more than one character, but it may make things easier.

## Basic Operations

Here are some basic operations framed in Turipong:

* [Test](https://github.com/rpav/turipong/blob/master/examples/add.tp):
  Basic test for equality; in this case we push 1 and 2 on the stack,
  subtract, and test.  If they are equal, the ball will pass through
  the `???` and print `y`, otherwise it will bounce and print `n`.

* [I/O](https://github.com/rpav/turipong/blob/master/examples/input.tp):
  Nothing special, just print three letters, input three characters,
  printing them.  Also demonstrates newlines.

* [Message Passing](https://github.com/rpav/turipong/blob/master/examples/swap.tp):
  Parallelism isn't useful if processes can't communicate.
  Fortunately, Turipong implements message passing via you precisely
  timing the bounce of the ball.

* [Loops](https://github.com/rpav/turipong/blob/master/examples/loop.tp):
  Make the ball go around and count down.  This example could
  obviously use some optimization... tighten up the loop by actually
  shrinking it down!

## Beyond

Further examples are left to the reader as an exercise, probably in
frustration.  Turipong has just enough operations to make things seem
easy, and just enough practical limitations to make them all painfully
difficult.  You will likely need to make heavy use of the visualizer,
Emacs' picture-mode, and trial and error.
