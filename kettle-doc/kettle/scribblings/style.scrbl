#lang scribble/manual
@(require (for-label racket/base
                     kettle))

@title{Styling}

Terminal text styling with colors, attributes, and text reflow.

@section{ANSI Colors}

@defproc[(ansi-color [code string?]) string?]{
  Produce an ANSI escape sequence for the given color code.}

@defproc[(ansi-reset) string?]{
  Produce the ANSI reset escape sequence.}

@defproc[(color-256 [n exact-nonnegative-integer?]
                    [#:foreground foreground boolean? #t]) string?]{
  Produce a 256-color escape code. Set @racket[foreground] to @racket[#f]
  for background.}

@defproc[(color-rgb [r byte?] [g byte?] [b byte?]
                    [#:foreground foreground boolean? #t]) string?]{
  Produce a 24-bit true-color escape code.}

@defproc[(parse-hex-color [hex string?]
                          [#:foreground foreground boolean? #t]) string?]{
  Parse a hex color string (e.g., @racket["#ff0000"] or @racket["#f00"])
  and produce a color escape code.}

@section{Adaptive Colors}

@defstruct[adaptive-color ([light string?] [dark string?]) #:transparent]{
  A color that adapts to light or dark terminal backgrounds.}

@defstruct[complete-color ([truecolor (or/c string? #f)]
                           [ansi256 (or/c exact-nonnegative-integer? #f)]
                           [ansi (or/c string? #f)]) #:transparent]{
  A color with fallbacks for different terminal capabilities.}

@defstruct[complete-adaptive-color ([light complete-color?]
                                    [dark complete-color?]) #:transparent]{
  An adaptive color with complete fallback chains for each mode.}

@defproc[(detect-dark-background) boolean?]{
  Detect whether the terminal has a dark background.}

@defproc[(detect-color-support) (or/c 'truecolor '256color '16color 'monochrome)]{
  Detect the terminal's color support level.}

@defproc[(resolve-color [color any/c]) any/c]{
  Resolve a @racket[complete-color] to the best available code.}

@defproc[(resolve-adaptive-color [color any/c]) any/c]{
  Resolve an adaptive color based on terminal background.}

@section{Named Color Constants}

Foreground colors: @racket[fg-black], @racket[fg-red], @racket[fg-green],
@racket[fg-yellow], @racket[fg-blue], @racket[fg-magenta], @racket[fg-cyan],
@racket[fg-white], and their @tt{bright-} variants.

Background colors: @racket[bg-black], @racket[bg-red], @racket[bg-green],
@racket[bg-yellow], @racket[bg-blue], @racket[bg-magenta], @racket[bg-cyan],
@racket[bg-white], and their @tt{bright-} variants.

@section{Style}

@defstruct[style ([foreground any/c] [background any/c]
                  [bold? boolean?] [italic? boolean?]
                  [underline? boolean?] [blink? boolean?]
                  [reverse? boolean?] [strikethrough? boolean?]
                  [faint? boolean?]
                  [padding-left exact-nonnegative-integer?]
                  [padding-right exact-nonnegative-integer?]
                  [padding-top exact-nonnegative-integer?]
                  [padding-bottom exact-nonnegative-integer?]
                  [margin-left exact-nonnegative-integer?]
                  [margin-right exact-nonnegative-integer?]
                  [margin-top exact-nonnegative-integer?]
                  [margin-bottom exact-nonnegative-integer?]
                  [width (or/c exact-nonnegative-integer? #f)]
                  [height (or/c exact-nonnegative-integer? #f)]
                  [max-width (or/c exact-nonnegative-integer? #f)]
                  [max-height (or/c exact-nonnegative-integer? #f)]
                  [inline? boolean?]
                  [align (or/c 'left 'right 'center)]) #:transparent]{
  A complete style specification with colors, attributes, padding,
  margins, and layout constraints.}

@defproc[(make-style [#:foreground fg any/c #f]
                     [#:background bg any/c #f]
                     [#:bold bold? boolean? #f]
                     [#:italic italic? boolean? #f]
                     [#:underline underline? boolean? #f]
                     [#:padding padding exact-nonnegative-integer? 0]
                     [#:margin margin exact-nonnegative-integer? 0]
                     [#:width width (or/c exact-nonnegative-integer? #f) #f]
                     [#:height height (or/c exact-nonnegative-integer? #f) #f]
                     [#:align align (or/c 'left 'right 'center) 'left]) style?]{
  Create a style (simplified signature; see source for all keyword args).}

@defproc[(render-styled [s style?] [text string?]) string?]{
  Render @racket[text] with the given style applied.}

@defproc[(bold [text string?]) string?]{Bold text.}
@defproc[(italic [text string?]) string?]{Italic text.}
@defproc[(underline [text string?]) string?]{Underlined text.}
@defproc[(colored [text string?] [#:fg fg any/c #f] [#:bg bg any/c #f]) string?]{
  Apply foreground and/or background color.}

@section{Text Reflow}

@defproc[(wrap-text [text string?] [width exact-positive-integer?]
                    [#:break-words break-words boolean? #f]
                    [#:normalize-spaces normalize-spaces boolean? #t]) string?]{
  Word-wrap text to the given width.}

@defproc[(truncate-text [text string?] [width exact-nonnegative-integer?]
                        [#:ellipsis ellipsis string? "\u2026"]) string?]{
  Truncate text to the given visible width, adding an ellipsis if truncated.}

@defproc[(ellipsize [text string?] [width exact-nonnegative-integer?]) string?]{
  Shorthand for @racket[truncate-text] with the default ellipsis.}

@defproc[(indent-lines [text string?] [n exact-nonnegative-integer?]) string?]{
  Indent every line of @racket[text] by @racket[n] spaces.}

@section{Measurement}

@defproc[(text-width [text string?]) exact-nonnegative-integer?]{
  Width of the widest line (excluding ANSI escapes).}

@defproc[(text-height [text string?]) exact-nonnegative-integer?]{
  Number of lines.}

@defproc[(text-size [text string?]) (values exact-nonnegative-integer? exact-nonnegative-integer?)]{
  Returns @racket[(values width height)].}

@defproc[(visible-length [str string?]) exact-nonnegative-integer?]{
  Visible display width of @racket[str], excluding ANSI escape sequences.}

@defproc[(split-string-by-newline [str string?]) (listof string?)]{
  Split a string by newlines, stripping trailing carriage returns.}
