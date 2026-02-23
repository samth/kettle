#lang scribble/manual
@(require (for-label racket/base
                     kettle))

@title{Borders}

Border rendering with predefined and custom styles.

@defstruct[border ([top string?] [bottom string?]
                   [left string?] [right string?]
                   [top-left string?] [top-right string?]
                   [bottom-left string?] [bottom-right string?]
                   [middle-left string?] [middle-right string?]
                   [middle string?]
                   [middle-top string?] [middle-bottom string?]) #:transparent]{
  A complete border specification with glyphs for every position.}

@defproc[(make-border [#:top top string? "─"]
                      [#:bottom bottom string? "─"]
                      [#:left left string? "│"]
                      [#:right right string? "│"]
                      [#:top-left tl string? "┌"]
                      [#:top-right tr string? "┐"]
                      [#:bottom-left bl string? "└"]
                      [#:bottom-right br string? "┘"]) border?]{
  Create a custom border (simplified signature).}

@section{Predefined Border Styles}

@defthing[border-normal border?]{Standard single-line box drawing: @tt{┌─┐│└┘}.}
@defthing[border-rounded border?]{Rounded corners: @tt{╭─╮│╰╯}.}
@defthing[border-thick border?]{Heavy/thick lines: @tt{┏━┓┃┗┛}.}
@defthing[border-double border?]{Double lines: @tt{╔═╗║╚╝}.}
@defthing[border-block border?]{Full block characters: @tt{████}.}
@defthing[border-hidden border?]{Invisible border (spaces).}
@defthing[border-ascii border?]{ASCII-only: @tt{+-+|+-+}.}
@defthing[border-markdown border?]{Markdown table style.}

@section{Rendering}

@defproc[(render-border [text string?] [border border?]
                        [#:top show-top boolean? #t]
                        [#:bottom show-bottom boolean? #t]
                        [#:left show-left boolean? #t]
                        [#:right show-right boolean? #t]
                        [#:fg-color fg-color (or/c string? #f) #f]
                        [#:bg-color bg-color (or/c string? #f) #f]
                        [#:title title (or/c string? #f) #f]
                        [#:title-position title-position (or/c 'left 'center 'right) 'center]) string?]{
  Render text with a border around it. Optionally include a title in
  the top border.}

@defproc[(render-shadow [text string?]
                        [#:width shadow-width exact-nonnegative-integer? 2]
                        [#:offset offset exact-nonnegative-integer? 1]
                        [#:style shadow-style (or/c 'solid 'light 'medium 'heavy 'dark) 'dark]) string?]{
  Add a drop shadow to a text block.}

@defproc[(shadow-chars [style symbol?]) (cons/c string? string?)]{
  Return @racket[(cons right-char bottom-char)] for the given shadow style.}
