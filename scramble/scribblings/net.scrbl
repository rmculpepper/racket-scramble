#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract scramble/number scramble/net/addr))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require scramble/net/addr)))

@title[#:tag "net"]{Networking}

@; ----------------------------------------
@section[#:tag "net-addr"]{Network Addresses}

@defmodule[scramble/net/addr]

@history[#:added "0.4"]

@subsection[#:tag "net-addr-ip4"]{IPv4 Addresses}

@defstruct*[ip4 ([number ip4-number?])]{

Represents an IPv4 internet address.

The @racket[ip4] constructor can be given any @racket[ip4-in/c] value, but the
address is converted to a 32-bit unsigned integer for storage.

Instances of the @racket[ip4] structure use the string form of @racket[address]
for printing.

@examples[#:eval the-eval #:escape unsyntax
(eval:alts (ip4 #,(racketvalfont "#x7F000001"))
           (ip4 #x7F000001))
(ip4 '(11 22 33 44))
]}

@defthing[ip4-in/c flat-contract?
          #:value (or/c ip4? ip4-number? ip4-bytes? ip4-string? ip4-list?)]{

Contract for values that can be interpreted as IPv4 addresses.
}

@defproc[(ip4-number? [v any/c]) boolean?]{

Accepts any 32-bit unsigned integer. Equivalent to @racket[uint32?].
}

@defproc[(ip4-bytes? [v any/c]) boolean?]{

Accepts any byte string of four bytes. It is intepreted as an IPv4 address
number in network byte order (big-endian).
}

@defproc[(ip4-string? [v any/c]) boolean?]{

Accepts strings containing the ``dotted-decimal'' notation for IPv4 addresses.

@examples[#:eval the-eval
(ip4-string? "127.0.0.1")
(code:line (ip4-string? "12.34.56.789") (code:comment "last component out of range"))
(code:line (ip4-string? "11.22.3.4.56") (code:comment "too many components"))
]}

@defproc[(ip4-list? [v any/c]) boolean?]{

Accepts any list of four @racket[uint8?] values.

@examples[#:eval the-eval
(ip4-list? '(127 0 0 1))
]}

@deftogether[[
@defproc[(ip4->number [addr ip4-in/c]) ip4-number?]
@defproc[(ip4->bytes [addr ip4-in/c]) ip4-bytes?]
@defproc[(ip4->string [addr ip4-in/c]) ip4-string?]
@defproc[(ip4->list [addr ip4-in/c]) ip4-list]
]]{

Converts the given IPv4 @racket[addr] to its number, byte string, string, or
list representation, respectively.

@examples[#:eval the-eval
(ip4->number '(127 0 0 1))
(ip4->bytes '(127 0 0 1))
(ip4->string (ip4 '(11 22 33 44)))
(eval:alts (ip4->list #,(racketvalfont "#x7F000001"))
           (ip4->list #x7F000001))
]}

@defproc[(string->ip4 [address-string string?]) (or/c ip4? #f)]{

If @racket[address-string] is a valid IPv4 representation, converts it to an
@racket[ip4] instance; otherwise, returns @racket[#f].
}

@defstruct*[cidr4 ([address ip4-number?] [prefix (integer-in 0 32)])]{

Represents a CIDR set of IPv4 addresses. In particular, it represents the set of
addresses whose highest @racket[prefix] bits match the high bits of
@racket[address].

Like @racket[ip4], the @racket[address] field is stored as an integer but can be
given to the constructor in any @racket[ip4-in/c] form. The constructor
automatically clears the bits of @racket[address] after the first
@racket[prefix] bits.

@examples[#:eval the-eval
(cidr4 "11.22.0.0" 16)
(code:line (cidr4 "11.22.33.44" 16) (code:comment "low bits cleared"))
(code:line (cidr4 '(255 0 0 1) 4) (code:comment "low bits cleared"))
]}

@defproc[(cidr4-contains? [cidr cidr4?] [addr ip4-in/c]) boolean?]{

Returns @racket[#t] if @racket[cidr] represents a set of addresses including
@racket[addr]; otherwise, returns @racket[#f].

@examples[#:eval the-eval
(cidr4-contains? (cidr4 "11.22.0.0" 16) "11.22.33.44")
(cidr4-contains? (cidr4 "11.22.0.0" 16) "11.34.5.6")
]}

@defproc[(cidr4-range [cidr cidr4]) (values ip4-number? ip4-number?)]{

Returns the minimum and maximum IPv4 address numbers in the contiguous range
represented by @racket[cidr].

@examples[#:eval the-eval
(cidr4-range (cidr4 '(192 168 0 0) 16))
]}

@; ----------------------------------------
@subsection[#:tag "net-addr-ip6"]{IPv6 Addresses}

@defstruct*[ip6 ([number ip6-number?])]{

Represents an IPv6 internet address.

The @racket[ip6] constructor can be given any @racket[ip6-in/c] value, but the
address is converted to a 128-bit unsigned integer for storage.

Instances of the @racket[ip6] structure use the string form of @racket[address]
for printing.

@examples[#:eval the-eval #:escape unsyntax
(eval:alts (ip6 #,(racketvalfont "#x260647003036000000000000681530eb"))
           (ip6 #x260647003036000000000000681530eb))
(ip6 '(#x2606 #x4700 #x3036 0 0 0 #x6815 #x30eb))
(ip6 "2606:4700:3036::6815:30eb")
]}

@defthing[ip6-in/c flat-contract?
          #:value (or/c ip6? ip6-number? ip6-bytes? ip6-string? ip6-list?)]{

Contract for values that can be interpreted as IPv6 addresses.
}

@defproc[(ip6-number? [v any/c]) boolean?]{

Accepts any 128-bit unsigned integer. Equivalent to @racket[uint128?].
}

@defproc[(ip6-bytes? [v any/c]) boolean?]{

Accepts any byte string of 16 bytes. It is intepreted as an IPv6 address
number in network byte order (big-endian).
}

@defproc[(ip6-string? [v any/c]) boolean?]{

Accepts strings containing the notation for IPv6 addresses described by
@hyperlink["https://datatracker.ietf.org/doc/html/rfc1884"]{RFC 1884},
Section 2.2.

@examples[#:eval the-eval
(ip6-string? "::1")
(ip6-string? "2606:4700:3036::6815:30eb")
(ip6-string? "2606:4700:3036:0:0:0:6815:30eb")
(ip6-string? "::FFFF:11.22.33.44")
]}

@defproc[(ip6-list? [v any/c]) boolean?]{

Accepts any list of eight @racket[uint16?] values.
}

@deftogether[[
@defproc[(ip6->number [addr ip6-in/c]) ip6-number?]
@defproc[(ip6->bytes [addr ip6-in/c]) ip6-bytes?]
@defproc[(ip6->string [addr ip6-in/c]) ip6-string?]
@defproc[(ip6->list [addr ip6-in/c]) ip6-list]
]]{

Converts the given IPv6 @racket[addr] to its number, byte string, string, or
list representation, respectively.

@examples[#:eval the-eval
(ip6->number "2606:4700:3036::6815:30eb")
(ip6->list "2606:4700:3036::6815:30eb")
(ip6->list "::FFFF:11.22.33.44")
]}

@defproc[(string->ip6 [address-string string?]) (or/c ip6? #f)]{

If @racket[address-string] is a valid IPv6 representation, converts it to an
@racket[ip6] instance; otherwise, returns @racket[#f].
}

@defstruct*[cidr6 ([address ip6-number?] [prefix (integer-in 0 128)])]{

Represents a CIDR set of IPv6 addresses. In particular, it represents the set of
addresses whose highest @racket[prefix] bits match the high bits of
@racket[address].

Like @racket[ip6], the @racket[address] field is stored as an integer but can be
given to the constructor in any @racket[ip6-in/c] form. The constructor
automatically clears the bits of @racket[address] after the first
@racket[prefix] bits.

@examples[#:eval the-eval
(cidr6 "FE80::" 10)
]}

@defproc[(cidr6-contains? [cidr cidr6?] [addr ip6-in/c]) boolean?]{

Returns @racket[#t] if @racket[cidr] represents a set of addresses including
@racket[addr]; otherwise, returns @racket[#f].

@examples[#:eval the-eval
(cidr6-contains? (cidr6 "FE80::" 10) "FE81:1234::5:67")
]}

@defproc[(cidr6-range [cidr cidr6]) (values ip6-number? ip6-number?)]{

Returns the minimum and maximum IPv6 address numbers in the contiguous range
represented by @racket[cidr].

@examples[#:eval the-eval
(cidr6-range (cidr6 "FE80::" 10))
]}

@(close-eval the-eval)
