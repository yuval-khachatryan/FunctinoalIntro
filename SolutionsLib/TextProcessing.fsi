module TextProcessing 

open System.Text.RegularExpressions
// Regular expressions
val captureSingle       : Match -> int -> string
val captureList         : Match -> int -> string list
val captureCount        : Match -> int -> int
val captureCountList    : Match -> int list

// File functions
open System.IO

val fileXfold : ('T -> StreamReader -> 'T) -> 'T -> string -> 'T
val fileXiter : (StreamReader -> unit) -> string -> unit
val fileFold  : ('T -> string -> 'T) -> 'T -> string -> 'T
val fileIter  : (string -> unit) -> string -> unit

// File handling
val saveValue    : 'T -> string -> unit
val restoreValue : string -> 'T

// Culture dependent string ordering
open System

exception StringOrderingMishmatch

[<Sealed>]
type orderString = interface IComparable

val orderString : string -> (string -> orderString)
val orderCulture : orderString -> string

