type wave = 
  | Clock 
  | Binary of int array
  | Data of int array

type t = string * wave

