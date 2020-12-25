module Latex where

import           Text.LaTeX
import           Text.LaTeX.Base.Class  (LaTeXC, comm0, comm1, comm2, comm3)

config :: (Texy a, Texy b) => a -> b -> LaTeX
config val st = ang $ texy val <> fromString ", " <> texy st

-- CUSTOM COMMANDS FROM TEMPLATE --

drule :: LaTeXC l => l -> l -> l
drule = comm3 "drule" ""

ang :: LaTeXC l => l -> l
ang = comm1 "ang"

bse :: LaTeXC l => l
bse = comm0 "bse"

bsb :: LaTeXC l => l
bsb = comm0 "bsb"

bsc :: LaTeXC l => l
bsc = comm0 "bsc"

true :: LaTeXC l => l
true = comm0 "true"

false :: LaTeXC l => l
false = comm0 "false"

neg :: LaTeXC l => l
neg = comm0 "neg"

ifthen :: LaTeXC l => l -> l -> l -> l
ifthen = comm3 "ifthen"

while :: LaTeXC l => l -> l -> l
while = comm2 "while"
