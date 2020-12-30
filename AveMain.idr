module Main

import Average
import System.REPL

main : IO ()
main = repl "Enter a string: "
            showAverage
