module Rune.IR.Optimizer
( runIROptimizer
) where

import Rune.IR.Nodes (IRProgram)

--
-- public
--

runIROptimizer :: IRProgram -> IRProgram
runIROptimizer p = p
