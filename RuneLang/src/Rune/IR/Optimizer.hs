{-# OPTIONS_GHC -cpp #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Optimizer
  ( runIROptimizer
  )
where
#else
module Rune.IR.Optimizer
  ( runIROptimizer
  )
where
#endif

import Rune.IR.Nodes

--
-- public
--

runIROptimizer :: IRProgram -> IRProgram
runIROptimizer (IRProgram name tops) = IRProgram name (optimizeTopLevel <$> tops)


--
-- private
--

optimizeTopLevel :: IRTopLevel -> IRTopLevel
optimizeTopLevel = id
