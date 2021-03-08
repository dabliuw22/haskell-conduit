module Basic.Conduit (run) where

import Conduit
  ( ConduitT,
    Identity,
    Void,
    await,
    concatC,
    foldC,
    mapC,
    mapMC,
    mapM_C,
    runConduit,
    runConduitPure,
    sinkList,
    sumC,
    takeC,
    yield,
    yieldMany,
    (.|),
  )

simple :: ConduitT () Int Identity ()
simple = yield 1

-- data ConduitT i o m r
source :: ConduitT () Int IO () -- Source IO Int
source = yieldMany [1 .. 10] -- CL.sourceList [1..10]

sink :: ConduitT String Void IO () -- Sink String IO ()
sink = mapM_C putStr --CL.mapM_ putStr

conduit :: ConduitT Int String IO () -- Conduit Int IO String
conduit = mapC show -- CL.map show

monadicSource :: Monad m => ConduitT () Int m ()
monadicSource = do
  yieldMany [1 .. 10]
  yieldMany [11 .. 20]

monadicSink :: Monad m => ConduitT Int Void m (String, Int)
monadicSink = do
  x <- takeC 5 .| mapC show .| foldC
  y <- sumC
  return (x, y)

applicativeSink :: Monad f => ConduitT Int Void f (String, Int)
applicativeSink = (,) <$> (takeC 5 .| mapC show .| foldC) <*> sumC

run :: IO ()
run = do
  -- p <- runConduit $ source .| takeC 2 .| sinkList
  let m = runConduitPure $ monadicSource .| monadicSink
  print m
  let a = runConduitPure $ monadicSource .| applicativeSink
  print a
  let y = runConduitPure $ simple .| await
  print y
  _ <-
    runConduit $
      yieldMany (map (replicate 2) [1 .. 10])
        .| concatC
        .| mapM_C print
  p <-
    runConduit $
      source
        .| conduit
        .| mapMC (\i -> pure (i <> ".."))
        .| sinkList --  source $$ conduit =$ sink
  print p
