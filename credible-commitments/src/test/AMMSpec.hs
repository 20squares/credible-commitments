main = do
  verboseCheck prop_eqForallInitialChains


------------------------------------------------
-- Explore tests of given strategies relative to
-- random starting conditions, i.e. chains

-- draw only positive numbers
genPos :: Gen Int
genPos = abs `fmap` (arbitrary :: Gen Int) `suchThat` (\x -> x > 0  && x < 12)

-- draw a random vertice with increasing number given the state
drawNode :: Id -> Gen (Id,Vote)
drawNode id = (,) <$> choose (id,id) <*> genPos


-- draw an id for the chain at hand
drawId :: Chain -> Gen Id
drawId chain =
  let size = vertexCount chain
      in choose (1,size - 1)


-- create a list of vertices with increasing id
listOfVertices :: Arbitrary (Id,Vote) => Id -> Gen [(Id,Vote)]
listOfVertices id = frequency
  [ (1, return [])
  , (4, (:) <$> drawNode id <*> listOfVertices (id + 1))]

-- create a list of vertices with fixed root and increasing id 
listOfVerticesWRoot :: Arbitrary (Id,Vote) => Gen [(Id,Vote)]
listOfVerticesWRoot = do
  ls <- listOfVertices 2
  let withRoot = [(1,2)] ++ ls
  return withRoot 

-- first, generate a simple linear path
drawChain :: Gen [(Id,Vote)] -> Gen Chain
drawChain = fmap path


-- checkEq condition on game given an initial chain
eqForallInitialChains initialChain = 
  checkEq initialChain  == True
  where
   checkEq initialChain =  generateEquilibrium $  evaluate (oneEpisode "p0" "p1" "a10" "a20" "a11" "a21" 2 2) strategyOneEpisode context
   context =  StochasticStatefulContext (pure ((),(initialChain,3,initialMap))) (\_ _ -> pure ())
   initialMap = M.fromList [("a10",3),("a20",3)]

-- construct testable property
prop_eqForallInitialChains = forAll (drawChain $ listOfVerticesWRoot) eqForallInitialChains
