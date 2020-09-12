-- a monopoly like game to demonstrate the state monad

-- an object to record the game state
data GameState = GameState
  { players :: [Player]
  , chancedeck :: [GameCard]
  , properties :: Map Property PropertyState
  , piecePositions ::  Map Player Property
  , generator :: StdGen  }

data PropertyState = Unowned | Owned Player

-- other data types for the chancedeck
data ChanceCard = ...
data Player = ...
data BoardPosition = ...
data GameAction = ....

-- functions required
    -- roll the dice
    -- make a move based on the dice output and current player
    -- piece on the board would change (changing state) and an output action would be required
        -- output action would be draw a card, buy or auction property, pay rent
        -- each of there actions would change the board state .
    -- eg drawing a chace card would update the state of the deck, while returning us a camecard to resolve

-- this function outputs and int to us and modifies the random number generator stored in our state
rollDice :: State GameState Int
rollDice = do
  currentState <- get
  let gen = generator generator currentState
  let (d1, gen') = randomR (1, 6) gen
  let (d2, gen'') = randomR (1, 6) gen'
  put (currentState { generator = gen'' } )
  return (d1 + d2)


-- this function gives us the output of the new property that we landed on while modifying the board
-- with the new position of our piece

movePiece :: Player -> Int -> State GameState Property
movePiece player roll  = do
  currentState <- get
  let currentPositions = piecePositions currentState
  let currentPos = fromJust (M.lookup player currentPositions)
  let next = nextProperty currentPos roll
  let newMap = M.insert player next currentPositions
  put (currentState { piecePositions = newMap } )
  return next

nextProperty :: Property -> Int -> Property
-- write this later

--based on the position, we might take different actions, like drawing a chance chard

-- this function will modify the pile of available chance cards in the pile and return the top card
drawChance :: State GameState ChanceCard
drawChance = do
  currentState <- get
  let (fstCard : deck) = chanceDeck currentState
  put (currentState {chanceDeck = deck})
  return fstCard

-- there are other functions thyat we could implement
-- these exist within the state monad

buyProperty :: Player -> Property -> State GameState ()

payRent :: Player -> Property -> State GameState ()

--combining these functions is quite clean using do syntax,

-- we can combine multiple functions and let the monads handle the side effects



resolveTurn :: State GameState ()
resolveTurn = do
  currentState <- get
  let playerToMove = currentPlayer currentState -- note that currentPlayer does not exist in the data structure yet
  roll <- rollDice
  newProperty <- movePiece playerToMove roll
  action <- actionForProperty playerToMove newProperty
  resolveAction action
  switchNextPlayer
  return ()

--functions that need implementing
-- actionForProperty
-- resolveAction
-- switchNextPlayer
