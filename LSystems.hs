module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type Angle
  = Float

type Axiom
  = String

type LSystem
  = (Angle, Axiom, Rules)

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Command
  = Char

type Commands
  = [Command]

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (angle, _axiom, _rules)
  = angle

-- Returns the axiom string for the given system.
axiom :: LSystem -> String
axiom (_angle, axiom, _rules)
  = axiom

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules
rules (_angle, _axiom, rules)
  = rules

-- Return the binding for the given character in the list of rules
lookupChar :: Char -> Rules -> String
-- Pre: the character has a binding in the Rules list
lookupChar char ((key, value) : rules)
  | char == key = value
  | otherwise   = lookupChar char rules
lookupChar _ [] = ""

-- Expand command string s once using rule table r
expandOne :: String -> Rules -> String
expandOne axiom@(x : xs) rules
  = lookupChar x rules ++ expandOne xs rules
expandOne [] _ = ""


-- Expand command string s n times using rule table r
expand :: String -> Int -> Rules -> String
expand axiom n rules
  | n > 0     = expand (expandOne axiom rules) (n - 1) rules
  | otherwise = axiom


-- Move a turtle
move :: Command -> Angle -> TurtleState -> TurtleState
move 'L' angle (pos, angle') = (pos, (angle' + angle ))
move 'R' angle (pos, angle') = (pos, (angle' - angle))
move 'F' angle ((x, y), angle')
  =  ((x + cos(angle' * pi / 180), y + sin(angle' * pi / 180)), angle')
move  _ _ _ = error "Move Invalid"


--
-- Trace lines drawn by a turtle using the given colour, following the
-- commands in `cs' and assuming the given angle of rotation.

trace1 :: Commands -> Angle -> Colour -> [ColouredLine]
trace1 (command : commands) angle colour
  = fst (trace1' commands angle ((0,0), 90))
  where
    -- trace1' ( '[' : commands) angle state
    --   = trace1' commands angle state
    -- trace1' ( ']' : commands) angle state
    --   = ([], commands)
    trace1' (command : commands) angle state
      | command == 'F'                   = (line : trace, commands')
      | command == 'L' || command == 'R' = (trace, commands')
      where
        endState@(endPos, endAngle)  = move command angle state
        (trace, commands') = trace1' commands angle endState
        (startPos, _)      = state
        line               = (startPos, endPos, colour)

    trace1' _ _ _ = ([], "")



trace2 :: Commands -> Angle -> Colour -> [ColouredLine]
trace2 commands angle colour
  = trace2' commands angle colour ((0,0), 90) []
  where
    trace2' (command : commands) angle colour state stack
      | command == 'F' = (startPos, endPos, colour) : trace2' commands angle colour endState stack
      | command == 'L' || command == 'R' = trace2' commands angle colour endState stack
      | command == '[' = trace2' commands angle colour state (state : stack)
      where
        endState@(endPos, _endAngle)  = move command angle state
        (startPos, _startAngle)       = state
    trace2' (']' : commands) angle colour state (top : rest)
      = trace2' commands angle colour top rest
    trace2' _ _ _ _ _ = []

----------------------------------------------------------
-- Some given functions

expandLSystem :: LSystem -> Int -> String
expandLSystem (_, axiom, rs) n
  = expandOne (expand axiom n rs) commandMap

drawLSystem1 :: LSystem -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (expandLSystem system n) (angle system) colour)

drawLSystem2 :: LSystem -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (expandLSystem system n) (angle system) colour)

----------------------------------------------------------
-- Some test systems.

cross, triangle, arrowHead, peanoGosper, dragon, snowflake, tree, bush :: LSystem

cross
  = (90,
     "M-M-M-M",
     [('M', "M-M+M+MM-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

triangle
  = (90,
     "-M",
     [('M', "M+M-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

arrowHead
  = (60,
     "N",
     [('M', "N+M+N"),
      ('N', "M-N-M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

peanoGosper
  = (60,
     "M",
     [('M', "M+N++N-M--MM-N+"),
      ('N', "-M+NN++N+M--M-N"),
      ('+', "+"),
      ('-', "-")
     ]
    )

dragon
  = (45,
     "MX",
     [('M', "A"),
      ('X', "+MX--MY+"),
      ('Y', "-MX++MY-"),
      ('A', "A"),
      ('+', "+"),
      ('-', "-")
     ]
    )

snowflake
  = (60,
     "M--M--M",
     [('M', "M+M--M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

tree
  = (45,
     "M",
     [('M', "N[-M][+M][NM]"),
      ('N', "NN"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

bush
  = (22.5,
     "X",
     [('X', "M-[[X]+X]+M[+MX]-X"),
      ('M', "MM"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

commandMap :: Rules
commandMap
  = [('M', "F"),
     ('N', "F"),
     ('X', ""),
     ('Y', ""),
     ('A', ""),
     ('[', "["),
     (']', "]"),
     ('+', "L"),
     ('-', "R")
    ]
