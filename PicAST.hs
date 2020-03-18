{-|
Module       : PicAST
Description  : Defines abstract syntax for pic programs.
Maintainer   : CS 131, Programming Languages (Melissa O'Neill, Chris Stone, Ben Wiedermann)
-}

module PicAST where

-- | A picture is a (sequence) of elements.
type Picture = [Element]

-- | Elements: Draw or turn, possibly updating the state
data Element = Draw      Shape [Attribute]  -- ^ draw a shape (with the given attributes)
             | Turn      Direction          -- ^ change the current direction
               deriving (Show, Eq, Ord)

-- | Shapes: boxes, circles, lines, moves, and arrows.
data Shape = Box
           | Circle
           | Line
           | Move    -- ^ a shape because it's an invisible line
           | Arrow
               deriving (Show, Eq, Ord)

-- | Attributes for shapes: Labels or directions
data Attribute = Label String      -- ^ a quoted string
               | Dir   Direction   -- ^ a direction
               deriving (Show, Eq, Ord)

-- | Directions: left/right/up/down
data Direction = Lt | Rt | Up | Dn
               deriving (Show, Eq, Ord)

