{-|
Module       : PicParser
Description  : A parser for a subset of the pic programming language.
Maintainer   : CS 131, Programming Languages (Melissa O'Neill, Chris Stone, Ben Wiedermann)
-}

module PicParser where

import PicAST  -- This is the same abstract syntax as last week's assignment
import ParserCombinators


-- | A picture consists of one or more elements (which can handle leading
--   whitespace), possibly followed by unconsumed trailing whitespace
--   characters at the end of the file.
picture :: Parser Picture
picture = some element <+-> whitespace


-- | An element is either a direction command or a shape command
element :: Parser Element
element =     (direction <+-> sym ';'                  >>=: \d -> Turn d)
          <|> (shape <+> many attr <+-> sym ';'        >>=: \(s,a) -> Draw s a)
          <???> "<element> expected"


-- | An attribute is either a literal string or a direction
attr :: Parser Attribute
attr =        (litstring                                >>=: \s -> Label s)
          <|> (direction                                >>=: \d -> Dir d)
          <???> "<attr> expected"


-- | A direction can be "left", "right", "up", or "down"
direction :: Parser Direction
direction =    (rword "left"                            >>: Lt)
           <|> (rword "right"                           >>: Rt)
           <|> (rword "up"                              >>: Up)
           <|> (rword "down"                            >>: Dn)
           <???> "<direction> expected"


-- | A shape can be "box", "circle", "line", "move", or "arrow"
shape :: Parser Shape
shape =        (rword "box"                             >>: Box)
           <|> (rword "circle"                          >>: Circle)
           <|> (rword "line"                            >>: Line)
           <|> (rword "move"                            >>: Move)
           <|> (rword "arrow"                           >>: Arrow)
           <???> "<shape> expected"


-- | A string literal is a sequence of string characters inside quotation
--   marks, optionally preceeded by some whitespace.
litstring = whitespace <-+> (char '"' <-+> stringcontent <+-> char '"')
  where stringcontent = many stringchar

  
-- | A string character is either any character that is not a double quote,
--   or the escape sequence \" (which we interpret as a double quote
--   character inside our string)
stringchar =    (get <=> (\c -> c /= '"'))
            <|> (string "\\\""                           >>: '\"')

