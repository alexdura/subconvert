module SubContainer where

data Metadata = Metadata {
  desc :: String
  } deriving Show

data FontStyle = FontStyle {
  face :: String,
  family :: String,
  size :: String,
  color :: String,
  weight :: String,
  italic :: String,
  underline :: String,
  alpha :: String,
  back_color :: String,
  outline_color :: String,
  outline_level :: String,
  shadow_color :: String,
  shadow_level :: String,
  wrap :: String
  } deriving Show

data Position = Position {
  alignment :: String,
  horizontal_margin :: String,
  vertical_margin :: String,
  relative_to :: String,
  rotate_x :: String,
  rotate_y :: String,
  rotate_z :: String
  } deriving Show

data Style = Style {
  name :: String,
  fontStyle :: FontStyle,
  position :: Position
  } deriving Show

data TextChunk = TextChunk {
  format :: Maybe FontStyle,
  text :: String
  } deriving Show

data Text = Text {
  style :: Maybe Style,
  chunks :: TextChunk
  } deriving Show

data Subtitle = Subtitle {
  start :: Int,
  stop :: Int,
  subtext :: [Text]
  } deriving Show

data SubContainer = SubContainer {
  metadata :: Metadata,
  styles :: [Style],
  sub :: [Subtitle]
} deriving Show
