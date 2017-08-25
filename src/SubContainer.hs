module SubContainer where

data Metadata = Metadata {
  desc :: String
  } deriving (Show, Eq)

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
  } deriving (Show, Eq)

data Position = Position {
  alignment :: String,
  horizontal_margin :: String,
  vertical_margin :: String,
  relative_to :: String,
  rotate_x :: String,
  rotate_y :: String,
  rotate_z :: String
  } deriving (Show, Eq)

data Style = Style {
  name :: String,
  fontStyle :: FontStyle,
  position :: Position
  } deriving (Show, Eq)

data TextChunk = TextChunk {
  format :: Maybe FontStyle,
  text :: String
  } deriving (Show, Eq)

data Text = Text {
  style_name :: Maybe String,
  chunks :: TextChunk
  } deriving (Show, Eq)

data Subtitle = Subtitle {
  start :: Int,
  stop :: Int,
  subtext :: [Text]
  } deriving (Show, Eq)

data SubContainer = SubContainer {
  metadata :: Metadata,
  styles :: [Style],
  sub :: [Subtitle]
} deriving (Show, Eq)
