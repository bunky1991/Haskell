import Data.List
import Data.Char

type Location = String
type Direction = Char
type Thing = String
type Response = String

type PathMap = [((Location, Direction), Location)]
paths:: PathMap
paths = [
    (("room1", 'w'), "room2"),
    (("room2", 's'), "room1"),
    (("room2", 'w'), "room3"),
    (("room3", 's'), "room2"),
    (("room3", 'a'), "room4"),
    (("room4", 'd'), "room3"),
    (("room3", 'd'), "room5"),
    (("room5", 'a'), "room3"),
    (("room5", 'w'), "room7"),
    (("room7", 's'), "room5"),
    (("room4", 'w'), "room6"),
    (("room6", 's'), "room4"),
    (("room6", 'w'), "room8"),
    (("room8", 's'), "room6"),
    (("room7", 'w'), "room9"),
    (("room9", 's'), "room7"),
    (("room9", 'w'), "room11"),
    (("room9", 'd'), "room9a"),
    (("room11", 's'), "room9"),
    (("room8", 'w'), "room10"),
    (("room10", 's'), "room8"),
    (("room8", 'd'), "room13"),
    (("room13", 'a'), "room8"),
    (("room13", 's'), "room14")
    ]

type LocationMap = [(Thing, Location)]
locations :: LocationMap
locations = [
    ("myself", "room1"),
    ("sword", "room2"),
    ("enemy","room3"),
    ("apple", "room4"),
    ("enemy", "room5"),
    ("enemy", "room6"),
    ("myself", "room7"),
    ("special lock", "room8"),
    ("mushroom", "room9"),
    ("special key", "room9a"),
    ("flash light", "room10"),
    ("shield", "room11"),
    ("myself", "room12"),
    ("boss", "room13"),
    ("endgame", "room14"),
    ("room1", "alive")]

type World = (PathMap, LocationMap, Response)
world :: IO (PathMap, LocationMap, Response)
world = return (paths, locations, "")

main :: IO (String)
main = do
  putStr "Hello welcome to this text based game."
  putStrLn instructions
  game ( return (paths, locations, ""))
  return "Goodbye!"

game :: IO(World) -> IO(World)
game world = do
  (paths, locations, response) <- world
  putStrLn response
  putStrLn ""
  do
    putStr "command >>> "
    command <- getChar
    if command == 'q'
      then return (paths, locations, "Quitting")
      else game ( return (do_command command paths locations))

move :: Location -> Direction -> PathMap -> Location
move from direction paths = get(from, direction) paths

do_command :: Char -> PathMap -> LocationMap -> World
do_command 'w' paths locations = go 'w' paths locations
do_command 's' paths locations = go 's' paths locations
do_command 'a' paths locations = go 'a' paths locations
do_command 'd' paths locations = go 'd' paths locations

go :: Char -> PathMap -> LocationMap -> World
go direction paths locations = do
            let my_location = get "myself" locations
            let new_location = move my_location direction paths
            let new_locations = put "myself" new_location new_locations
            let response = describe new_location new_locations
            (paths, new_locations, response)

get :: Eq a => a -> [(a, String)] -> String
get value list = case lookup value list of
                    Just result -> result
                    Nothing -> "Not found."

put :: Eq t => t -> t1 -> [(t, t1)] -> [(t, t1)]
put key value list =
    let without = filter (\(x, y) -> x /= key) list
    in (key, value) : without

describe :: Location -> LocationMap -> String
describe new_location locations =
    let here = get "myself" locations
        room1_status = get "room1" locations
        ruby_location = get "ruby" locations
    in describe_helper here room1_status ruby_location  locations

describe_helper :: Location -> String -> String -> LocationMap -> String
describe_helper "room3" "dead" "holding" locations = description "room32"
describe_helper "room2" "alive" "holding" locations = description "room23"
describe_helper "room2" "dead" _ locations = description "room22"
describe_helper "room1" "dead" _ locations = description "room12"
describe_helper here _ _ locations = description here

instructions =
  "Enter commands using one or two words.\n" ++
  "Available commands are:\n" ++
  "main               -- to start the game.\n" ++
  "W, A, S, D         -- to go in that direction.\n" ++
  "quit               -- to end the game and quit."

description :: Location -> String

description "room14" = "Congratulations!!  You have recovered the ruby and won the game."

description "room2" =
    "There is a giant room1 here!  One hairy leg, about the\n" ++
    "size of a telephone pole, is directly in front of you!\n" ++
    "I would advise you to leave promptly and quietly...."

description "room22" =
    "Yecch!  There is a giant room1 here, twitching."

description "room23" =
     "The room1 sees you with the ruby and attacks!!!\n" ++
     "    ...it is over in seconds...."

description "room1" =
    "You are on top of a giant room1, standing in a rough\n" ++
    "mat of coarse hair.  The smell is awful."

description someplace = someplace ++ ", and you can't see anything."
