import Data.List
import Data.Char

type Location = String
type Direction = String
type Thing = String
type Response = String

type PathMap = [((Location, Direction), Location)]
paths:: PathMap
--this is the path.
--from pressing a which direction you want to go and where it takes you
paths = [
    --rooms ways in the patter w a s d

    (("room1", "w"), "room2"),
    --room 2 ways
    (("room2", "w"), "room3"),
    --(("room2", "a"), "room2"),
    (("room2", "s"), "room1"),
    --room 3 ways
    (("room3", "a"), "room4"),
    (("room3", "s"), "room2"),
    (("room3", "d"), "room5"),
    --room 4 ways
    (("room4", "w"), "room6"),
    (("room4", "d"), "room3"),
    --room 5 ways
    (("room5", "w"), "room7"),
    (("room5", "a"), "room3"),
    --room 6 ways
    (("room6", "w"), "room8"),
    (("room6", "s"), "room4"),
    --room 7 ways
    (("room7", "w"), "room9"),
    (("room7", "s"), "room5"),
    --room 8 ways
    (("room8", "w"), "room10"),
    (("room8", "s"), "room6"),
    (("room8", "d"), "room13"),
    --room 9 ways
    (("room9", "w"), "room12"),
    (("room9", "s"), "room7"),
    (("room9", "d"), "room9a"),
    -- hidden room 9a ways
    (("room9a", "a"), "room9"),
    --room 10 ways
    (("room10", "s"), "room8"),
    (("room10", "d"), "room11"),
    --room 11 ways
    (("room11", "a"), "room10"),
    (("room11", "d"), "room12"),
    --room 12 ways
    (("room12", "a"), "room11"),
    (("room12", "s"), "room9"),
    --room 13 ways
    (("room13", "a"), "room8"),
    (("room13", "s"), "room14")
    ]

type LocationMap = [(Thing, Location)]
locations :: LocationMap
--this is showing what Thing is in what location
locations = [
    ("myself", "room1"),
    ("enemy", "room5"),
    ("sword", "room6"),
    ("special key", "room9a"),
    ("shield", "room11"),
    ("boss", "room13"),
    --this is a hack
    ("room1", "alive")]

--this is what world contains ( pathmap, LocationMap, Response)
type World = (PathMap, LocationMap, Response)
world :: IO (PathMap, LocationMap, Response)
world = return (paths, locations, "")

--when program is first run this is where it starts
main :: IO (String)
main = do
  putStr "Hello welcome to this text based game."
  putStr instructions
  putStr mapImage
  putStr room_One
  game ( return (paths, locations, ""))
  return "Goodbye!"

--this is the loop the game runs on
game :: IO (World) -> IO (World)
game world = do
  (paths, locations, response) <- world
  putStrLn response
  putStrLn ""
  if response == win
    then return (paths, locations, "Quitting")
  else if response == lost
    then return (paths, locations, "Quitting")
  else do
    putStr "command >>> "
    command <- getLine
    if command == "quit"
      then return (paths, locations, "Quitting")
      else game ( return (do_command command paths locations))#

--these conditions need to be ment so that you can move in to the room you want to enter
can_move :: Location -> Direction -> PathMap -> LocationMap -> Bool
can_move "room9" "d" _ locations = get "key" locations == "holding"
can_move "room8" "d" _ locations = get "special key" locations == "holding"
can_move "room13" "s" _ locations = get "boss weapon" locations == "holding"
can_move from direction paths _ =
    elem (from, direction) keys
    where (keys, _) = unzip paths

cannot_move :: Location -> Direction -> Response
cannot_move "room8" "d" = "you see a kind of lock with a indent in the middle of it"
cannot_move "room9" "d" = "You see a door but you need a key to enter."
cannot_move "room13" "s" = "You have to defeat the bose before you can progress"
cannot_move _ _ = "you can not go this way, there is a wall in your way."

move :: Location -> Direction -> PathMap -> Location
move from direction paths = get (from, direction) paths

do_command :: String -> PathMap -> LocationMap -> World
do_command "w" paths locations = go "w" paths locations
do_command "s" paths locations = go "s" paths locations
do_command "a" paths locations = go "a" paths locations
do_command "d" paths locations = go "d" paths locations
do_command "pick" paths locations = game_take paths locations
do_command "kill" paths locations = kill paths locations
do_command _ paths locations = (paths, locations, "Invalid Input!")

game_take :: PathMap -> LocationMap -> World
game_take path locations = do
  let here = get "myself" locations
  let sword_location = get "sword" locations
  let shield_location = get "shield" locations
  let key_location = get "key" locations
  let special_key_location = get "special key" locations
  let boss_weapon = get "boss weapon" locations
  if here == sword_location then do
    let new_locations = put "sword" "holding" locations
    (path, new_locations, "You Take the sword!")
  else if here == shield_location then do
    let new_locations = put "shield" "holding" locations
    (path, new_locations, "You Take the shield!")
  else if here == key_location then do
    let new_locations = put "key" "holding" locations
    (path, new_locations, "You Take the key!")
  else if here == special_key_location then do
    let new_locations = put "special key" "holding" locations
    (path, new_locations, "You Take the special key!")
  else if here == boss_weapon then do
    let new_locations = put "boss weapon" "holding" locations
    (path, new_locations, "You have collected the boses weapon!")
  else
    (paths, locations, "Nothing to take!")

kill :: PathMap -> LocationMap -> World
kill paths locations =
    case get "myself" locations of
        "room5" ->
            if get "sword" locations == "holding"
                then do
                  let updated_locations = put "key" "room5" locations
                  (paths,
                      put "room5" "alive" updated_locations,
                      "You draw the bow, line up and take the shot, the arrow goes\n" ++
                      "through the zombies head, blood gushes everywhere, the zombie is dead\n" ++
                      "now the enemy is dead see if you can find the key")
                else (paths, locations,
                    "You have no weapon to fight with, see if you can find one first\n" ++
                    "")
        "room13" ->
            if get "sword" locations == "holding" && get "shield" locations == "holding"
                then do
                  let updated_locations = put "boss weapon" "room13" locations
                  (paths,
                      put "room13" "alive" updated_locations,
                      "You draw the bow, line up and take the shot, the arrow goes\n" ++
                      "through the zombies head, blood gushes everywhere, the zombie is dead\n" ++
                      "now the enemy is dead see if you can find the key\n"++
                       "you may procced to the last room")
                else (paths, locations,
                      "You have no weapon to fight with, see if you can find one first\n" ++
                      "")
        _ -> (paths, locations, "There is nothing to kill here")

instructions =
  "Enter commands using one word or letter.\n"++
  "Available commands are:\n"++
  "main               -- to start the game.\n"++
  "w, a, s, d         -- to go in that direction.\n"++
  "pick               -- to pick up item.\n"++
  "map                -- to see the map and what room you are in\n"++
  "quit               -- to end the game before you win.\n"++
  "\n"++
  "\n"++
  "\n"++
  "\n"++
  ""

mapImage =
  "\n"++
  "10  11   12\n"++
  "   ______  \n"++
  "8  ! 13 | 9    9a\n"++
  "           \n"++
  "6  | 14 | 7\n"++
  "   ______  \n"++
  "4    3    5\n"++
  "           \n"++
  "     2     \n"++
  "           \n"++
  "     1     \n"++
  "\n"++
  "\n"

room_One =
  "You are in the main entrance of a tunnel\n" ++
  "from here you can go forward into a slightly lit up area kind of like a corridor\n"

go :: String -> PathMap -> LocationMap -> World
go direction paths locations = do
    let my_location = get "myself" locations
    if can_move my_location direction paths locations
        then do
            let new_location = move my_location direction paths
            let new_locations = put "myself" new_location locations
            let response = describe new_location new_locations
            (paths, new_locations, response)
        else (paths, locations, cannot_move my_location direction)

items_here :: LocationMap -> Response
items_here locations =
    let here = get "myself" locations
        things = ["There is " ++ thing ++ " here." |
                  (thing, place) <- locations, place == here, thing /= "myself"]
    in intercalate "\n" things


-- gets the weapon from weapon map based on position
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
        normal_key_location = get "key" locations
        special_Key_location = get "special key" locations
        special_lock_location = get "special lock" locations
        boss_weapon_status = get "boss weapon" locations
    in describe_helper here room1_status special_Key_location special_lock_location boss_weapon_status locations

describe_helper :: Location -> String -> String -> String -> String -> LocationMap -> String
--describe_helper "room5" "alive" "holding" "sword" _ locations = description win
describe_helper "room5" "dead" _ _ _ locations = description "lost"
describe_helper "room9" "alive" "holding" _ _ locations = description "room9"
describe_helper "room13" "dead" _ _ _ locations = description "lost"
describe_helper "room14" _ _ _ "holding" locations = description "win"
describe_helper here _ _ _ _ locations = description here

description :: Location -> String

win = "You have beaten the boss and won the game, welldone"
lost = "you wasnt prepared for the journey, you lost"

description "win" = win
description "lost" = lost
description "room1" = "\n"
description "room2" =
  mapImage ++ "\n" ++
  "Now that you are in the the corridor you see that the object lighting up the are\n" ++
  "is infact a group of fire flies.\n" ++
  "You look around an see that the corridor/tunnel leads onwards to another area.\n"
description "room3" =
  "For the first time entering the cave you see two dirrections to go instead of forwards.\n" ++
  "You look around a see "
description "room4" = ""
description "room5" = "5"
description "room6" = "6 there is a sword"
description "room7" = "7"
description "room8" = "8"
description "room9" = "normal lock room"
description "room9a" = "key room"
description "room10" = "10"
description "room11" = "11"
description "room12" = "12"
description "room13" = "13"
description "room14" = "14"
description someplace = someplace ++ ", and you can't see anything."
