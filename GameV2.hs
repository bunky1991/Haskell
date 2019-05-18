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
    --from a room which buttont to press to get to the next

    (("room1", "w"), "room2"),
    --room 2 ways
    (("room2", "w"), "room3"),
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
    (("room13", "s"), "room14")]

type LocationMap = [(Thing, Location)]
locations :: LocationMap
--this is showing what Thing is in what location
locations = [
    ("myself", "room1"),
    ("enemy", "room5"),
    ("key", "room5"),
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
    --if you get the response of win
    then return (paths, locations, "Quitting")
  else if response == lost
    --if you get the response of lost
    then return (paths, locations, "Quitting")
  else do
    --this is self explanatory of whats happeneing
    putStr "command >>> "
    command <- getLine
    if command == "quit"
      then return (paths, locations, "Quitting")
      else game ( return (do_command command paths locations))

--these conditions need to be ment so that you can move in to the room you want to enter
can_move :: Location -> Direction -> PathMap -> LocationMap -> Bool
can_move "room9" "d" _ locations = get "key" locations == "holding"
can_move "room8" "d" _ locations = get "special key" locations == "holding"
can_move "room13" "s" _ locations = get "boss weapon" locations == "holding"
can_move from direction paths _ =
    elem (from, direction) keys
    where (keys, _) = unzip paths

--these are basicaly the default conditions, so if the can_move are not then these are executed
cannot_move :: Location -> Direction -> Response
cannot_move "room8" "d" = "you see a kind of lock with a indent in the middle of it"
cannot_move "room9" "d" = "You see a door but you need a key to enter."
cannot_move "room13" "s" = "You have to defeat the bose before you can progress"
cannot_move _ _ = "you can not go this way, there is a wall in your way."

--this is so when you press a button like a, s, d, w it knwos what its ment to do with the direction
move :: Location -> Direction -> PathMap -> Location
move from direction paths = get (from, direction) paths

--the do command is so for the cammand during the game is playing
do_command :: String -> PathMap -> LocationMap -> World
do_command "w" paths locations = go "w" paths locations
do_command "s" paths locations = go "s" paths locations
do_command "a" paths locations = go "a" paths locations
do_command "d" paths locations = go "d" paths locations
do_command "pick" paths locations = game_take paths locations
do_command "kill" paths locations = kill paths locations
do_command _ paths locations = (paths, locations, "Invalid Input!")

--this is for the pick item up.
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

--this function is so certain conditions have to be met in order for you to kill a enemy
kill :: PathMap -> LocationMap -> World
kill paths locations =
    case get "myself" locations of
      --this for th efirst enemy you encounter
        "room5" ->
            if get "sword" locations == "holding"
                then do
                  --this function is so when you kill the enemy it will be replced with a key on the floor
                  let updated_locations = put "key" "room5" locations
                  (paths,
                      put "room5" "alive" updated_locations,
                      --when you type kill while in room5 and with a sword this is what will be displayed
                      "You walk towards the cave lizard, but as you get closer it turns invisible.\n"++
                      "As you spin on your heels you hear a noise, like a pebble falling.\n"++
                      "You look up in the area where the pebble fell and you see something that startled you.\n"++
                      "What you see is two white eyes, that seemed to move out of sync with each other, one looking forwards and the other looking back\n"++
                      "Then you see lots of teeth like something is smiling at you.\n"++
                      "It's the cave lizard and it's about to pounce on you, just as it leaps towards you\n"++
                      "you throw your self backwards with your sword pointing towards the cave lizard and you shut your eyes.\n"++
                      "As you open your eyes you see that your clumsy plan worked, you used the cave lizards strength against itself by allowing it to jump at you.\n"++
                      "now you can get the key that was in the room\n")

                else (paths, locations,
                     "You have no weapon to fight with, see if you can find one first\n" ++
                     "")
        --this is for when you fight the boss
        "room13" ->
            if get "sword" locations == "holding" && get "shield" locations == "holding"
                then do
                  --same as the previous enemy obce you kill it, it will be replaced with a key on the floor
                  let updated_locations = put "boss weapon" "room13" locations
                  (paths,
                      --this is the description that will happen when you fight the boss
                      put "room13" "alive" updated_locations,
                      "As you move towards the bear you expect it to charge at you but it doesn't.\n"++
                      "You realise that the bear is old and covered with scares from all the adventures trying to get in to the room it guards\n"++
                      "As you move a bit closer the bear looks at you and bows its head in a kind of welcome.\n"++
                      "At the this point you are stumped to see that the bear is chained to the wall.\n"++
                      "as you get closer you put your hand out to the bear with your palm open and the bear places hime nose in your hand.\n"++
                      "a minuet goes by and you hear foot steps, a man walks in with a wip and starts to wip the bear to make it angery to fight you.\n"++
                      "but as th eman wips the bear you jump up to block the wip with your shield.\n"++
                      "just as quick as it went the wip cam back and you sliced at the wip with your sword.\n"++
                      "you manage to get back to your feet and charge at the man and as you do the bear charges with you at the man.\n"++
                      "the bear pounces on the man and as you jumped you came down slicing off the mans head.\n"++
                      "The bear looks at you and you realise that the cage was broke during the fighting\n"++
                      "The giant bear sees the chaneg is broken and walks out cave never to be seen again\n"
                      )
                else (paths, locations,

                      "You have no weapon to fight with, see if you can find one first\n" ++
                      "")
        _ -> (paths, locations, "There is nothing to kill here")


--instructions for the user to say what words and buttons you can use
instructions =
  "Enter commands using one word or letter.\n"++
  "Available commands are:\n"++
  "w, a, s, d         -- to go in that direction.\n"++
  "pick               -- to pick up item.\n"++
  "kill               -- to kill a enemy thats in front of you\n"++
  "quit               -- to end the game before you win.\n"++
  "\n"++
  "\n"++
  ""

--map for the game
mapImage =
  "\n"++
  "10  11   12\n"++
  "   ______  \n"++
  "8  ! 13 | 9 ! 9a\n"++
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

--ths function is use in the do_command, its how you can move your self from one room to another in the direction you want
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

-- gets the weapon from weapon map based on position
get :: Eq a => a -> [(a, String)] -> String
get value list = case lookup value list of
                    Just result -> result
                    Nothing -> "Not found."

--this function is used in the game_take function so you can take a item from that room
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

--these are all the descriptions fro wach room
description :: Location -> String

win = "now you have entered the room your eyes start to go white and a you felt light and you have a sensation that what you \n"++
  "was here for has been furfilled"
lost = "you wasnt prepared for the journey, you lost"

description "win" = win
description "lost" = lost
description "room1" = "\n"

description "room2" =
  "Now that you are in the the corridor you see that the object lighting up the are\n" ++
  "is in-fact a group of fire flies.\n" ++
  "You look around an see that the tunnel leads on-wards to another area.\n"++
  "You can go 'w' to the next room or you can go back to previous room 's'\n"

description "room3" =
  "For the first time entering the cave you see two directions to go instead of forwards.\n" ++
  "You look around a see you can go left or right.\n"++
  "left you see a passage way that you can enter 'a'.\n"++
  "right you You hear a noise in the next room like a hiss 'd'.\n"

description "room4" = "You see in this room that a passageway leads to another room. \n"++
  "in the next room you can just make out something shinning in the distance\n"++
  "You can go forward 'w' or go back to the previous room you was in with 'd'\n"

description "room5" = "As you enter the room you see a cave lizard and you see that there is a key behind it.\n"++
  "you notice that you need to find a weapon to fight this lizard, then perhaps you can get the key. \n"

description "room6" = "When you enter this room you see that there is a sword in the corner, purhaps that could be useful\n"++
  "from this room you can go forward 'w' to the next room and you hear a kind of banging.\n"++
  "or you can run back to the previous room you was in 's'\n"

description "room7" = "you enter this next room to see that there is nothing interesting but you see there is another room further on\n"++
  "you can go to the next room 'w' or go back to the previous room 's' \n"

description "room8" = "when you enter this room you notice the banging got alot louder. once you realise where the noise is coming from you see a door.\n"++
  "on the this door you see a kind of slot like a key fits on it.\n"++
  "from here you can forward 'w' to the next room or go back to the previous room you was in 's'\n"

description "room9" = "when you enter this room you see a door on your right, when you try to open the door it wont budge.\n"++
  "perhaps you can find th ekey somewhere to get in there.\n"++
  "in the distnace you see a weird round object has fallen over\n"++
  "from here you can go forward 'w' or back to the previous room 's'\n"

description "room9a" = "now you have opened the door you see that there is a strange looking object\n"++
  "perhaps you should pick it up to see if you can use it somewhere.\n"++
  "from here you see that there is only one place to go which is back to where you came from 'a'\n"

description "room10" = "from here see you that the passage just carries on to your right 'd'\n"++
  "or you can go back to the previous 's'\n"

description "room11" = "from here you notice that the object that fell over was a shield, perhaps you could use that.\n"++
  "you can go on to the the next room to your left 'a' or you can go back to the previous room you was in 'd'\n"

description "room12" = "When you enter this room you see the whole roof is full of fireflies that start to fly around you as if they are \n"++
  "curious to see what you are.\n"++
  "you could go in to next room 'a' or 'd'"

description "room13" = "Now the doors open you notice that the banging was a massive bear thats guarding a door that has a wierd symbol in it\n"++
  "The bear looks at you and waits for you to make the first move.\n"++
  "if you defeat the bear you should be able to open the door to see where it goes.\n"++
  "before you attack you could go back and see if theres anything else to help you."

description "room14" = "now you have entered the room your eyes start to go white and a you felt light and you have a sensation that what you \n"++
  "was here for has been furfilled"

description someplace = someplace ++ ", and you can't see anything."
