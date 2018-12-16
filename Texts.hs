module Texts where

wakeUp = "You wake up in a dark room."

help = "Me helping you."

pushedShelf = "You pushed the shelf revealing a small hole leading to the a dark room\nbelow you."

rigthLockerCode = "The locker displays shows you the words \"Right Code\" while\na Long Field Communication Module falls out of it."

tRoomDoorOpen = "Transmission Room Door opened."

wonGame = "You won. Press Enter"

enteringCave = "Welcome in the cave."

foundDeadBody = "You found a dead body. On his arm is a code: \"4546B\"."

locName r
    | r ==  1 = "Level 0 - Floor"
    | r ==  2 = "Level 0 - Elevator Room"
    | r ==  3 = "Level 0 - Chiefs Room"
    | r ==  4 = "Level 0 - Machine Room"
    | r ==  5 = "Level 0 - Snake Farm"
    | r ==  6 = "Level 0 - Terminal Room"
    | r ==  7 = "Level 0 - Laboratory"
    | r ==  8 = "Level 0 - Western Panorama Room"
    | r ==  9 = "Level 0 - Preperation Room"
    | r == 10 = "Level 0 - Airlock Room"
    | r == 11 = "Level 1 - Elevator Room"
    | r == 12 = "Level 1 - Eastern Panorama Room"
    | r == 13 = "Level 1 - Northern Floor"
    | r == 14 = "Level 1 - Southern Floor"
    | r == 15 = "Level 1 - Incubation Room"
    | r == 16 = "Level 1 - Stock"
    | r == 17 = "Level 1 - Eastern Floor"
    | r == 18 = "Level 2 - Elevator Room"
    | r == 19 = "Level 2 - Transmission Room"
    | otherwise = "Error"

locText r
    | r ==  1 = "Text for: Level 0 - Floor: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r ==  2 = "Text for: Level 0 - Elevator Room: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r ==  3 = "Text for: Level 0 - Chiefs Room: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r ==  4 = "Text for: Level 0 - Machine Room: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r ==  5 = "Text for: Level 0 - Snake Farm: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam etYOU ARE DEAD PRESS ENTER"
    | r ==  6 = "Text for: Level 0 - Terminal Room: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r ==  7 = "Text for: Level 0 - Laboratory: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r ==  8 = "Text for: Level 0 - Western Panorama Room: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r ==  9 = "Text for: Level 0 - Preperation Room: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r == 10 = "Text for: Level 0 - Airlock Room: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r == 11 = "Text for: Level 1 - Elevator Room: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r == 12 = "Text for: Level 1 - Eastern Panorama Room: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r == 13 = "Text for: Level 1 - Northern Floor: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r == 14 = "Text for: Level 1 - Southern Floor: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r == 15 = "Text for: Level 1 - Incubation Room: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r == 16 = "Text for: Level 1 - Stock: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r == 17 = "Text for: Level 1 - Eastern Floor: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r == 18 = "Text for: Level 2 - Elevator Room: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | r == 19 = "Text for: Level 2 - Transmission Room: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et"
    | otherwise = "Error"

itemName i
    | i == 0 = "Chiefs ID"
    | i == 1 = "Long Field Communication Module (LFCM)"
    | i == 2 = "Universal Compression Unit (UCU)"
    | i == 3 = "Voice Message"
    | otherwise = "Error"

itemText i
    | i == 0 = "Chiefs ID TEXT"
    | i == 1 = "Long Field Communication Module (LFCM) TEXT"
    | i == 2 = "Universal Compression Unit (UCU) TEXT"
    | i == 3 = "\"HELLO HO\""
    | otherwise = "Error"
