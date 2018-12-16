module Intro (playIntro) where
import Control.Concurrent
import System.Timeout

-- ### Main Introsequence ###

playIntro = do
    clearScreen
    p "[Crashing Sound]\n"
    wait 1
    p "\"[Screaming] Oh, Hey .. Hi."
    p " It's your captain speaking!"
    p " We have kind of gotten ourselves into a pretty bad position here\n where our shields have been demolished and our hull is getting\n penetrated by enemy missile~~\"\n"
    p "[Big explosion]\n"
    wait 10
    p "\"Shit .. Ahem .. It appeares that we have to perform an emergency jump\n to hyperspace."
    p " So, to all mechanical employees: Stop what you are doing and get\n that freaking engine started!\"\n"
    p "         [Enter] Follow instructions."
    getLine
    p "You jump out of your bed and run to the door."
    p "\n\"[Mechanical Voice] Ship is in emergency mode.\n All doors are closed. Say your name loud and clear for voice recognition:\"\n"
    putStr "> "
    name <- getName
    p $ "\n\"[Mechanical Voice] Access allowed, specialist " ++ name ++ ".\"\n"
    wait 4
    p "You run straight through the hallway where the horizontal elevator takes you\nright to the other side of the ship."
    p "Arriving at Elevator Room 0 you quickly run to the Machine Room to meet up with the other mechanical employees.\n"
    wait 12
    p "\"Oh, see who finally decided to show up.\"\n"
    wait 3
    p $ "\"Stop that, lieutenant Quill, we have an engine to fix!\n Specialist " ++ name ++ ". There is a problem with the reactor but we~~\"\n"
    wait 7
    p "\"I found it, Chief! Both the Universal Compressiun Unit and\n Long Field Communication Module are broken. It is easy to fix but I\n need those parts!\"\n"
    wait 5
    p "         [1] \"I'll get the Universal Compression Unit.\""
    p "         [2] \"I'll get the Long Field Communication Module.\""
    putStr "> "
    number <- timeout 15000000 getLine
    p ""
    if number == Just "2"
        then do
            p "\"Okey, the LFC Module is in the third locker in the Stock Room, code 4761."
            p " Quill, you will go the Preperation Room. There should be an UC Unit."
        else do
            if number == Just "1"
                then do
                    p "\"Okey, there should be an UC Unit in the Preperation Room."
                else do
                    p $ "\n\"" ++ name ++ ", go to the Preperation Room and get us an UC Unit."
            p " Quill, there is a LFC Module in the third locker in the Stock Room, code 4761."
    p " Hurry up!\"\n"
    wait 10
    p "Together with Quill you both run out of the Machine Room to get the\nrepair parts."
    p "Right after you two split up the ship starts to shake and you fall to the ground\nwhile hearing Quill shrieking from another room."
    p "You decide to finish your task first before looking after him.\n"
    wait 15
    p "But before you got up, you hear the Machine Room door closing behind you."
    p "Immedietly you turn around just to see a brigth red light and the scarred\nfaces of you colleagues disappearing behind the door.\n"
    wait 12
    p "Suddenly you get pushed to a wall by the rapidly accelerating\nship as if it where to jump into hyperspace."
    p "The ship starts to shake again. A small pipe crashes from the ceiling and is\ncomming directly at your face."
    p "And then everything is black." 
    p "..."
    p "         [Enter]"
    getLine
    return name

-- ### Help Functions ###

clearScreen = putStr "\ESC[2J"
p text = putStrLn text -- Kuerzere Schreibweise
wait seconds = threadDelay $ seconds * 1000000

getName = do
    name <- getLine
    if not $ null name
        then return name
        else do
            putStr $ "\n\"[Mechanical Voice] Could not identify. Repeat.\"\n\n> "
            getName
