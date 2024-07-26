import System.Process (callCommand, system)
import Data.List (intercalate)
import Control.Monad
import Data.Char
-- Define global settings
modKey :: String
modKey = "Mod4"

terminal :: String
terminal = "qterminal"
tags = ["1", "2", "3", "4", "5", "6"]
states = ["active", "urgent", "normal"]
resizestep :: Float
resizestep  = 0.1
cleanConfig :: IO ()
cleanConfig = do
    _ <- system "herbstclient keyunbind --all"
    _ <- system "herbstclient mouseunbind --all"
    _ <- system "herbstclient unrule -F"
    return ()
createCommand :: String -> String -> String -> String
createCommand baseCommand input action = baseCommand ++ " " ++ input ++ " " ++ action

-- Function to set up tags using herbstclient
setupTags :: IO ()
setupTags = do
    -- Loop through each tag
    mapM_ (\tag -> do
        -- Construct the add command
        let addCmd = "herbstclient add " ++ tag
        -- Execute the add command
        callCommand addCmd
        -- Construct the keybind command
        let switchCmd = "herbstclient keybind " ++ modKey  ++ "-" ++ tag ++ " use_index " ++ tag
        let moveCmd = "herbstclient keybind " ++ modKey  ++ "-Shift-" ++ tag ++ " move_index " ++ tag
        putStrLn moveCmd
        putStrLn switchCmd
        -- Execute the keybind command
        callCommand switchCmd
        callCommand moveCmd
          ) tags

-- Define the bindKeys functionp
bindKeys :: String -> String -> IO ()
bindKeys input action = do
    let command = createCommand "herbstclient keybind" input action
    putStrLn $ "Executing: " ++ command
    callCommand command

-- Implement the replace helper function
alphabet = ['a'..'z'] ++ ['A'..'Z']

-- Modified replace function
replace :: String -> String -> String -> String
replace _ _ [] = []
replace target replacement (x:xs)
    | null target || x /= head target = x : replace target replacement xs
    | otherwise = 
        let nextChar = if null xs then '\0' else toLower (head xs)
            isLetterAfter = nextChar `elem` alphabet
        in if isLetterAfter then x : replace target replacement xs else replacement ++ replace target replacement xs

applyAttr :: (String, String) -> IO ()
applyAttr (attrName, attrValue) = do
    let command = "herbstclient attr " ++ attrName ++ " " ++ attrValue
    putStrLn $ "Applying attribute setting: " ++ command
    callCommand command
applySetting :: (String, String) -> IO ()
applySetting (settingName, settingValue) = do
    let command = "herbstclient set " ++ settingName ++ " " ++ settingValue
    putStrLn $ "Applying setting: " ++ command
-- Define the list of general keybindings with automatic "M" to "mod4" replacement


keys :: [(String, String)]
keys =
    map (\(key, action) -> 
        let replacedKey = replace "M" modKey (replace "C" "Control" (replace "S" "Shift" key))
        in (replacedKey, action)) 
    [ ("M-S-q", "quit")
    , ("M-S-r", "reload")
    , ("M-q", "close")
    , ("M-Return", "spawn " ++  terminal)
    , ("M-Left", "focus left")
    , ("M-Down", "focus down")
    , ("M-Up", "focus up")
    , ("M-Right", "focus right")
    , ("M-S-Left", "shift left")
    , ("M-S-Right", "shift right")
    , ("M-S-Down", "shift down")
    , ("M-S-Up", "shift up")
    , ("M-C-Left", "resize left" ++ " +" ++ show resizestep)
    , ("M-C-Right", "resize right" ++ " +" ++ show resizestep)
    , ("M-C-Up", "resize up" ++ " +" ++ show resizestep)
    , ("M-C-Down", "resize down" ++ " +" ++ show resizestep)
    , ("M-u", "split bottom 0.5")
    , ("M-o", "split right 0.5")
    , ("M-C-space", "split explode")
    , ("M-period", "use_index +1 --skip-visible")
    , ("M-comma", "use_index -1 --skip-visible")
    , ("M-r", "remove")
    , ("M-s", "floating toggle")
    , ("M-f", "fullscreen toggle")
    , ("M-p", "pseudotile toggle")
    , ("M-space", "cycle_layout")
    , ("M-BackSpace", "cycle_monitor")
    , ("M-Tab", "cycle_all +1")
    , ("M-S-Tab", "cycle_all +1")
    , ("M-c", "cycle")
    , ("M-i", "jumpto urgent")
    ]


-- Implement the mouseBind function
mouseBind :: String -> String -> IO ()
mouseBind input action = do
    let command = createCommand "herbstclient mousebind" input action
    putStrLn $ "Executing: " ++ command
    callCommand command

-- Implement the function to replace "B" with "Button"
replaceBWithButton :: String -> String
replaceBWithButton = replace "B" "Button"

-- Define the list of mouse keybindings
mouseKeys :: [(String, String)]
mouseKeys =
      map (\(key, action) -> (replace "M" modKey key, action)) 
    [ ("M-B1", "move")
    , ("M-B2", "zoom")
    , ("M-B3", "resize")
    ]

-- Automatically apply replaceBWithButton to each tuple in mouseKeys
mouseKeysModified :: [(String, String)]
mouseKeysModified = map (\(key, action) -> (replaceBWithButton key, action)) mouseKeys

attrs :: [(String, String)]
attrs = [  ("theme.title_height", "15")
        , ("theme.title_when", "'always'")
        , ("theme.title_font", "'Dejavu Sans:pixelsize=12'")
        , ("theme.title_depth", "3")
        , ("theme.active.color", "'#345F0Cef'")
        , ("theme.title_color", "'#ffffff'")
        , ("theme.normal.color", "'#323232dd'")
        , ("theme.urgent.color", "'#7811A1dd'")
        , ("theme.tab_color", "'#1F1F1Fdd'")
        , ("theme.active.tab_color", "'#2B4F0Add'")
        , ("theme.active.tab_outer_color", "'#6C8257dd'")
        , ("theme.active.tab_title_color", "'#ababab'")
        , ("theme.normal.title_color", "'#898989'")
        , ("theme.inner_width", "1")
        , ("theme.inner_color", "'black'")
        , ("theme.border_width", "3")
        , ("theme.floating.border_width", "4")
        , ("theme.floating.outer_width", "1")
        , ("theme.floating.outer_color", "'black'")
        , ("theme.active.inner_color", "'#789161'")
        , ("theme.urgent.inner_color", "'#9A65B0'")
        , ("theme.normal.inner_color", "'#606060'")
 ]
  
setStateColors :: String -> IO ()
setStateColors state = do
    let cmd = "herbstclient substitute C theme." ++ state ++ ".inner_color attr theme." ++ state ++ ".outer_color C"
    putStrLn cmd
    callCommand cmd

settings :: [(String, String)]
settings = [ ("frame_border_active_color", "'#222222cc'")
           , ("frame_border_normal_color", "'#101010cc'")
           , ("frame_bg_normal_color", "'#565656aa'")
           , ("frame_bg_active_color", "'#345F0Caa'")
           , ("frame_border_width", "1")
           , ("show_frame_decorations", "'focused_if_multiple'")
           , ("frame_bg_transparent", "on")
           , ("frame_transparent_width", "5")
           , ("frame_gap", "4")

           ]
-- Boolean rules are represented as (String, Bool) tuples
type BooleanRule = (String, Bool)
booleanRules :: [BooleanRule]
booleanRules = [("focus", True)]
-- Boolean rules are represented as (String, Bool) tuples

-- Complex rules are represented as (String, String) tuples
type ComplexRule = (String, String)
complexRules :: [ComplexRule]
complexRules = [
    ("windowtype~'_NET_WM_WINDOW_TYPE_(DIALOG|UTILITY|SPLASH)'", "floating=on")
 ,    ("windowtype='_NET_WM_WINDOW_TYPE_DIALOG'", "focus=on")
 ,   ("windowtype~'_NET_WM_WINDOW_TYPE_(NOTIFICATION|DOCK|DESKTOP)'", "manage=off")
 ,   ("fixedsize", "floating=on") -- Assuming "fixedsize" is a condition that matches certain windows
 ]           
--window rules
generateBooleanCommand :: (String, Bool) -> String
generateBooleanCommand (rule, state) = 
    let stateStr = if state then "on" else "off"
    in "herbstclient rule " ++ rule ++ "=" ++ stateStr
generateComplexCommand :: (String, String) -> String
generateComplexCommand (condition, state) = 
    "herbstclient rule " ++ condition ++ " " ++ state


applyRule :: String -> IO ()
applyRule command = do
    putStrLn $ "Applying rule: " ++ command
    callCommand command
applyBooleanRules :: [BooleanRule] -> IO ()
applyBooleanRules rules = mapM_ (\rule -> applyRule $ generateBooleanCommand rule) rules
applyComplexRules :: [ComplexRule] -> IO ()
applyComplexRules rules = mapM_ (\rule -> applyRule $ generateComplexCommand rule) rules

-- Implement the main logic
main :: IO ()
main = do
    cleanConfig
    mapM_ applyAttr attrs
    setupTags
    mapM_ (\(key, action) -> bindKeys key action) keys
    mapM_ (\(key, action) -> mouseBind key action) mouseKeysModified
    mapM_ applySetting settings        
    mapM_ (setStateColors) states
    
    applyBooleanRules booleanRules
    applyComplexRules complexRules
    _ <- system "herbstclient detect_monitors"
    return ()
