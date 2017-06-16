module Game exposing (Difficulty(..), NumOfPlayers, difficultyFromStr, difficultyToStr, numOfPlayersFromStr)


type Difficulty
    = Normal
    | Hard
    | Suicidal
    | HellOnEarth


difficultyFromStr : String -> Maybe Difficulty
difficultyFromStr string =
    case string of
        "normal" ->
            Just Normal

        "hard" ->
            Just Hard

        "suicidal" ->
            Just Suicidal

        "hellonearth" ->
            Just HellOnEarth

        _ ->
            Nothing


difficultyToStr : Difficulty -> String
difficultyToStr difficulty =
    case difficulty of
        Normal ->
            "normal"

        Hard ->
            "hard"

        Suicidal ->
            "suicidal"

        HellOnEarth ->
            "hellonearth"


type alias NumOfPlayers =
    Int


numOfPlayersFromStr : String -> Maybe NumOfPlayers
numOfPlayersFromStr numOfPlayers =
    let
        result =
            String.toInt numOfPlayers
    in
    case result of
        Ok int ->
            if int >= 1 && int <= 6 then
                Just int
            else
                Nothing

        Err msg ->
            Nothing
