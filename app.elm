module Main exposing (..)

import Calculator exposing (calcBulletsToKill, calcDamage)
import Damage exposing (Class)
import Dict exposing (Dict)
import Game
    exposing
        ( Difficulty
        , NumOfPlayers
        , difficultyFromStr
        , difficultyToStr
        , numOfPlayersFromStr
        )
import Html exposing (Html, div, h1, h2, input, option, p, select, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import Player
    exposing
        ( Level
        , Perk
        , Player
        , Skill
        , getSkill1Name
        , levelFromStr
        , perkFromStr
        , perkToStr
        , skillFromStr
        , skillToStr
        )
import Weapon exposing (Class)
import Zed exposing (Class, getHealth)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { difficulty : Game.Difficulty
    , numOfPlayers : Game.NumOfPlayers
    , player : Player.Player
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        Game.Normal
        1
        { perk = Player.SharpShooter
        , level = 1
        , skill1 = Player.NotApplied
        , skill2 = Player.NotApplied
        , skill3 = Player.NotApplied
        , skill4 = Player.NotApplied
        , skill5 = Player.NotApplied
        }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeDifficulty String
    | ChangeNumOfPlayers String
    | ChangePerk String
    | ChangeLevel String
    | ChangeSkill1 String
    | ChangeSkill2 String
    | ChangeSkill3 String
    | ChangeSkill4 String
    | ChangeSkill5 String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeDifficulty difficulty ->
            let
                fixed =
                    Maybe.withDefault
                        Game.Normal
                        (Game.difficultyFromStr difficulty)
            in
            ( { model | difficulty = fixed }, Cmd.none )

        ChangeNumOfPlayers numOfPlayers ->
            case Game.numOfPlayersFromStr numOfPlayers of
                Just fixed ->
                    ( { model | numOfPlayers = fixed }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ChangePerk perk ->
            let
                fixed =
                    Maybe.withDefault
                        Player.SharpShooter
                        (Player.perkFromStr perk)
            in
            ( { model
                | player =
                    { perk = fixed
                    , level = model.player.level
                    , skill1 = Player.NotApplied
                    , skill2 = Player.NotApplied
                    , skill3 = Player.NotApplied
                    , skill4 = Player.NotApplied
                    , skill5 = Player.NotApplied
                    }
              }
            , Cmd.none
            )

        ChangeLevel level ->
            case Player.levelFromStr level of
                Just fixed ->
                    ( { model
                        | player =
                            { perk = model.player.perk
                            , level = fixed
                            , skill1 = model.player.skill1
                            , skill2 = model.player.skill2
                            , skill3 = model.player.skill3
                            , skill4 = model.player.skill4
                            , skill5 = model.player.skill5
                            }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ChangeSkill1 skill ->
            let
                fixed =
                    Player.skillFromStr skill
            in
            ( { model
                | player =
                    { perk = model.player.perk
                    , level = model.player.level
                    , skill1 = fixed
                    , skill2 = model.player.skill2
                    , skill3 = model.player.skill3
                    , skill4 = model.player.skill4
                    , skill5 = model.player.skill5
                    }
              }
            , Cmd.none
            )

        ChangeSkill2 skill ->
            let
                fixed =
                    Player.skillFromStr skill
            in
            ( { model
                | player =
                    { perk = model.player.perk
                    , level = model.player.level
                    , skill1 = model.player.skill1
                    , skill2 = fixed
                    , skill3 = model.player.skill3
                    , skill4 = model.player.skill4
                    , skill5 = model.player.skill5
                    }
              }
            , Cmd.none
            )

        ChangeSkill3 skill ->
            let
                fixed =
                    Player.skillFromStr skill
            in
            ( { model
                | player =
                    { perk = model.player.perk
                    , level = model.player.level
                    , skill1 = model.player.skill1
                    , skill2 = model.player.skill2
                    , skill3 = fixed
                    , skill4 = model.player.skill4
                    , skill5 = model.player.skill5
                    }
              }
            , Cmd.none
            )

        ChangeSkill4 skill ->
            let
                fixed =
                    Player.skillFromStr skill
            in
            ( { model
                | player =
                    { perk = model.player.perk
                    , level = model.player.level
                    , skill1 = model.player.skill1
                    , skill2 = model.player.skill2
                    , skill3 = model.player.skill3
                    , skill4 = fixed
                    , skill5 = model.player.skill5
                    }
              }
            , Cmd.none
            )

        ChangeSkill5 skill ->
            let
                fixed =
                    Player.skillFromStr skill
            in
            ( { model
                | player =
                    { perk = model.player.perk
                    , level = model.player.level
                    , skill1 = model.player.skill1
                    , skill2 = model.player.skill2
                    , skill3 = model.player.skill3
                    , skill4 = model.player.skill4
                    , skill5 = fixed
                    }
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        damageScarClotHead =
            Calculator.calcDamage model.player Weapon.Scar Zed.Clot Zed.Head

        damageScarClotBody =
            Calculator.calcDamage model.player Weapon.Scar Zed.Clot Zed.Body

        damageM14ClotHead =
            Calculator.calcDamage model.player Weapon.M14 Zed.Clot Zed.Head

        damageM14ClotBody =
            Calculator.calcDamage model.player Weapon.M14 Zed.Clot Zed.Body

        damageRailgunClotHead =
            Calculator.calcDamage model.player Weapon.Railgun Zed.Clot Zed.Head

        damageRailgunClotBody =
            Calculator.calcDamage model.player Weapon.Railgun Zed.Clot Zed.Body

        damageScarScrakeHead =
            Calculator.calcDamage model.player Weapon.Scar Zed.Scrake Zed.Head

        damageScarScrakeBody =
            Calculator.calcDamage model.player Weapon.Scar Zed.Scrake Zed.Body

        damageM14ScrakeHead =
            Calculator.calcDamage model.player Weapon.M14 Zed.Scrake Zed.Head

        damageM14ScrakeBody =
            Calculator.calcDamage model.player Weapon.M14 Zed.Scrake Zed.Body

        damageRailgunScrakeHead =
            Calculator.calcDamage model.player Weapon.Railgun Zed.Scrake Zed.Head

        damageRailgunScrakeBody =
            Calculator.calcDamage model.player Weapon.Railgun Zed.Scrake Zed.Body

        healthClotHead =
            Zed.getHealth Zed.Clot model.difficulty model.numOfPlayers Zed.Head

        healthClotBody =
            Zed.getHealth Zed.Clot model.difficulty model.numOfPlayers Zed.Body

        healthScrakeHead =
            Zed.getHealth Zed.Scrake model.difficulty model.numOfPlayers Zed.Head

        healthScrakeBody =
            Zed.getHealth Zed.Scrake model.difficulty model.numOfPlayers Zed.Body
    in
    div []
        [ h1 [] [ text "calc" ]
        , h2 [] [ text "Clot" ]
        , p []
            [ text <|
                toString healthClotHead
                    ++ " / "
                    ++ toString damageScarClotHead
                    ++ " [ to kill by Scar: "
                    ++ (Calculator.calcBulletsToKill damageScarClotHead healthClotHead
                            |> toString
                       )
                    ++ " ]"
            ]
        , p []
            [ text <|
                toString healthClotBody
                    ++ " / "
                    ++ toString damageScarClotBody
                    ++ " [ to kill by Scar: "
                    ++ (Calculator.calcBulletsToKill damageScarClotBody healthClotBody
                            |> toString
                       )
                    ++ " ]"
            ]
        , p []
            [ text <|
                toString healthClotHead
                    ++ " / "
                    ++ toString damageM14ClotHead
                    ++ " [ to kill by M14: "
                    ++ (Calculator.calcBulletsToKill damageM14ClotHead healthClotHead
                            |> toString
                       )
                    ++ " ]"
            ]
        , p []
            [ text <|
                toString healthClotBody
                    ++ " / "
                    ++ toString damageM14ClotBody
                    ++ " [ to kill by M14: "
                    ++ (Calculator.calcBulletsToKill damageM14ClotBody healthClotBody
                            |> toString
                       )
                    ++ " ]"
            ]
        , p []
            [ text <|
                toString healthClotHead
                    ++ " / "
                    ++ toString damageRailgunClotHead
                    ++ " [ to kill by Railgun: "
                    ++ (Calculator.calcBulletsToKill damageRailgunClotHead healthClotHead
                            |> toString
                       )
                    ++ " ]"
            ]
        , p []
            [ text <|
                toString healthClotBody
                    ++ " / "
                    ++ toString damageRailgunClotBody
                    ++ " [ to kill by Railgun: "
                    ++ (Calculator.calcBulletsToKill damageRailgunClotBody healthClotBody
                            |> toString
                       )
                    ++ " ]"
            ]
        , h2 [] [ text "Scrake " ]
        , p []
            [ text <|
                toString healthScrakeHead
                    ++ " / "
                    ++ toString damageScarScrakeHead
                    ++ " [ to kill by Scar: "
                    ++ (Calculator.calcBulletsToKill damageScarScrakeHead healthScrakeHead
                            |> toString
                       )
                    ++ " ]"
            ]
        , p []
            [ text <|
                toString healthScrakeBody
                    ++ " / "
                    ++ toString damageScarScrakeBody
                    ++ " [ to kill by Scar: "
                    ++ (Calculator.calcBulletsToKill damageScarScrakeBody healthScrakeBody
                            |> toString
                       )
                    ++ " ]"
            ]
        , p []
            [ text <|
                toString healthScrakeHead
                    ++ " / "
                    ++ toString damageM14ScrakeHead
                    ++ " [ to kill by M14: "
                    ++ (Calculator.calcBulletsToKill damageM14ScrakeHead healthScrakeHead
                            |> toString
                       )
                    ++ " ]"
            ]
        , p []
            [ text <|
                toString healthScrakeBody
                    ++ " / "
                    ++ toString damageM14ScrakeBody
                    ++ " [ to kill by M14: "
                    ++ (Calculator.calcBulletsToKill damageM14ScrakeBody healthScrakeBody
                            |> toString
                       )
                    ++ " ]"
            ]
        , p []
            [ text <|
                toString healthScrakeHead
                    ++ " / "
                    ++ toString damageRailgunScrakeHead
                    ++ " [ to kill by Railgun: "
                    ++ (Calculator.calcBulletsToKill damageRailgunScrakeHead healthScrakeHead
                            |> toString
                       )
                    ++ " ]"
            ]
        , p []
            [ text <|
                toString healthScrakeBody
                    ++ " / "
                    ++ toString damageRailgunScrakeBody
                    ++ " [ to kill by Railgun: "
                    ++ (Calculator.calcBulletsToKill damageRailgunScrakeBody healthScrakeBody
                            |> toString
                       )
                    ++ " ]"
            ]
        , select
            [ value <| Game.difficultyToStr model.difficulty
            , onInput ChangeDifficulty
            ]
            [ option [ value "normal" ] [ text "Normal" ]
            , option [ value "hard" ] [ text "Hard" ]
            , option [ value "suicidal" ] [ text "Suicidal" ]
            , option [ value "hellonearth" ] [ text "Hell on Earth" ]
            ]
        , input
            [ placeholder "Number of Players"
            , onInput ChangeNumOfPlayers
            ]
            []
        , select
            [ value <| Player.perkToStr model.player.perk
            , onInput ChangePerk
            ]
            [ option [ value "sharpshooter" ] [ text "SharpShooter" ]
            , option [ value "commando" ] [ text "Commando" ]
            ]
        , input
            [ placeholder "Level"
            , onInput ChangeLevel
            ]
            []
        , select
            [ value <| Player.skillToStr model.player.skill1
            , onInput ChangeSkill1
            ]
            [ option [ value "not_applied" ] [ text "Not Applied" ]
            , option [ value "left" ] [ text (getSkill1Name model.player.perk Player.Left) ]
            , option [ value "right" ] [ text (getSkill1Name model.player.perk Player.Right) ]
            ]
        , select
            [ value <| Player.skillToStr model.player.skill2
            , onInput ChangeSkill2
            ]
            [ option [ value "not_applied" ] [ text "Not Applied" ]
            , option [ value "left" ] [ text "Left" ]
            , option [ value "right" ] [ text "Right" ]
            ]
        , select
            [ value <| Player.skillToStr model.player.skill3
            , onInput ChangeSkill3
            ]
            [ option [ value "not_applied" ] [ text "Not Applied" ]
            , option [ value "left" ] [ text "Left" ]
            , option [ value "right" ] [ text "Right" ]
            ]
        , select
            [ value <| Player.skillToStr model.player.skill4
            , onInput ChangeSkill4
            ]
            [ option [ value "not_applied" ] [ text "Not Applied" ]
            , option [ value "left" ] [ text "Left" ]
            , option [ value "right" ] [ text "Right" ]
            ]
        , select
            [ value <| Player.skillToStr model.player.skill5
            , onInput ChangeSkill5
            ]
            [ option [ value "not_applied" ] [ text "Not Applied" ]
            , option [ value "left" ] [ text "Left" ]
            , option [ value "right" ] [ text "Right" ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
