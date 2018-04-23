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
import Html exposing (Html, br, div, h1, h2, input, option, p, select, table, td, text, th, tr)
import Html.Attributes exposing (colspan, placeholder, value)
import Html.Events exposing (onInput)
import Player
    exposing
        ( Level
        , Perk
        , Player
        , getSkillNames
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
        , skill1 = Player.NoSkill
        , skill2 = Player.NoSkill
        , skill3 = Player.NoSkill
        , skill4 = Player.NoSkill
        , skill5 = Player.NoSkill
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
                    , skill1 = Player.NoSkill
                    , skill2 = Player.NoSkill
                    , skill3 = Player.NoSkill
                    , skill4 = Player.NoSkill
                    , skill5 = Player.NoSkill
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
        damageScarCystHead =
            Calculator.calcDamage model.player Weapon.Scar Zed.Cyst Zed.Head

        damageScarCystBody =
            Calculator.calcDamage model.player Weapon.Scar Zed.Cyst Zed.Body

        damageM14CystHead =
            Calculator.calcDamage model.player Weapon.M14 Zed.Cyst Zed.Head

        damageM14CystBody =
            Calculator.calcDamage model.player Weapon.M14 Zed.Cyst Zed.Body

        damageRailgunCystHead =
            Calculator.calcDamage model.player Weapon.Railgun Zed.Cyst Zed.Head

        damageRailgunCystBody =
            Calculator.calcDamage model.player Weapon.Railgun Zed.Cyst Zed.Body

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

        healthCystHead =
            Zed.getHealth Zed.Cyst model.difficulty model.numOfPlayers Zed.Head

        healthCystBody =
            Zed.getHealth Zed.Cyst model.difficulty model.numOfPlayers Zed.Body

        healthScrakeHead =
            Zed.getHealth Zed.Scrake model.difficulty model.numOfPlayers Zed.Head

        healthScrakeBody =
            Zed.getHealth Zed.Scrake model.difficulty model.numOfPlayers Zed.Body

        ( skill1, skill2, skill3, skill4, skill5 ) =
            getSkillNames model.player.perk

        skillOptions1 =
            generateSkillOptions skill1

        skillOptions2 =
            generateSkillOptions skill2

        skillOptions3 =
            generateSkillOptions skill3

        skillOptions4 =
            generateSkillOptions skill4

        skillOptions5 =
            generateSkillOptions skill5
    in
    div []
        [ h1 [] [ text "calc" ]
        , div []
            [ select
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
            ]
        , div []
            [ select
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
            ]
        , div []
            [ select
                [ value <| Player.skillToStr model.player.skill1
                , onInput ChangeSkill1
                ]
                skillOptions1
            , select
                [ value <| Player.skillToStr model.player.skill2
                , onInput ChangeSkill2
                ]
                skillOptions2
            , select
                [ value <| Player.skillToStr model.player.skill3
                , onInput ChangeSkill3
                ]
                skillOptions3
            , select
                [ value <| Player.skillToStr model.player.skill4
                , onInput ChangeSkill4
                ]
                skillOptions4
            , select
                [ value <| Player.skillToStr model.player.skill5
                , onInput ChangeSkill5
                ]
                skillOptions5
            ]
        , table []
            [ tr []
                [ th [] []
                , th [ colspan 2 ] [ text "Cyst" ]
                , th [ colspan 2 ] [ text "Scrake" ]
                ]
            , tr []
                [ th [] []
                , th [] [ text "Head" ]
                , th [] [ text "Body" ]
                , th [] [ text "Head" ]
                , th [] [ text "Body" ]
                ]
            , tr []
                [ th [] [ text "Scar" ]
                , td []
                    [ text <|
                        (Calculator.calcBulletsToKill damageScarCystHead healthCystHead
                            |> toString
                        )
                    , br [] []
                    , text <|
                        " ("
                            ++ toString healthCystHead
                            ++ " / "
                            ++ toString damageScarCystHead
                            ++ " )"
                    ]
                , td []
                    [ text <|
                        (Calculator.calcBulletsToKill damageScarCystBody healthCystBody
                            |> toString
                        )
                    , br [] []
                    , text <|
                        " ("
                            ++ toString healthCystBody
                            ++ " / "
                            ++ toString damageScarCystBody
                            ++ " )"
                    ]
                , td []
                    [ text <|
                        (Calculator.calcBulletsToKill damageScarScrakeHead healthScrakeHead
                            |> toString
                        )
                    , br [] []
                    , text <|
                        " ("
                            ++ toString healthScrakeHead
                            ++ " / "
                            ++ toString damageScarScrakeHead
                            ++ " )"
                    ]
                , td []
                    [ text <|
                        (Calculator.calcBulletsToKill damageScarScrakeBody healthScrakeBody
                            |> toString
                        )
                    , br [] []
                    , text <|
                        " ("
                            ++ toString healthScrakeBody
                            ++ " / "
                            ++ toString damageScarScrakeBody
                            ++ " )"
                    ]
                ]
            , tr []
                [ th [] [ text "M14" ]
                , td []
                    [ text <|
                        (Calculator.calcBulletsToKill damageM14CystHead healthCystHead
                            |> toString
                        )
                    , br [] []
                    , text <|
                        " ("
                            ++ toString healthCystHead
                            ++ " / "
                            ++ toString damageM14CystHead
                            ++ " )"
                    ]
                , td []
                    [ text <|
                        (Calculator.calcBulletsToKill damageM14CystBody healthCystBody
                            |> toString
                        )
                    , br [] []
                    , text <|
                        " ("
                            ++ toString healthCystBody
                            ++ " / "
                            ++ toString damageM14CystBody
                            ++ " )"
                    ]
                , td []
                    [ text <|
                        (Calculator.calcBulletsToKill damageM14ScrakeHead healthScrakeHead
                            |> toString
                        )
                    , br [] []
                    , text <|
                        " ("
                            ++ toString healthScrakeHead
                            ++ " / "
                            ++ toString damageM14ScrakeHead
                            ++ " )"
                    ]
                , td []
                    [ text <|
                        (Calculator.calcBulletsToKill damageM14ScrakeBody healthScrakeBody
                            |> toString
                        )
                    , br [] []
                    , text <|
                        " ("
                            ++ toString healthScrakeBody
                            ++ " / "
                            ++ toString damageM14ScrakeBody
                            ++ " )"
                    ]
                ]
            , tr []
                [ th [] [ text "Railgun" ]
                , td []
                    [ text <|
                        (Calculator.calcBulletsToKill damageRailgunCystHead healthCystHead
                            |> toString
                        )
                    , br [] []
                    , text <|
                        " ("
                            ++ toString healthCystHead
                            ++ " / "
                            ++ toString damageRailgunCystHead
                            ++ " )"
                    ]
                , td []
                    [ text <|
                        (Calculator.calcBulletsToKill damageRailgunCystBody healthCystBody
                            |> toString
                        )
                    , br [] []
                    , text <|
                        " ("
                            ++ toString healthCystBody
                            ++ " / "
                            ++ toString damageRailgunCystBody
                            ++ " )"
                    ]
                , td []
                    [ text <|
                        (Calculator.calcBulletsToKill damageRailgunScrakeHead healthScrakeHead
                            |> toString
                        )
                    , br [] []
                    , text <|
                        " ("
                            ++ toString healthScrakeHead
                            ++ " / "
                            ++ toString damageRailgunScrakeHead
                            ++ " )"
                    ]
                , td []
                    [ text <|
                        (Calculator.calcBulletsToKill damageRailgunScrakeBody healthScrakeBody
                            |> toString
                        )
                    , br [] []
                    , text <|
                        " ("
                            ++ toString healthScrakeBody
                            ++ " / "
                            ++ toString damageRailgunScrakeBody
                            ++ " )"
                    ]
                ]
            ]
        ]


generateSkillOptions : List String -> List (Html msg)
generateSkillOptions skills =
    List.map
        (\skill ->
            option
                [ value skill ]
                [ text skill ]
        )
        skills
        |> List.append [ option [ value "No Skill" ] [ text "No Skill" ] ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
