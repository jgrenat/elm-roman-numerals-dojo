module Roman exposing (Roman, fromInt, fromString, toInt, toString)

import List.Extra exposing (group, stripPrefix)


type Roman
    = Roman (List RomanLetter)


type RomanLetter
    = I
    | V
    | X
    | L
    | C
    | D
    | M


fromInt : Int -> Roman
fromInt input =
    factorizeSymbol input
        |> Roman


fromString : String -> Result String Roman
fromString romanString =
    case romanString of
        "" ->
            Err "Empty string input is not allowed"

        _ ->
            String.toList romanString
                |> List.map charToSymbol
                |> List.foldr (Result.map2 (::)) (Ok [])
                |> Result.andThen
                    (\romanLetters ->
                        if hasMoreThan4IdenticalCharacters romanLetters then
                            Err "Invalid input string: more than 3 identical letters in a row"

                        else
                            Ok romanLetters
                    )
                |> Result.andThen
                    (\romanLetters ->
                        if isValidSequence romanLetters then
                            Ok romanLetters

                        else
                            Err "Invalid sequence of letters"
                    )
                |> Result.map Roman


hasMoreThan4IdenticalCharacters : List RomanLetter -> Bool
hasMoreThan4IdenticalCharacters romanLetters =
    group romanLetters
        |> List.any (\( value, occurences ) -> List.length occurences > 2 && value /= M)


isValidSequence : List RomanLetter -> Bool
isValidSequence romanLetters =
    conversionTable
        |> List.foldl
            (\( letters, _ ) accumulator ->
                let
                    step remainingLetters =
                        case stripPrefix letters remainingLetters of
                            Just newRemainingLetters ->
                                step newRemainingLetters

                            Nothing ->
                                remainingLetters
                in
                step accumulator
            )
            romanLetters
        |> List.isEmpty


toInt : Roman -> Int
toInt (Roman romanLetters) =
    conversionTable
        |> List.foldl
            (\( letters, value ) accumulator ->
                let
                    step ( remainingLetters, sum ) =
                        case stripPrefix letters remainingLetters of
                            Just newRemainingLetters ->
                                step ( newRemainingLetters, sum + value )

                            Nothing ->
                                ( remainingLetters, sum )
                in
                step accumulator
            )
            ( romanLetters, 0 )
        |> Tuple.second


conversionTable =
    [ ( [ M ], 1000 )
    , ( [ C, M ], 900 )
    , ( [ D ], 500 )
    , ( [ C, D ], 400 )
    , ( [ C ], 100 )
    , ( [ X, C ], 90 )
    , ( [ L ], 50 )
    , ( [ X, L ], 40 )
    , ( [ X ], 10 )
    , ( [ I, X ], 9 )
    , ( [ V ], 5 )
    , ( [ I, V ], 4 )
    , ( [ I ], 1 )
    ]


factorizeSymbol : Int -> List RomanLetter
factorizeSymbol input =
    conversionTable
        |> List.foldl
            (\( romanLetters, value ) ( remaining, letters ) ->
                let
                    occurrences =
                        remaining // value

                    newLetters : List RomanLetter
                    newLetters =
                        List.repeat occurrences romanLetters
                            |> List.concat

                    newRemaining =
                        remaining - occurrences * value
                in
                ( newRemaining, letters ++ newLetters )
            )
            ( input, [] )
        |> Tuple.second


symbolToChar : RomanLetter -> Char
symbolToChar romanLetter =
    case romanLetter of
        I ->
            'I'

        V ->
            'V'

        X ->
            'X'

        L ->
            'L'

        C ->
            'C'

        D ->
            'D'

        M ->
            'M'


charToSymbol : Char -> Result String RomanLetter
charToSymbol char =
    case char of
        'I' ->
            Ok I

        'V' ->
            Ok V

        'X' ->
            Ok X

        'L' ->
            Ok L

        'C' ->
            Ok C

        'D' ->
            Ok D

        'M' ->
            Ok M

        _ ->
            Err ("Invalid symbol '" ++ String.fromChar char ++ "'")


toString : Roman -> String
toString (Roman letters) =
    List.map symbolToChar letters
        |> List.map String.fromChar
        |> List.foldr (++) ""
