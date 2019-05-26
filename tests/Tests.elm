module Tests exposing (suite)

import Expect exposing (Expectation, equal)
import Fuzz
import Roman exposing (Roman, fromInt, fromString, toInt, toString)
import Test exposing (..)


suite : Test
suite =
    concat [ intToString, stringToInt, fuzzSample ]


fuzzSample : Test
fuzzSample =
    fuzz (Fuzz.intRange 1 999999)
        "A value converted to string then to int should be the same"
        (\value ->
            fromInt value |> toString |> fromString |> Result.map toInt |> Expect.equal (Ok value)
        )


intToString : Test
intToString =
    describe "numericToRoman"
        [ test "should return I for 1" <|
            \_ -> fromInt 1 |> toString |> equal "I"
        , test "should return V for 5" <|
            \_ -> fromInt 5 |> toString |> equal "V"
        , test "should return X for 10" <|
            \_ -> fromInt 10 |> toString |> equal "X"
        , test "should return L for 50" <|
            \_ -> fromInt 50 |> toString |> equal "L"
        , test "should return C for 100" <|
            \_ -> fromInt 100 |> toString |> equal "C"
        , test "should return D for 500" <|
            \_ -> fromInt 500 |> toString |> equal "D"
        , test "should return M for 1000" <|
            \_ -> fromInt 1000 |> toString |> equal "M"
        , test "should return II for 2" <|
            \_ -> fromInt 2 |> toString |> equal "II"
        , test "should return III for 3" <|
            \_ -> fromInt 3 |> toString |> equal "III"
        , test "should return VI for 6" <|
            \_ -> fromInt 6 |> toString |> equal "VI"
        , test "should return VII for 7" <|
            \_ -> fromInt 7 |> toString |> equal "VII"
        , test "should return IV for 4" <|
            \_ -> fromInt 4 |> toString |> equal "IV"
        , test "should return IX for 9" <|
            \_ -> fromInt 9 |> toString |> equal "IX"
        , test "should return IC for 99" <|
            \_ -> fromInt 99 |> toString |> equal "XCIX"
        ]


stringToInt : Test
stringToInt =
    describe "romanToNumeric"
        [ describe "1-10"
            [ test "I" <| \_ -> fromString "I" |> Result.map toInt |> equal (Ok 1)
            , test "II" <| \_ -> fromString "II" |> Result.map toInt |> equal (Ok 2)
            , test "III" <| \_ -> fromString "III" |> Result.map toInt |> equal (Ok 3)
            , test "IV" <| \_ -> fromString "IV" |> Result.map toInt |> equal (Ok 4)
            , test "V" <| \_ -> fromString "V" |> Result.map toInt |> equal (Ok 5)
            , test "VI" <| \_ -> fromString "VI" |> Result.map toInt |> equal (Ok 6)
            , test "VII" <| \_ -> fromString "VII" |> Result.map toInt |> equal (Ok 7)
            , test "VIII" <| \_ -> fromString "VIII" |> Result.map toInt |> equal (Ok 8)
            , test "IX" <| \_ -> fromString "IX" |> Result.map toInt |> equal (Ok 9)
            , test "X" <| \_ -> fromString "X" |> Result.map toInt |> equal (Ok 10)
            ]
        , describe "11-20"
            [ test "XI" <| \_ -> fromString "XI" |> Result.map toInt |> equal (Ok 11)
            , test "XII" <| \_ -> fromString "XII" |> Result.map toInt |> equal (Ok 12)
            , test "XIII" <| \_ -> fromString "XIII" |> Result.map toInt |> equal (Ok 13)
            , test "XIV" <| \_ -> fromString "XIV" |> Result.map toInt |> equal (Ok 14)
            , test "XV" <| \_ -> fromString "XV" |> Result.map toInt |> equal (Ok 15)
            , test "XVI" <| \_ -> fromString "XVI" |> Result.map toInt |> equal (Ok 16)
            , test "XVII" <| \_ -> fromString "XVII" |> Result.map toInt |> equal (Ok 17)
            , test "XVIII" <| \_ -> fromString "XVIII" |> Result.map toInt |> equal (Ok 18)
            , test "XIX" <| \_ -> fromString "XIX" |> Result.map toInt |> equal (Ok 19)
            , test "XX" <| \_ -> fromString "XX" |> Result.map toInt |> equal (Ok 20)
            ]
        , describe "21-30"
            [ test "XXI" <| \_ -> fromString "XXI" |> Result.map toInt |> equal (Ok 21)
            , test "XXII" <| \_ -> fromString "XXII" |> Result.map toInt |> equal (Ok 22)
            , test "XXIII" <| \_ -> fromString "XXIII" |> Result.map toInt |> equal (Ok 23)
            , test "XXIV" <| \_ -> fromString "XXIV" |> Result.map toInt |> equal (Ok 24)
            , test "XXV" <| \_ -> fromString "XXV" |> Result.map toInt |> equal (Ok 25)
            , test "XXVI" <| \_ -> fromString "XXVI" |> Result.map toInt |> equal (Ok 26)
            , test "XXVII" <| \_ -> fromString "XXVII" |> Result.map toInt |> equal (Ok 27)
            , test "XXVIII" <| \_ -> fromString "XXVIII" |> Result.map toInt |> equal (Ok 28)
            , test "XXIX" <| \_ -> fromString "XXIX" |> Result.map toInt |> equal (Ok 29)
            , test "XXX" <| \_ -> fromString "XXX" |> Result.map toInt |> equal (Ok 30)
            ]
        , describe "31-40"
            [ test "XXXI" <| \_ -> fromString "XXXI" |> Result.map toInt |> equal (Ok 31)
            , test "XXXII" <| \_ -> fromString "XXXII" |> Result.map toInt |> equal (Ok 32)
            , test "XXXIII" <| \_ -> fromString "XXXIII" |> Result.map toInt |> equal (Ok 33)
            , test "XXXIV" <| \_ -> fromString "XXXIV" |> Result.map toInt |> equal (Ok 34)
            , test "XXXV" <| \_ -> fromString "XXXV" |> Result.map toInt |> equal (Ok 35)
            , test "XXXVI" <| \_ -> fromString "XXXVI" |> Result.map toInt |> equal (Ok 36)
            , test "XXXVII" <| \_ -> fromString "XXXVII" |> Result.map toInt |> equal (Ok 37)
            , test "XXXVIII" <| \_ -> fromString "XXXVIII" |> Result.map toInt |> equal (Ok 38)
            , test "XXXIX" <| \_ -> fromString "XXXIX" |> Result.map toInt |> equal (Ok 39)
            , test "XL" <| \_ -> fromString "XL" |> Result.map toInt |> equal (Ok 40)
            ]
        , describe "41-50"
            [ test "XLI" <| \_ -> fromString "XLI" |> Result.map toInt |> equal (Ok 41)
            , test "XLII" <| \_ -> fromString "XLII" |> Result.map toInt |> equal (Ok 42)
            , test "XLIII" <| \_ -> fromString "XLIII" |> Result.map toInt |> equal (Ok 43)
            , test "XLIV" <| \_ -> fromString "XLIV" |> Result.map toInt |> equal (Ok 44)
            , test "XLV" <| \_ -> fromString "XLV" |> Result.map toInt |> equal (Ok 45)
            , test "XLVI" <| \_ -> fromString "XLVI" |> Result.map toInt |> equal (Ok 46)
            , test "XLVII" <| \_ -> fromString "XLVII" |> Result.map toInt |> equal (Ok 47)
            , test "XLVIII" <| \_ -> fromString "XLVIII" |> Result.map toInt |> equal (Ok 48)
            , test "XLIX" <| \_ -> fromString "XLIX" |> Result.map toInt |> equal (Ok 49)
            , test "L" <| \_ -> fromString "L" |> Result.map toInt |> equal (Ok 50)
            ]
        , describe "51-60"
            [ test "LI" <| \_ -> fromString "LI" |> Result.map toInt |> equal (Ok 51)
            , test "LII" <| \_ -> fromString "LII" |> Result.map toInt |> equal (Ok 52)
            , test "LIII" <| \_ -> fromString "LIII" |> Result.map toInt |> equal (Ok 53)
            , test "LIV" <| \_ -> fromString "LIV" |> Result.map toInt |> equal (Ok 54)
            , test "LV" <| \_ -> fromString "LV" |> Result.map toInt |> equal (Ok 55)
            , test "LVI" <| \_ -> fromString "LVI" |> Result.map toInt |> equal (Ok 56)
            , test "LVII" <| \_ -> fromString "LVII" |> Result.map toInt |> equal (Ok 57)
            , test "LVIII" <| \_ -> fromString "LVIII" |> Result.map toInt |> equal (Ok 58)
            , test "LIX" <| \_ -> fromString "LIX" |> Result.map toInt |> equal (Ok 59)
            , test "LX" <| \_ -> fromString "LX" |> Result.map toInt |> equal (Ok 60)
            ]
        , describe "61-70"
            [ test "LXI" <| \_ -> fromString "LXI" |> Result.map toInt |> equal (Ok 61)
            , test "LXII" <| \_ -> fromString "LXII" |> Result.map toInt |> equal (Ok 62)
            , test "LXIII" <| \_ -> fromString "LXIII" |> Result.map toInt |> equal (Ok 63)
            , test "LXIV" <| \_ -> fromString "LXIV" |> Result.map toInt |> equal (Ok 64)
            , test "LXV" <| \_ -> fromString "LXV" |> Result.map toInt |> equal (Ok 65)
            , test "LXVI" <| \_ -> fromString "LXVI" |> Result.map toInt |> equal (Ok 66)
            , test "LXVII" <| \_ -> fromString "LXVII" |> Result.map toInt |> equal (Ok 67)
            , test "LXVIII" <| \_ -> fromString "LXVIII" |> Result.map toInt |> equal (Ok 68)
            , test "LXIX" <| \_ -> fromString "LXIX" |> Result.map toInt |> equal (Ok 69)
            , test "LXX" <| \_ -> fromString "LXX" |> Result.map toInt |> equal (Ok 70)
            ]
        , describe "71-80"
            [ test "LXXI" <| \_ -> fromString "LXXI" |> Result.map toInt |> equal (Ok 71)
            , test "LXXII" <| \_ -> fromString "LXXII" |> Result.map toInt |> equal (Ok 72)
            , test "LXXIII" <| \_ -> fromString "LXXIII" |> Result.map toInt |> equal (Ok 73)
            , test "LXXIV" <| \_ -> fromString "LXXIV" |> Result.map toInt |> equal (Ok 74)
            , test "LXXV" <| \_ -> fromString "LXXV" |> Result.map toInt |> equal (Ok 75)
            , test "LXXVI" <| \_ -> fromString "LXXVI" |> Result.map toInt |> equal (Ok 76)
            , test "LXXVII" <| \_ -> fromString "LXXVII" |> Result.map toInt |> equal (Ok 77)
            , test "LXXVIII" <| \_ -> fromString "LXXVIII" |> Result.map toInt |> equal (Ok 78)
            , test "LXXIX" <| \_ -> fromString "LXXIX" |> Result.map toInt |> equal (Ok 79)
            , test "LXXX" <| \_ -> fromString "LXXX" |> Result.map toInt |> equal (Ok 80)
            ]
        , describe "81-90"
            [ test "LXXXI" <| \_ -> fromString "LXXXI" |> Result.map toInt |> equal (Ok 81)
            , test "LXXXII" <| \_ -> fromString "LXXXII" |> Result.map toInt |> equal (Ok 82)
            , test "LXXXIII" <| \_ -> fromString "LXXXIII" |> Result.map toInt |> equal (Ok 83)
            , test "LXXXIV" <| \_ -> fromString "LXXXIV" |> Result.map toInt |> equal (Ok 84)
            , test "LXXXV" <| \_ -> fromString "LXXXV" |> Result.map toInt |> equal (Ok 85)
            , test "LXXXVI" <| \_ -> fromString "LXXXVI" |> Result.map toInt |> equal (Ok 86)
            , test "LXXXVII" <| \_ -> fromString "LXXXVII" |> Result.map toInt |> equal (Ok 87)
            , test "LXXXVIII" <| \_ -> fromString "LXXXVIII" |> Result.map toInt |> equal (Ok 88)
            , test "LXXXIX" <| \_ -> fromString "LXXXIX" |> Result.map toInt |> equal (Ok 89)
            , test "XC" <| \_ -> fromString "XC" |> Result.map toInt |> equal (Ok 90)
            ]
        , describe "91-100"
            [ test "XCI" <| \_ -> fromString "XCI" |> Result.map toInt |> equal (Ok 91)
            , test "XCII" <| \_ -> fromString "XCII" |> Result.map toInt |> equal (Ok 92)
            , test "XCIII" <| \_ -> fromString "XCIII" |> Result.map toInt |> equal (Ok 93)
            , test "XCIV" <| \_ -> fromString "XCIV" |> Result.map toInt |> equal (Ok 94)
            , test "XCV" <| \_ -> fromString "XCV" |> Result.map toInt |> equal (Ok 95)
            , test "XCVI" <| \_ -> fromString "XCVI" |> Result.map toInt |> equal (Ok 96)
            , test "XCVII" <| \_ -> fromString "XCVII" |> Result.map toInt |> equal (Ok 97)
            , test "XCVIII" <| \_ -> fromString "XCVIII" |> Result.map toInt |> equal (Ok 98)
            , test "XCIX" <| \_ -> fromString "XCIX" |> Result.map toInt |> equal (Ok 99)
            , test "C" <| \_ -> fromString "C" |> Result.map toInt |> equal (Ok 100)
            ]
        , describe "1000 & 500"
            [ test "M" <| \_ -> fromString "M" |> Result.map toInt |> equal (Ok 1000)
            , test "D" <| \_ -> fromString "D" |> Result.map toInt |> equal (Ok 500)
            , test "MM" <| \_ -> fromString "MM" |> Result.map toInt |> equal (Ok 2000)
            , test "MMM" <| \_ -> fromString "MMM" |> Result.map toInt |> equal (Ok 3000)
            , test "MMMM" <| \_ -> fromString "MMMM" |> Result.map toInt |> equal (Ok 4000)
            , test "MCMLXXIII" <| \_ -> fromString "MCMLXXIII" |> Result.map toInt |> equal (Ok 1973)
            ]
        , describe "Error"
            [ test "IL" <| \_ -> fromString "IL" |> Result.map toInt |> equal (Err "Invalid sequence of letters")
            , test "IC" <| \_ -> fromString "IC" |> Result.map toInt |> equal (Err "Invalid sequence of letters")
            , test "Coucou" <| \_ -> fromString "Coucou" |> Result.map toInt |> equal (Err "Invalid symbol 'o'")
            , test "empty" <| \_ -> fromString "" |> Result.map toInt |> equal (Err "Empty string input is not allowed")
            , test "LIIII" <| \_ -> fromString "LIIII" |> Result.map toInt |> equal (Err "Invalid input string: more than 3 identical letters in a row")
            ]
        ]
