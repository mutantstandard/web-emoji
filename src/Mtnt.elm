module Mtnt exposing ( Emoji
                     , MM(..)
                     , Morph(..)
                     , CM(..)
                     , Color(..)
                     , Error(..)
                     , setDecoder

                     , humanColors
                     , pawColors
                     , clawColors
                     , hoofColors
                     , allColors
                     , isValidModifierPairing
                     , shortcodeFromMM
                     , shortcodeFromCM
                     , emojiHasMM
                     , emojiHasCM

                     , getEmojiFromShortcode
                     , getDesc
                     , codeInMtntPUA
                     )

{-| A module for the decoding of Mutant Standard
JSON Metadata and presenting and interpreting Mutant Standard emoji.
-}


import Json.Decode exposing (Decoder, andThen, oneOf, string, list, int, fail, succeed)
import Json.Decode.Pipeline exposing (required, optional)




{-| A single Mutant Standard emoji as represented
by a single JSON entry.

- short: shortcode
- root: The base emoji shortcode - that is to say,
if it has modifiers, this is the shortcode without modifiers.
- desc: Image description for the emoji, in English.
- cat: category (expressions, food, animals, etc.)

optional elements
- code: The sequence of Unicode codepoints that refer to this emoji,
 as represented by a list of Integers. If it's empty, this emoji has
 no assigned codepoints.
- color: Color Modifier (CM).
- morph: Morph Modifier (MM).

'src' is ignored because 'src' is the file used in 
he original build instructions.

-}
type alias Emoji =
    { short : String
    , root : String
    , desc : String
    , cat : String

    -- maybes
    , code : List Int
    , color : CM
    , morph : MM
    }


{-| Decoder for all entries in a Mutant Standard JSON.
-}
setDecoder : Decoder (List Emoji)
setDecoder =
    list emojiDecoder


{-| Decoder for a single emoji entry from a Mutant Standard JSON.
-}
emojiDecoder : Decoder Emoji
emojiDecoder = 
    succeed Emoji
    |> required "short" string
    |> required "root" string
    |> required "desc" string
    |> required "cat" string
    |> required "code" codepointDecoder
    |> optional "color"
        (string |> andThen colorDecoder)
        NoCM
    |> optional "morph"
        (string |> andThen morphDecoder)
        NoMM


{-| Decodes the `code` field of a Mutant Standard JSON.

In the JSON, a string with "!" is marking an emoji as having no codepoints.
If codepoint(s), do exist, then the field is a list of ints.
-}
codepointDecoder : Decoder (List Int)
codepointDecoder = 
     oneOf [ list int
           , string |> andThen (\_ -> succeed [])
           ]




{-| Type encapsulating the kinds of errors that can be made in this module.

- `InvalidModifierCombo` - a MM and CM pairing are invalid.
-}
type Error
    = InvalidModifierCombo

------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
                       --CMs and MMs--
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------








{-| Custom Type representing all morph modifiers
in Mutant Standard.

See the Mutant Standard modifier documentation for
information on what they all mean and how they work.
-}
type MM
    = NoMM
    | AssignedMM Morph


type Morph
    = Human
    | Paw
    | Claw
    | Hoof

{-| Converts an MM to its equivalent string in shortcodes.
-}
shortcodeFromMM : MM -> Maybe String
shortcodeFromMM mm =
    case mm of
        NoMM -> Nothing
        AssignedMM m -> Just <| shortcodeFromMorph m


{-| Converts a Morph to its equivalent string in shortcodes.
-}
shortcodeFromMorph : Morph -> String
shortcodeFromMorph morph =
    case morph of
        Human -> "hmn"
        Paw -> "paw"
        Claw -> "claw"
        Hoof -> "hoof"


{-| Tries to return an equivalent MM from it's shortcode string,
will return Nothing if the input string is incorrect.
-}
morphFromShortcode : String -> Maybe Morph
morphFromShortcode str =
    case str of
        "hmn" -> Just Human
        "paw" -> Just Paw
        "clw" -> Just Claw
        "hoof" -> Just Hoof
        _ -> Nothing


{-| Decoder for morph modifier values in a Mutant Standard JSON.
-}
morphDecoder :  String -> Decoder MM
morphDecoder str =
    case morphFromShortcode str of
        Just mm -> succeed <| AssignedMM mm
        Nothing -> fail <| "Morph Modifier" ++ str ++ "is not supported."




---------------------------------------------




{-| Custom Type representing the structure of CM values in
Mutant Standard.

Whenever there is a CM involved in Mutant Standard, it can be
assigned or unassigned (default color). As such, the structure
is a bit different compared to MMs.
-}
type CM
    = NoCM
    | UnassignedCM -- 'default'
    | AssignedCM Color


{-| Custom Type representing all color modifiers
in Mutant Standard.

See the Mutant Standard modifier documentation for
information on what they all mean and how they work.
-}
type Color
    -- human only
    = H1
    | H2
    | H3
    | H4
    | H5

    -- universal palettes
    | R1
    | R2
    | R3
    | D1
    | D2
    | D3
    | O1
    | O2
    | O3
    | Y1
    | Y2
    | Y3

    | L1
    | L2
    | L3
    | G1
    | G2
    | G3
    | T1
    | T2
    | T3

    | C1
    | C2
    | C3
    | S1
    | S2
    | S3
    | B1
    | B2
    | B3

    | V1
    | V2
    | V3
    | M1
    | M2
    | M3
    | P1
    | P2
    | P3

    | E1
    | E2
    | E3
    | K1
    | K2
    | K3

    -- experimental for furries
    | FE1
    | FT1
    | FK1


{-| Converts a Color to its equivalent string in shortcodes.

Both an emoji that has no CMs and an emoji that has a 'default' CM
will return nothing, as default CMs aren't explicitly represented
by a shortcode.
-}
shortcodeFromCM : CM -> Maybe String
shortcodeFromCM cm =
    case cm of
        NoCM -> Nothing
        UnassignedCM -> Nothing
        AssignedCM c -> Just <| shortcodeFromColor c


{-| Converts a Color to its equivalent string in shortcodes.
-}
shortcodeFromColor : Color -> String
shortcodeFromColor col =
    case col of
        H1 -> "h1"
        H2 -> "h2"
        H3 -> "h3"
        H4 -> "h4"
        H5 -> "h5"

        R1 -> "r1"
        R2 -> "r2"
        R3 -> "r3"
        D1 -> "d1"
        D2 -> "d2"
        D3 -> "d3"
        O1 -> "o1"
        O2 -> "o2"
        O3 -> "o3"
        Y1 -> "y1"
        Y2 -> "y2"
        Y3 -> "y3"

        L1 -> "l1"
        L2 -> "l2"
        L3 -> "l3"
        G1 -> "g1"
        G2 -> "g2"
        G3 -> "g3"
        T1 -> "t1"
        T2 -> "t2"
        T3 -> "t3"

        C1 -> "c1"
        C2 -> "c2"
        C3 -> "c3"
        S1 -> "s1"
        S2 -> "s2"
        S3 -> "s3"
        B1 -> "b1"
        B2 -> "b2"
        B3 -> "b3"

        V1 -> "v1"
        V2 -> "v2"
        V3 -> "v3"
        M1 -> "m1"
        M2 -> "m2"
        M3 -> "m3"
        P1 -> "p1"
        P2 -> "p2"
        P3 -> "p3"

        E1 -> "e1"
        E2 -> "e2"
        E3 -> "e3"
        K1 -> "k1"
        K2 -> "k2"
        K3 -> "k3"

        FE1 -> "fe1"
        FT1 -> "ft1"
        FK1 -> "fk1"


{-| Tries to return an equivalent CM from it's JSON string,
will return Nothing if the input string is incorrect.

This is different to colorFromString, as it includes 'default',
which means no CM was assigned to the emoji.

This is called 'FromJsonString' because the way the 'default'
CM is represented in JSON and shortcodes is not the same.
-}
cmFromJsonString : String -> Maybe CM
cmFromJsonString str =
    case str of
        "default" -> Just UnassignedCM
        _ -> Maybe.map (\x -> AssignedCM x) (colorFromShortcode str)


{-| Tries to return an equivalent Color from it's shortcode string,
will return Nothing if the input string is incorrect.
-}
colorFromShortcode : String -> Maybe Color
colorFromShortcode str =
    case str of
        "h1" -> Just H1
        "h2" -> Just H2
        "h3" -> Just H3
        "h4" -> Just H4
        "h5" -> Just H5

        "r1" -> Just R1
        "r2" -> Just R2
        "r3" -> Just R3
        "d1" -> Just D1
        "d2" -> Just D2
        "d3" -> Just D3
        "o1" -> Just O1
        "o2" -> Just O2
        "o3" -> Just O3
        "y1" -> Just Y1
        "y2" -> Just Y2
        "y3" -> Just Y3

        "l1" -> Just L1
        "l2" -> Just L2
        "l3" -> Just L3
        "g1" -> Just G1
        "g2" -> Just G2
        "g3" -> Just G3
        "t1" -> Just T1
        "t2" -> Just T2
        "t3" -> Just T3

        "c1" -> Just C1
        "c2" -> Just C2
        "c3" -> Just C3
        "s1" -> Just S1
        "s2" -> Just S2
        "s3" -> Just S3
        "b1" -> Just B1
        "b2" -> Just B2
        "b3" -> Just B3

        "v1" -> Just V1
        "v2" -> Just V2
        "v3" -> Just V3
        "m1" -> Just M1
        "m2" -> Just M2
        "m3" -> Just M3
        "p1" -> Just P1
        "p2" -> Just P2
        "p3" -> Just P3

        "e1" -> Just E1
        "e2" -> Just E2
        "e3" -> Just E3
        "k1" -> Just K1
        "k2" -> Just K2
        "k3" -> Just K3

        "fe1" -> Just FE1
        "ft1" -> Just FT1
        "fk1" -> Just FK1

        _ -> Nothing


{-| Decoderfor color modifier values in a Mutant Standard JSON.
-}
colorDecoder : String -> Decoder CM
colorDecoder str =
    case cmFromJsonString str of
        Just cm -> succeed cm
        Nothing -> fail <| "Color Modifier" ++ str ++ "is not supported."




{-| A list outlining all of the possible CM Colors for
human-esque contexts.
-}
humanColors : List Color
humanColors =
    restrictedHumanPalette
    ++ sharedPalette


{-| A list outlining all of the possible CM Colors for
paw hand contexts.
-}
pawColors : List Color
pawColors =
    sharedPalette
    ++ restrictedPawPalette


{-| A list outlining all of the possible CM Colors for
claw hand contexts.
-}
clawColors : List Color
clawColors = sharedPalette


{-| A list outlining all of the possible CM Colors for
hoof hand contexts.
-}
hoofColors : List Color
hoofColors = sharedPalette


{-| A list of every single color in Mutant Standard.

Used for things like body expressions, body parts and roles.
-}
allColors : List Color
allColors =
    restrictedHumanPalette
    ++ sharedPalette
    ++ restrictedPawPalette



{-| The list of CM Colors that only human morphs
(and emoji with no morphs) can have.
-}
restrictedHumanPalette : List Color
restrictedHumanPalette =
    [ H1, H2, H3, H4, H5 ]

{-| The list of CM Colors that only paw morphs
(and emoji with no morphs) can have.
-}
restrictedPawPalette : List Color
restrictedPawPalette =
    [ FE1, FT1, FK1 ]

{-| A list of designated shared CM Colors - colors that
are guaranteed to work with anything that CMs
can be applied to.
-}
sharedPalette : List Color
sharedPalette =
    [ R1, R2, R3
    , D1, D2, D3
    , O1, O2, O3
    , Y1, Y2, Y3
    , L1, L2, L3
    , G1, G2, G3
    , T1, T2, T3
    , C1, C2, C3
    , S1, S2, S3
    , B1, B2, B3
    , V1, V2, V3
    , M1, M2, M3
    , P1, P2, P3
    , E1, E2, E3
    , K1, K2, K3
    ]


{-| Takes a CM and MM and tests them to make sure that
they are a valid pairing in Mutant Standard.

Basic rules:
- Hx CMs are only for Human morphs.
- Fx CMs are only for Paw morphs.
- `default`/Unassigned CMs work with everything.
- Emoji with no morphs (body expressions, body parts, roles) have to take every CM.
- You can't have no CM
-}
isValidModifierPairing : MM -> CM -> Bool
isValidModifierPairing mm cm =
    case cm of
        NoCM ->
            case mm of
                NoMM -> True -- It's fine if they're both none.
                AssignedMM _ -> False -- You can't assign an MM without a CM.

        -- Default CMs work with anything.
        UnassignedCM -> True

        -- Assigned CMs work circumstantially.
        AssignedCM col ->
            case mm of
                -- body expressions, body parts and roles have
                -- no MM, so they can accept any CM.
                NoMM -> True
    
                -- make sure the CM is allowed based on the morph:
                AssignedMM morph ->
                    case morph of
                        Human -> List.member col humanColors
                        Paw -> List.member col pawColors
                        Claw -> List.member col clawColors
                        Hoof -> List.member col hoofColors



{-| Checks an emoji to see if it has an MM.
-}
emojiHasMM : Emoji -> Bool
emojiHasMM emoji =
    emoji.morph /= NoMM


{-| Checks an emoji to see if it has a CM.
-}
emojiHasCM : Emoji -> Bool
emojiHasCM emoji =
    emoji.color /= NoCM



------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
                       --Helper functions--
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------


{-| Tries to get an emoji from a shortcode.

Returns Nothing if it fails.
-}
getEmojiFromShortcode : String -> List Emoji -> Maybe Emoji
getEmojiFromShortcode short emojiList =
    List.filter (\e -> e.short == short) emojiList
        |> List.head


{-| Tries to get an image description from a shortcode string.

Returns Nothing if it fails.
-}
getDesc : String -> List Emoji -> Maybe String
getDesc short emojiList =
    Maybe.map (\e -> e.desc) (getEmojiFromShortcode short emojiList)


{-| The start of the Mutant Standard PUA area - 
101600 in hexadecimal.
-}
mtntPUAStartCode : Int
mtntPUAStartCode = 1054208


{-| The current end of the Mutant Standard PUA area -
1016FF in hexadecimal.

The Mutant Standard PUA Standard may expand to subsequent
blocks in the future, but this is where the current end is at.
-}
mtntPUAEndCode : Int
mtntPUAEndCode = 1054463


{-| Checks if the codepoint of an emoji is
in the Mutant Standard PUA range or not.

Will return true if at least one codepoint
in the sequence is in the PUA area.
-}
codeInMtntPUA : Emoji -> Bool
codeInMtntPUA emoji =
    let
        codeInPUA : Int -> Bool
        codeInPUA code = code >= mtntPUAStartCode && code <= mtntPUAEndCode
    in
    List.any codeInPUA emoji.code
