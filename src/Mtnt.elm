module Mtnt exposing
    ( decoder
    , Set
    , setFromDecoded
    , Picker
    , pickerFromSet

    , emojiFromShort
    , descFromShort
    , englishTitleFromCat
    )

import Dict exposing (Dict)
import Mtnt.Emoji exposing (Emoji)
import Json.Decode exposing (Decoder)



{-| Decoder for all entries in a Mutant Standard JSON.
-}
decoder : Decoder (List Emoji)
decoder =
    Json.Decode.list Mtnt.Emoji.decoder


{-| Type alias for a shortcode to make the type annotations more readable in the package.
-}
type alias Short = String

{-| Representing all of the important data to come from the emoji JSON.

Dicts are important for performant search, while Lists are important
because for pickers, there's a certain order to emoji that
was pre-encoded into the JSON.

So the two are encoded in the `data` and `order` fields respectively.
-}
type alias Set =
    { data : Dict Short Emoji
    , order : List Short
    }


{-| Takes a List Emoji from an an attempt to decode and if
it was successful, makes the decoded list into a Dict for
properly working with.
-}
setFromDecoded : List Emoji -> Set
setFromDecoded emojiList =
    let 
        acc = List.foldl
            (\emoji (accDict, accList) ->
                ( Dict.insert emoji.short emoji accDict
                , emoji.short :: accList
                )
            )
            (Dict.empty, [])
            emojiList
    in
        { data = Tuple.first acc
        , order = List.reverse <| Tuple.second acc -- better performance to cons first and reverse after all is done.
        }

{-| A data structure suitable for an emoji picker or demo.

- modifiableShorts : A List of shortcodes whose emoji are modifiable
in some way, so changing modifiers is more efficient.
- deduplicatedData : An alternative version of the emoji set data where
duplicate modified emoji have been removed.
- deduplicatedOrder : An alternative version of the emoji order that includes categories.
-}
type alias Picker =
    { modifiableShorts : List Short
    , deduplicatedData : Dict Short Emoji
    , deduplicatedOrder : Dict String (List Short)
    , catOrder : List String
    }


{-| Converts a set into a picker.
-}
pickerFromSet : Set -> Picker
pickerFromSet set =
    let 
        accumulator =
            { modifiableShorts = []
            , deduplicatedData = Dict.empty
            , deduplicatedOrder = Dict.empty
            , catOrder = []
            }

        initialAcc =
            List.foldl
                    (\short acc ->
                        case Dict.get short set.data of
                            Nothing -> acc -- This probably will never happen unless something goes really bad.
                            Just emoji ->

                                -- check if the root is already in deduplicatedData.
                                case Dict.get emoji.root acc.deduplicatedData of
                                    Just _ -> acc

                                    Nothing ->
                                        acc

                                        -- insert emoji to deduplicatedData
                                        |>  (\a -> { a | deduplicatedData = Dict.insert emoji.root emoji acc.deduplicatedData } )

                                        -- add emoji to the modifiable list if it's modifiable.
                                        |>  (\a -> 
                                                if Mtnt.Emoji.isAnyModifiable emoji then
                                                    { a | modifiableShorts = emoji.short :: acc.modifiableShorts }
                                                else
                                                    a
                                            )

                                        -- add category to the category order list if it's not already there.
                                        |> (\a ->
                                                if List.member emoji.cat acc.catOrder then
                                                    a
                                                else
                                                    { a | catOrder = emoji.cat :: acc.catOrder }
                                            )


                                        -- add to deduplicated order (and add category if it doesn't already exist)
                                        |>  (\a ->
                                                case Dict.get emoji.cat acc.deduplicatedOrder of
                                                    Nothing ->
                                                        { a | deduplicatedOrder = Dict.insert emoji.cat [emoji.short] acc.deduplicatedOrder }
                                                    Just catList ->
                                                        { a | deduplicatedOrder = Dict.insert emoji.cat (emoji.short :: catList) acc.deduplicatedOrder }

                                            )
                                        

                    )
                    accumulator
                    set.order -- use the order for iteration.
        
        -- once again, cons and then reverse for speed.
        reversedOrderAcc =
            initialAcc
            |> (\a -> { a | deduplicatedOrder = 
                        Dict.map
                            (\cat orderList -> List.reverse orderList)
                            initialAcc.deduplicatedOrder
                    }
                )
            |> (\a -> { a | catOrder = List.reverse a.catOrder })

    in
        reversedOrderAcc



----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------
-------------------------- QUERIES -----------------------------
----------------------------------------------------------------
----------------------------------------------------------------


{-| Tries to get an emoji from a shortcode.

Returns Nothing if it fails.
-}
emojiFromShort : Short -> Set -> Maybe Emoji
emojiFromShort short set =
    Dict.get short set.data


{-| Tries to get an image description from a shortcode string.

Returns Nothing if it fails.
-}
descFromShort : Short -> Set -> Maybe String
descFromShort short set =
    Maybe.map (\e -> e.desc) (emojiFromShort short set)


{-| Gives an english category name from a cat string
-}
englishTitleFromCat : String -> Maybe String
englishTitleFromCat str =
    case str of
        "expressions" -> Just "Expressions"
        "people" -> Just "People"
        "activities_clothing" -> Just "Activities & Clothing"
        "gsr" -> Just "Gender, Sexuality and Relationships"
        "food_drink_herbs" -> Just "Food, Drink and Herbs"
        "nature" -> Just "Nature"
        "travel_places" -> Just "Travel & Places"
        "objects" -> Just "Objects"
        "extra" -> Just "Extra"
        "symbols" -> Just "Symbols"
        "utils" -> Just "Font utility symbols"
        _ -> Nothing
