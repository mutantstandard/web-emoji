# web-emoji

This is an Elm module that lets you seamlessly use [Mutant Standard emoji](https://mutant.tech) in your web app.

Decode a Mutant Standard Data JSON, pair it with a set of Mutant Standard emoji and use it to:
- Create accessible emoji views with image descriptions.
- Create emoji pickers with proper categories and Mutant Standard modifiers.

```

getEmojiData : Cmd Msg
getEmojiData =
    Http.get
        { url = "/emoji/mtnt_2020.04_data.json"
        , expect = Http.expectJson GotEmojiData Mtnt.decoder
        }

```


```
emojiView : Shared.Model -> String -> Html msg
emojiView shared shortcode =
    let
        -- Attempt to get text description from loaded JSON
        desc =
            case shared.emoji of
                Success e ->
                    case Mtnt.descFromShort short e of
                        Just d -> [ alt d ]
                        Nothing -> []
                _  -> []
    in

    -- Create picture with fallback PNG
    Html.node "picture"
        [ css
            [ property "image-rendering" "-webkit-optimize-contrast"
            , property "object-fit" "contain"
            ]
        , class "emoji"
        ]
        [ Html.source
            [ Attr.type_ "image/webp"
            , Attr.attribute "srcset" ("/emoji/webp-128/" ++ short ++ ".webp")
            , css
                [ property "object-fit" "contain"
                , maxHeight (pct 100)
                ]
            ]
            []
        , Html.img
            (   [ Attr.src ("/emoji/pngc-128/" ++ short ++ ".png")
                , css
                    [ property "object-fit" "contain"
                    , maxHeight (pct 100)
                    ]
                ]
                ++ desc
            )
            []

        ]

```
Different versions of this package may change to fit changing standards in subsequent versions of Mutant Standard. Because Elm version numbering is strictly semantic, they cannot be the same - please check release notes so you can pair versions properly.

