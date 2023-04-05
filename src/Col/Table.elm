module Col.Table exposing (make_tbl)

import Html exposing (Html, text, table, tr, td)


make_tbl : Html msg
make_tbl =
    table []
        [ tr [] [ td [] [ text "Row 1, Column 1" ]
                , td [] [ text "Row 1, Column 2" ]
                , td [] [ text "Row 1, Column 3" ]
                , td [] [ text "Row 1, Column 4" ]
                , td [] [ text "Row 1, Column 5" ]
                ]
        , tr [] [ td [] [ text "Row 2, Column 1" ]
                , td [] [ text "Row 2, Column 2" ]
                , td [] [ text "Row 2, Column 3" ]
                , td [] [ text "Row 2, Column 4" ]
                , td [] [ text "Row 2, Column 5" ]
                ]
        , tr [] [ td [] [ text "Row 3, Column 1" ]
                , td [] [ text "Row 3, Column 2" ]
                , td [] [ text "Row 3, Column 3" ]
                , td [] [ text "Row 3, Column 4" ]
                , td [] [ text "Row 3, Column 5" ]
                ]
        , tr [] [ td [] [ text "Row 4, Column 1" ]
                , td [] [ text "Row 4, Column 2" ]
                , td [] [ text "Row 4, Column 3" ]
                , td [] [ text "Row 4, Column 4" ]
                , td [] [ text "Row 4, Column 5" ]
                ]
        , tr [] [ td [] [ text "Row 5, Column 1" ]
                , td [] [ text "Row 5, Column 2" ]
                , td [] [ text "Row 5, Column 3" ]
                , td [] [ text "Row 5, Column 4" ]
                , td [] [ text "Row 5, Column 5" ]
                ]
        ]
