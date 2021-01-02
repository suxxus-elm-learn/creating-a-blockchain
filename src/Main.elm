module Main exposing (main)

import Array exposing (..)
import Browser
import Crypto.Hash
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Task
import Time exposing (Posix)



--- ALIAS ---


type alias BlockChain =
    List Block


type alias Block =
    { index : Int
    , time : String
    , data : BlockData
    , previousHash : String
    , hash : String
    }


type alias BlockData =
    { sender : String
    , receives : String
    , amount : Int
    }


type alias Model =
    { chain : BlockChain
    , data : BlockData
    , validChain : Bool
    , validated : Bool
    , showBlockChain : Bool
    }



--- DATA ---


initialData : BlockData
initialData =
    { sender = ""
    , receives = ""
    , amount = 0
    }


defaultBlock : Block
defaultBlock =
    { index = 0
    , time = ""
    , data = initialData
    , previousHash = ""
    , hash = ""
    }


sender : String -> ( String, Encode.Value )
sender value =
    ( "sender", Encode.string value )


receives : String -> ( String, Encode.Value )
receives value =
    ( "receives", Encode.string value )


amount : Int -> ( String, Encode.Value )
amount value =
    ( "amout", Encode.int value )


encodeBlockData : BlockData -> String
encodeBlockData blockData =
    Encode.object
        [ sender blockData.sender
        , receives blockData.receives
        , amount blockData.amount
        ]
        |> Encode.encode 0



--- HELPERS ---


isChainValid : BlockChain -> Int -> Int -> Bool
isChainValid blockChain len count =
    let
        currentBlock =
            Maybe.withDefault defaultBlock
                (Array.get count
                    (Array.fromList
                        blockChain
                    )
                )

        previousBlock =
            Maybe.withDefault defaultBlock
                (Array.get (count - 1)
                    (Array.fromList blockChain)
                )

        validateHash =
            \_ ->
                if currentBlock.previousHash /= previousBlock.hash then
                    False

                else if
                    currentBlock.hash
                        /= calculateHash currentBlock.index
                            currentBlock.time
                            currentBlock.data
                            currentBlock.previousHash
                then
                    False

                else
                    isChainValid blockChain len (count + 1)
    in
    if count < len then
        validateHash ()

    else
        True


getCurrentTime : Cmd Msg
getCurrentTime =
    Time.now
        |> Task.perform CurrentTime


posixToString : Posix -> String
posixToString time =
    time
        |> Time.posixToMillis
        |> String.fromInt


calculateHash : Int -> String -> BlockData -> String -> String
calculateHash idx time blockData prevHash =
    Crypto.Hash.sha224
        (String.fromInt idx
            ++ time
            ++ encodeBlockData blockData
            ++ prevHash
        )


createGenesisBlock : Block
createGenesisBlock =
    let
        blockData =
            { sender = "", receives = "", amount = 0 }
    in
    { index = 0
    , time = "0"
    , data = blockData
    , previousHash = "0"
    , hash = calculateHash 0 "0" blockData "0"
    }


getNextIndex : List Block -> Int
getNextIndex chain =
    List.length chain


getPreviousHash : List Block -> String
getPreviousHash list =
    list
        |> List.reverse
        |> List.head
        |> Maybe.withDefault defaultBlock
        |> (\block -> block.hash)


createNewBlock : Model -> String -> Block
createNewBlock model time =
    let
        blockData =
            { sender = model.data.sender
            , receives = model.data.receives
            , amount =
                model.data.amount
            }

        nextIndex =
            getNextIndex model.chain

        prevHash =
            getPreviousHash model.chain
    in
    { index = nextIndex
    , time = time
    , data = blockData
    , previousHash = prevHash
    , hash = calculateHash nextIndex time blockData prevHash
    }


isDataSetted : BlockData -> Bool
isDataSetted blockData =
    List.all (\item -> item)
        [ String.length blockData.sender > 0
        , String.length blockData.receives > 0
        , blockData.amount > 0
        ]


addBlock : Model -> Posix -> List Block
addBlock model posix =
    let
        newBlock =
            createNewBlock model (posixToString posix)

        chainReversed =
            List.reverse model.chain

        elementAppended =
            newBlock :: chainReversed
    in
    List.reverse elementAppended



---- MODEL ----


init : ( Model, Cmd Msg )
init =
    let
        model =
            { chain = List.singleton createGenesisBlock
            , data = initialData
            , validChain = False
            , validated = False
            , showBlockChain = False
            }
    in
    ( model, Cmd.none )


setSender : BlockData -> String -> BlockData
setSender blockData value =
    { blockData | sender = value }


setReceives : BlockData -> String -> BlockData
setReceives blockData value =
    { blockData | receives = value }


setAmount : BlockData -> String -> BlockData
setAmount blockData value =
    let
        checkaAmount =
            if value == "" then
                0

            else
                String.toInt value |> Maybe.withDefault blockData.amount
    in
    { blockData | amount = checkaAmount }



---- UPDATE ----


type Msg
    = CurrentTime Posix
    | ChangeBlockDataSender String
    | ChangeBlockDataReceives String
    | ChangeBlockDataAmount String
    | SubmitBlockData
    | ValidateBlockChain
    | ShowData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeBlockDataSender value ->
            ( { model | data = setSender model.data value }, Cmd.none )

        ChangeBlockDataReceives value ->
            ( { model | data = setReceives model.data value }, Cmd.none )

        ChangeBlockDataAmount value ->
            ( { model | data = setAmount model.data value }, Cmd.none )

        SubmitBlockData ->
            ( model, getCurrentTime )

        CurrentTime posix ->
            ( { model | chain = addBlock model posix }, Cmd.none )

        ValidateBlockChain ->
            let
                chain =
                    model.chain

                len =
                    List.length chain

                count =
                    1
            in
            ( { model
                | validated = True
                , validChain =
                    isChainValid chain len count
              }
            , Cmd.none
            )

        ShowData ->
            ( { model | showBlockChain = not model.showBlockChain }, Cmd.none )



-- VIEW ----


blockDataFields : BlockData -> Html Msg
blockDataFields blockData =
    Html.form
        [ class "ba b--mid-gray pa3 mb4" ]
        [ h3 [ class "f3 f3-m" ]
            [ text "New block" ]
        , label
            [ for "sender" ]
            [ text "sender" ]
        , br []
            []
        , input
            [ id "sender"
            , name "sender"
            , type_ "text"
            , onInput ChangeBlockDataSender
            , value blockData.sender
            ]
            []
        , br []
            []
        , label [ for "recive" ]
            [ text "recive" ]
        , br []
            []
        , input
            [ id "recive"
            , name "recive"
            , type_ "text"
            , onInput ChangeBlockDataReceives
            , value blockData.receives
            ]
            []
        , br []
            []
        , label [ for "amount" ]
            [ text "amount" ]
        , br []
            []
        , input
            [ id "amount"
            , name "amount"
            , type_ "text"
            , onInput ChangeBlockDataAmount
            , value <| String.fromInt blockData.amount
            ]
            []
        , br []
            []
        , button
            [ type_ "button"
            , onClick SubmitBlockData
            , disabled <| not (isDataSetted blockData)
            , class "mt4"
            ]
            [ text "Submit" ]
        ]


showValidationChainMsg : Bool -> Bool -> Html Msg
showValidationChainMsg isValidated validChain =
    if isValidated then
        if validChain then
            p
                []
                [ text "valid" ]

        else
            p [] [ text "not valid" ]

    else
        p [] [ text "not validated" ]


validateBlockChainField : Model -> Html Msg
validateBlockChainField model =
    div
        [ class "ba b--mid-gray br2 pa3 mb4" ]
        [ button [ onClick ValidateBlockChain ]
            [ text "validate blockchain" ]
        , showValidationChainMsg model.validated model.validChain
        ]


blockElements : Block -> List (Html Msg)
blockElements block =
    [ li [] [ text <| "index: " ++ String.fromInt block.index ]
    , li [] [ text <| "time: " ++ block.time ]
    , li [] [ text <| "data: " ++ encodeBlockData block.data ]
    , li [] [ text <| "previousHash: " ++ block.previousHash ]
    , li [] [ text <| "hash: " ++ block.hash ]
    ]


listBlocksElements : BlockChain -> List (Html Msg)
listBlocksElements blockchain =
    List.map
        (\block ->
            block
                |> blockElements
                |> ul [ class "ml5 list pl0 tl" ]
        )
        blockchain


renderBlockChainElements : BlockChain -> Bool -> Html Msg
renderBlockChainElements blockchain hasToShowData =
    div
        [ class "ba b--mid-gray br2 pa3" ]
        [ button [ onClick ShowData ] [ text "show blockChain" ]
        , if hasToShowData then
            blockchain
                |> listBlocksElements
                |> div []

          else
            div [] []
        ]


getLastBlockFromChain : BlockChain -> Block
getLastBlockFromChain blockchain =
    blockchain
        |> List.reverse
        |> List.head
        |> (\item -> Maybe.withDefault defaultBlock item)


showCreatedBlock : BlockChain -> Html Msg
showCreatedBlock blockchain =
    div
        [ class "ba b--mid-gray br2 pa3 mb4" ]
        [ h3 [ class "f3 f3-m" ] [ text "last block" ]
        , blockchain
            |> getLastBlockFromChain
            |> blockElements
            |> ul [ class "ml5 list pl0 tl" ]
        ]


view : Model -> Html Msg
view model =
    div [ class "pa5" ]
        [ h1 [] [ text "Simple Block chain" ]
        , showCreatedBlock model.chain
        , blockDataFields model.data
        , validateBlockChainField model
        , renderBlockChainElements model.chain model.showBlockChain
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
