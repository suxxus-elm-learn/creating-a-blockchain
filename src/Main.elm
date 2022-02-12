module Main exposing (main)

import Array exposing (..)
import Browser
import Crypto.Hash
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Regex
import Task
import Time exposing (Posix, Zone)



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
    , amount : String
    }


type alias Model =
    { chain : BlockChain
    , data : BlockData
    , isChainValidated : Bool
    , chainHasBeenValidated : Bool
    , validReceiver : Bool
    , validSender : Bool
    , validAmount : Bool
    , showBlockChain : Bool
    , zone : Zone
    }



--- DATA ---


initialData : BlockData
initialData =
    { sender = ""
    , receives = ""
    , amount = ""
    }


defaultBlock : Block
defaultBlock =
    { index = 0
    , time = ""
    , data = initialData
    , previousHash = ""
    , hash = ""
    }



--- ENCODERS ---


sender : String -> ( String, Encode.Value )
sender value =
    ( "sender", Encode.string value )


receives : String -> ( String, Encode.Value )
receives value =
    ( "receives", Encode.string value )


amount : String -> ( String, Encode.Value )
amount value =
    ( "amount", Encode.string value )


encodeBlockData : BlockData -> String
encodeBlockData blockData =
    Encode.object
        [ sender blockData.sender
        , receives blockData.receives
        , amount blockData.amount
        ]
        |> Encode.encode 0



--- REGEX ---


testRegex : Regex.Regex -> (String -> Bool)
testRegex regex =
    Regex.contains regex


validateRegexExp : String -> Regex.Regex
validateRegexExp pattern =
    pattern
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


numberWithDecimals : Regex.Regex
numberWithDecimals =
    "^[+-]?(\\d*\\.)?\\d+$"
        |> validateRegexExp


numberStartsWithZero : Regex.Regex
numberStartsWithZero =
    "^(0\\d{1,})$"
        |> validateRegexExp



---- PREDICATES ----


isGreaterThanZero : Float -> Bool
isGreaterThanZero value =
    value > 0



---- NEW BLOCK FORM VALIDATORS ----


isValidAmount : String -> Bool
isValidAmount value =
    let
        patternMatch =
            value
                |> testRegex numberWithDecimals

        greaterThanZero =
            value
                |> String.toFloat
                |> Maybe.withDefault 0
                |> isGreaterThanZero
    in
    patternMatch && greaterThanZero


startsWithZero : String -> Bool
startsWithZero value =
    testRegex numberStartsWithZero value


checkAmountField : String -> String
checkAmountField value =
    let
        shouldRemoveTheLeadingZeros =
            startsWithZero value
    in
    if shouldRemoveTheLeadingZeros then
        value
            |> String.toFloat
            |> Maybe.withDefault 0
            |> (\x -> x + 0)
            |> String.fromFloat

    else
        value


isNotEmpty : String -> Bool
isNotEmpty value =
    String.length value > 0



---- BLOCK CHAIN VALIDATOR ----


isChainValid : BlockChain -> Int -> Int -> Bool
isChainValid blockChain len count =
    let
        currentBlock =
            getBlockFromBlockChain blockChain defaultBlock count

        previousBlock =
            getBlockFromBlockChain blockChain defaultBlock (count - 1)

        hash =
            calculateHash currentBlock.index
                currentBlock.time
                currentBlock.data
                currentBlock.previousHash

        isValidHash =
            \_ ->
                if currentBlock.previousHash /= previousBlock.hash then
                    False

                else if currentBlock.hash /= hash then
                    False

                else
                    isChainValid blockChain len (count + 1)
    in
    if count < len then
        isValidHash ()

    else
        True



--- HELPERS ---


getBlockFromBlockChain : BlockChain -> Block -> Int -> Block
getBlockFromBlockChain blockchain block index =
    blockchain
        |> Array.fromList
        |> Array.get index
        |> Maybe.withDefault block


getCurrentTime : Cmd Msg
getCurrentTime =
    Time.now
        |> Task.perform CurrentTime


getZone : Cmd Msg
getZone =
    Time.here
        |> Task.perform GetTimeZone


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
            { sender = "GENESIS BLOCK", receives = "nothing", amount = "0" }
    in
    { index = 0
    , time = "1609700813806"
    , data = blockData
    , previousHash = "0"
    , hash = calculateHash 0 "0" blockData "0"
    }


getNextIndex : List Block -> Int
getNextIndex chain =
    List.length chain


getPreviousHash : Block -> BlockChain -> String
getPreviousHash block list =
    list
        |> List.reverse
        |> List.head
        |> Maybe.withDefault block
        |> (\{ hash } -> hash)


createNewBlock : Block -> Model -> String -> Block
createNewBlock block model time =
    let
        blockData =
            { sender =
                model.data.sender
            , receives =
                model.data.receives
            , amount =
                model.data.amount
            }

        nextIndex =
            getNextIndex model.chain

        prevHash =
            getPreviousHash block model.chain
    in
    { index = nextIndex
    , time = time
    , data = blockData
    , previousHash = prevHash
    , hash = calculateHash nextIndex time blockData prevHash
    }


validateNewBlockForm : BlockData -> Bool
validateNewBlockForm blockData =
    List.all (\item -> item)
        [ String.length blockData.sender > 0
        , String.length blockData.receives > 0
        , blockData.amount
            |> String.toFloat
            |> Maybe.withDefault 0
            |> isGreaterThanZero
        ]


addBlock : Model -> Posix -> BlockChain
addBlock model t =
    let
        time =
            posixToString t

        newBlock =
            createNewBlock defaultBlock model time

        chainReversed =
            List.reverse model.chain

        elementAppended =
            newBlock :: chainReversed
    in
    List.reverse elementAppended



---- BLOCK DATA FIELDS SETTERS ----


setSender : BlockData -> String -> BlockData
setSender blockData value =
    { blockData | sender = value }


setReceives : BlockData -> String -> BlockData
setReceives blockData value =
    { blockData | receives = value }


setAmount : BlockData -> String -> BlockData
setAmount blockData value =
    { blockData | amount = value }



---- MODEL ----


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { chain = List.singleton createGenesisBlock
            , data = initialData
            , isChainValidated = False
            , chainHasBeenValidated = False
            , showBlockChain = False
            , validSender = True
            , validReceiver = True
            , validAmount = True
            , zone = Time.utc
            }
    in
    ( model, getZone )



---- UPDATE ----


type Msg
    = CurrentTime Posix
    | GetTimeZone Zone
    | ChangeBlockDataSender String
    | ChangeBlockDataReceives String
    | ChangeBlockDataAmount String
    | SubmitBlockData
    | ValidateBlockChain
    | ShowData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTimeZone timeZone ->
            ( { model | zone = timeZone }, Cmd.none )

        ChangeBlockDataSender value ->
            ( { model
                | data = setSender model.data value
                , validSender = isNotEmpty value
              }
            , Cmd.none
            )

        ChangeBlockDataReceives value ->
            ( { model
                | data = setReceives model.data value
                , validReceiver = isNotEmpty value
              }
            , Cmd.none
            )

        ChangeBlockDataAmount value ->
            ( { model
                | data = setAmount model.data <| checkAmountField value
                , validAmount = isValidAmount <| checkAmountField value
              }
            , Cmd.none
            )

        SubmitBlockData ->
            ( model, getCurrentTime )

        CurrentTime posix ->
            ( { model
                | chain = addBlock model posix
              }
            , Cmd.none
            )

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
                | chainHasBeenValidated = True
                , isChainValidated =
                    isChainValid chain len count
              }
            , Cmd.none
            )

        ShowData ->
            ( { model | showBlockChain = not model.showBlockChain }, Cmd.none )



-- VIEW ----


blockDataFields : Model -> Html Msg
blockDataFields model =
    Html.form
        [ class "ba b--mid-gray pa3 mb4" ]
        [ h3 [ class "f3 f3-m" ]
            [ text "New block" ]
        , label
            [ for "sender" ]
            [ text "sender*" ]
        , br []
            []
        , input
            [ id "sender"
            , name "sender"
            , type_ "text"
            , onInput ChangeBlockDataSender
            , value model.data.sender
            ]
            []
        , br []
            []
        , div []
            [ if model.validSender then
                text ""

              else
                text "This field is required"
            ]
        , br []
            []
        , label [ for "recive" ]
            [ text "receive*" ]
        , br []
            []
        , input
            [ id "recive"
            , name "receive"
            , type_ "text"
            , onInput ChangeBlockDataReceives
            , value model.data.receives
            ]
            []
        , br []
            []
        , div []
            [ if model.validReceiver then
                text ""

              else
                text "This field is required"
            ]
        , br []
            []
        , label [ for "amount" ]
            [ text "amount*" ]
        , br []
            []
        , input
            [ id "amount"
            , name "amount"
            , type_ "text"
            , onInput ChangeBlockDataAmount
            , value model.data.amount
            ]
            []
        , br []
            []
        , div []
            [ if model.validAmount then
                text ""

              else
                text "Enter a valid amount eg: 1.001"
            ]
        , br []
            []
        , button
            [ type_ "button"
            , onClick SubmitBlockData
            , disabled <| not (validateNewBlockForm model.data)
            , class "mt4"
            ]
            [ text "Submit" ]
        ]



---- SECTION BLOCK CHAIN VALIDATION -----


showValidationChainMsg : Bool -> Bool -> Html Msg
showValidationChainMsg isValidated isChainValidated =
    if isValidated then
        if isChainValidated then
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
        , showValidationChainMsg model.chainHasBeenValidated model.isChainValidated
        ]


getMonth : Time.Month -> String
getMonth month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


formatTime : String -> String
formatTime value =
    if String.length value == 1 then
        "0" ++ value

    else
        value


getCreationDate : Zone -> String -> String
getCreationDate zone t =
    let
        time =
            t
                |> String.toInt
                |> Maybe.withDefault 0
                |> Time.millisToPosix

        year =
            Time.toYear zone time
                |> String.fromInt

        month =
            Time.toMonth zone time
                |> getMonth

        day =
            Time.toDay zone time
                |> String.fromInt
                |> formatTime

        hour =
            Time.toHour zone time
                |> String.fromInt

        minutes =
            Time.toMinute zone time
                |> String.fromInt
                |> formatTime

        seconds =
            Time.toSecond zone time
                |> String.fromInt
                |> formatTime
    in
    year
        ++ "-"
        ++ month
        ++ "-"
        ++ day
        ++ " "
        ++ hour
        ++ ":"
        ++ minutes
        ++ ":"
        ++ seconds


dataFields : BlockData -> Html Msg
dataFields blockData =
    let
        amountStr =
            blockData.amount
    in
    ul [ class "list pl0" ]
        [ li [] [ text <| "sender: " ++ blockData.sender ]
        , li [] [ text <| "receives: " ++ blockData.receives ]
        , li [] [ text <| "amount: " ++ amountStr ]
        ]


blockElements : Zone -> Block -> List (Html Msg)
blockElements zone block =
    [ li [] [ text <| "index: " ++ String.fromInt block.index ]
    , li [] [ text <| "created date: " ++ getCreationDate zone block.time ]
    , li [] [ text <| "time: " ++ block.time ]
    , li []
        [ dataFields block.data
        ]
    , li [] [ text <| "previousHash: " ++ block.previousHash ]
    , li [] [ text <| "hash: " ++ block.hash ]
    ]


listBlocksElements : Zone -> BlockChain -> List (Html Msg)
listBlocksElements zone blockchain =
    List.map
        (\block ->
            block
                |> blockElements zone
                |> ul [ class "ml5 list pl0 tl" ]
        )
        blockchain


renderBlockChainElements : Model -> Html Msg
renderBlockChainElements model =
    let
        hasToShowData =
            model.showBlockChain

        blockchain =
            model.chain

        zone =
            model.zone
    in
    div
        [ class "ba b--mid-gray br2 pa3" ]
        [ button [ onClick ShowData ] [ text "show blockChain" ]
        , if hasToShowData then
            blockchain
                |> listBlocksElements zone
                |> div []

          else
            div [] []
        ]


getLastBlockFromChain : Block -> BlockChain -> Block
getLastBlockFromChain block blockchain =
    blockchain
        |> List.reverse
        |> List.head
        |> Maybe.withDefault block


showCreatedBlock : Model -> Block -> Html Msg
showCreatedBlock model block =
    let
        zone =
            model.zone

        blockchain =
            model.chain
    in
    div
        [ class "ba b--mid-gray br2 pa3 mb4" ]
        [ h3 [ class "f3 f3-m" ] [ text "last block" ]
        , blockchain
            |> getLastBlockFromChain block
            |> blockElements zone
            |> ul [ class "ml5 list pl0 tl" ]
        ]


view : Model -> Html Msg
view model =
    div [ class "pa5" ]
        [ h1 [] [ text "Simple Block chain" ]
        , showCreatedBlock model defaultBlock
        , blockDataFields model
        , validateBlockChainField model
        , renderBlockChainElements model
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
