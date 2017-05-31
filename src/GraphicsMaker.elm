port module GraphicsMaker exposing (..)

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE
import Mouse exposing (Position)
import Json.Decode as Decode

-----------
-- MODEL --
-----------

main : Program Never Model Msg
main = 
  H.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { objs : List Object
  , lastId : Int
  , drag : Maybe Drag
  }

type alias Drag =
  { start : Position
  , current : Position
  }

type Msg
   = Noop
   | AddCircle
   | AddRect
   | AddText
   | AddLine
   | AddEllipse
   | SetObjectAttribute String String
   | ObjectClick Int
   | DragStart Position Int
   | DragAt Position
   | DragEnd Position
   | SaveSVG
   | SavePNG

type alias Circle =
  { id : Int
  , cx : Int
  , cy : Int
  , r  : Int
  , fill  : String
  }

type alias Rect =
  { id     : Int
  , x      : Int
  , y      : Int
  , width  : Int
  , height : Int
  , corner : Int
  , fill  : String
  }

type alias Text = 
  { id     : Int
  , x      : Int
  , y      : Int
  , text   : String
  , font   : String
  , size   : Int
  , fill  : String
  }

type alias RadialLine = 
  { id     : Int
  , x      : Int
  , y      : Int
  , len    : Int
  , angle  : Float
  , fill  : String
  , width  : Int
  }

type alias Ellipse =
  { id : Int
  , cx : Int
  , cy : Int
  , rx  : Int
  , ry  : Int
  , fill  : String
  }

-- dummy object
-- used when we want no objects to be selected
type alias Dummy = Int
  
type Object = D Dummy | C Circle | R Rect | T Text | L RadialLine | E Ellipse

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)

initialModel : Model
initialModel =
  { objs = [D 0]
  , lastId = 1
  , drag = Nothing
  }

globals : { height : Int, width : Int }
globals = 
  { width = 600
  , height = 600
  }

------------
-- UPDATE --
------------

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> (model, Cmd.none)

    AddCircle -> addCircle model
    AddRect -> addRect model
    AddText -> addText model
    AddLine -> addLine model
    AddEllipse -> addEllipse model

    SetObjectAttribute name val ->
      let objs = setObjectAttribute model.objs name val in
      let newModel = {model | objs = objs} in
      (newModel, Cmd.none)
    
    -- bring object to front of list to select it
    ObjectClick id ->
      let objs = bringIdToFront id model.objs in
      let newModel = {model | objs = objs} in
      (newModel, Cmd.none)

    DragStart xy id ->
      let objs = bringIdToFront id model.objs in
      let newModel = {model | objs = objs, drag = (Just (Drag xy xy))} in
      (newModel, Cmd.none)
    DragAt xy ->
      let newModel = {model | drag = (Maybe.map (\{start} -> Drag start xy) model.drag)} in
      (newModel, Cmd.none)
    DragEnd xy ->
      let objs = setPosition model.objs model.drag in
      let newModel = {model | objs = objs, drag = Nothing} in
      (newModel, Cmd.none)
    
    SaveSVG ->
      -- deselect objects (by selecting "dummy" object ID 0)
      -- then send saveSVG commmand
      let objs = bringIdToFront 0 model.objs in
      let newModel = {model | objs = objs} in
      (newModel, saveSVG ())
    SavePNG ->
      -- deselect objects (by selecting "dummy" object ID 0)
      -- then send savePNG commmand
      let objs = bringIdToFront 0 model.objs in
      let newModel = {model | objs = objs} in
      (newModel, savePNG ())






-- Object creation functions

-- make a circle object with some default parameters
addCircle : Model -> (Model, Cmd Msg)
addCircle model =
  let newId = model.lastId + 1 in
  let pos = 30 * newId in
  let cir = { cx = pos, cy = pos, r = 50, fill = "#FF683C", id = newId } in
  let newModel = {model | objs = (C cir) :: model.objs
                        , lastId = newId} in
  (newModel, Cmd.none)

-- make a rectangle object with some default parameters
addRect : Model -> (Model, Cmd Msg)
addRect model =
  let newId = model.lastId + 1 in
  let pos = 30 * newId in
  let rect = { x = pos, y = pos, width = 30, height = 60, fill = "#FF683C", corner = 0, id = newId } in
  let newModel = {model | objs = (R rect) :: model.objs
                        , lastId = newId} in
  (newModel, Cmd.none)
  
addText : Model -> (Model, Cmd Msg)
addText model =
  let newId = model.lastId + 1 in
  let pos = 30 * newId in
  let text = { x = pos, y = pos, text = "Hello", size = 50, fill = "#FF683C", font = "Helvetica", id = newId } in
  let newModel = {model | objs = (T text) :: model.objs
                        , lastId = newId} in
  (newModel, Cmd.none)

addLine : Model -> (Model, Cmd Msg)
addLine model =
  let newId = model.lastId + 1 in
  let pos = 30 * newId in
  let l = { x = pos, y = pos, len = 200, angle = pi / 3, fill = "#FF683C", width = 2, id = newId } in
  let newModel = {model | objs = (L l) :: model.objs
                        , lastId = newId} in
  (newModel, Cmd.none)

addEllipse : Model -> (Model, Cmd Msg)
addEllipse model =
  let newId = model.lastId + 1 in
  let pos = 30 * newId in
  let e = { cx = pos, cy = pos, rx = 30, ry = 60, fill = "#FF683C", id = newId } in
  let newModel = {model | objs = (E e) :: model.objs
                        , lastId = newId} in
  (newModel, Cmd.none)


-- Object list manipulation functions

-- brings object with given id to front of list
bringIdToFront : Int -> List Object -> List Object
bringIdToFront id objs =
  let (x, xs) = bringIdToFrontHelper id (Nothing, objs) in
  case x of
    Nothing -> xs
    Just y -> y :: xs

bringIdToFrontHelper : Int -> (Maybe Object, List Object) -> (Maybe Object, List Object)
bringIdToFrontHelper id (obj, objs) =
  case (obj, objs) of
    (Nothing, []) -> (Nothing, [])
    (Nothing, x :: xs) ->
      case x of
        C c -> 
          if c.id == id then (Just x, xs)
          else
            let (obj, objs) = bringIdToFrontHelper id (Nothing, xs)
            in (obj, x :: objs)
        R r -> 
          if r.id == id then (Just x, xs)
          else
            let (obj, objs) = bringIdToFrontHelper id (Nothing, xs)
            in (obj, x :: objs)
        T r -> 
          if r.id == id then (Just x, xs)
          else
            let (obj, objs) = bringIdToFrontHelper id (Nothing, xs)
            in (obj, x :: objs)
        L r -> 
          if r.id == id then (Just x, xs)
          else
            let (obj, objs) = bringIdToFrontHelper id (Nothing, xs)
            in (obj, x :: objs)
        E r -> 
          if r.id == id then (Just x, xs)
          else
            let (obj, objs) = bringIdToFrontHelper id (Nothing, xs)
            in (obj, x :: objs)
        D did ->
          if id == id then (Just x, xs)
          else
            let (obj, objs) = bringIdToFrontHelper id (Nothing, xs)
            in (obj, x :: objs)
    (Just x, _) -> (Just x, objs)

getTopObject : List Object -> (Maybe Object, List Object)
getTopObject objs =
  case objs of
    [] -> Debug.log "getting top of empty model" (Nothing, [])
    o :: os -> (Just o, os)

-- Object manipulation functions



setObjectAttribute : List Object -> String -> String -> List Object
setObjectAttribute objs name val = 
  let (top, tail) = getTopObject objs in
  case top of
    Just (C c) -> 
      case name of
        "r"  -> C {c | r = (getInt val)} :: tail
        "fill" -> C {c | fill = val} :: tail
        _    -> Debug.log "setObjectAttribute failed" objs
    Just (R r) ->
      case name of
        "width" -> R {r | width = (getInt val)} :: tail
        "height" -> R {r | height = (getInt val)} :: tail
        "corner" -> R {r | corner = (getInt val)} :: tail
        "fill" -> R {r | fill = val} :: tail
        _    -> Debug.log "setObjectAttribute failed" objs
    Just (T t) ->
      case name of
        "size" -> T {t | size = (getInt val)} :: tail
        "text" -> T {t | text = val} :: tail
        "fill" -> T {t | fill = val} :: tail
        "font" -> T {t | font = val} :: tail
        _    -> Debug.log "setObjectAttribute failed" objs
    Just (L l) -> 
      case name of
        "len"  -> L {l | len = (getInt val)} :: tail
        "angle"  -> L {l | angle = (getFloat val)} :: tail
        "width"  -> L {l | width = (getInt val)} :: tail
        "fill" -> L {l | fill = val} :: tail
        _    -> Debug.log "setObjectAttribute failed" objs
    Just (E c) -> 
      case name of
        "rx"  -> E {c | rx = (getInt val)} :: tail
        "ry"  -> E {c | ry = (getInt val)} :: tail
        "fill" -> E {c | fill = val} :: tail
        _    -> Debug.log "setObjectAttribute failed" objs
    Just (D _) ->  Debug.log "setObjectAttribute failed" objs
    Nothing ->  Debug.log "setObjectAttribute failed" objs

setPosition : List Object -> Maybe Drag -> List Object
setPosition objs drag =
  case (objs, drag) of
    (_, Nothing) -> objs
    ([], _) -> objs
    (o :: os, Just {start, current}) ->
      let top = 
        case o of
          C c ->
            let 
              dx = start.x - c.cx
              dy = start.y - c.cy
            in
              C {c | cx = current.x - dx, cy = current.y - dy}
          R r ->
            let 
              dx = start.x - r.x
              dy = start.y - r.y
            in
              R {r | x = current.x - dx, y = current.y - dy}
          T r ->
            let 
              dx = start.x - r.x
              dy = start.y - r.y
            in
              T {r | x = current.x - dx, y = current.y - dy}
          L r ->
            let 
              dx = start.x - r.x
              dy = start.y - r.y
            in
              L {r | x = current.x - dx, y = current.y - dy}
          E e ->
            let 
              dx = start.x - e.cx
              dy = start.y - e.cy
            in
              E {e | cx = current.x - dx, cy = current.y - dy}
          D _ -> o
      in
        top :: os

-- Helpers

-- necessary because sliders store value as string
-- we should never hit error case
getInt : String -> Int
getInt str = 
  case (String.toInt str) of 
    Ok num -> num
    Err error -> Debug.log error 0

getFloat : String -> Float
getFloat str = 
  case (String.toFloat str) of 
    Ok num -> num
    Err error -> Debug.log error 0

-------------------
-- SUBSCRIPTIONS --
-------------------

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none
    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]

-----------
-- PORTS --
-----------

port saveSVG : () -> Cmd msg
port savePNG : () -> Cmd msg

----------
-- VIEW --
----------

-- Object generation functions
-- convert object type to SVG

makeSvgObjs : List Object -> Maybe Drag -> List (S.Svg Msg)
makeSvgObjs objs drag  =
  let 
    makeObj o selected = 
      case o of
        C c -> (makeCircle c selected)
        R r -> (makeRect r selected)
        T t -> (makeText t selected)
        L l -> (makeLine l selected)
        E e -> (makeEllipse e selected)
        D _ -> (S.g [] [])
    helper objs = 
      case objs of
        [] -> []
        o :: os -> (helper os) ++ [(makeObj o False)]
    (top, tail) = getTopObject (setPosition objs drag)
  in
    case top of
      Nothing -> Debug.log "bad" (helper tail)
      Just t ->  (helper tail) ++ [(makeObj t True)]
    
makeCircle : Circle -> Bool -> S.Svg Msg
makeCircle c selected = 
  let 
    attributesList = 
      [ SA.cx (toString c.cx)
      , SA.cy (toString c.cy)
      , SA.r (toString c.r)
      , SA.fill c.fill
      , onMouseDown c.id
      ]
    selectedList = 
      [ SA.stroke "#ABFF55"
      , SA.strokeWidth "2"
      , SA.strokeDasharray "5, 5"
      ]
  in
    if selected then
      S.circle (attributesList ++ selectedList) []  
    else
      S.circle attributesList []

makeEllipse : Ellipse -> Bool -> S.Svg Msg
makeEllipse c selected = 
  let 
    attributesList = 
      [ SA.cx (toString c.cx)
      , SA.cy (toString c.cy)
      , SA.rx (toString c.rx)
      , SA.ry (toString c.ry)
      , SA.fill c.fill
      , onMouseDown c.id
      ]
    selectedList = 
      [ SA.stroke "#ABFF55"
      , SA.strokeWidth "2"
      , SA.strokeDasharray "5, 5"
      ]
  in
    if selected then
      S.ellipse (attributesList ++ selectedList) []  
    else
      S.ellipse attributesList []

makeRect : Rect -> Bool -> S.Svg Msg
makeRect r selected = 
  let 
    attributesList = 
      [ SA.x (toString r.x)
      , SA.y (toString r.y)
      , SA.rx (toString r.corner)
      , SA.ry (toString r.corner)
      , SA.width (toString r.width)
      , SA.height (toString r.height)
      , SA.fill r.fill
      , onMouseDown r.id
      ]
    selectedList = 
      [ SA.stroke "#ABFF55"
      , SA.strokeWidth "2"
      , SA.strokeDasharray "5, 5"
      ]
  in
    if selected then
      S.rect (attributesList ++ selectedList) []  
    else
      S.rect attributesList []

makeLine : RadialLine -> Bool -> S.Svg Msg
makeLine l selected = 
  let 
    attributesList = 
      [ SA.x1 (toString l.x)
      , SA.y1 (toString l.y)
      , SA.x2 (l.x + round ((toFloat l.len) * (cos l.angle)) |> toString)
      , SA.y2 (l.y + round ((toFloat l.len) * (sin l.angle)) |> toString)
      , SA.stroke l.fill
      , SA.strokeWidth (toString l.width)
      , onMouseDown l.id
      ]
    selectedList = -- second, thicker line used to indicate selection
      [ SA.x1 (toString l.x)
      , SA.y1 (toString l.y)
      , SA.x2 (l.x + round ((toFloat l.len) * (cos l.angle)) |> toString)
      , SA.y2 (l.y + round ((toFloat l.len) * (sin l.angle)) |> toString)
      , SA.stroke "#ABFF55"
      , SA.strokeWidth (l.width + 2 |> toString)
      , onMouseDown l.id
      ]
  in
    if selected then
      S.g [] [S.line selectedList [], S.line attributesList []]  
    else
      S.line attributesList []


makeText : Text -> Bool -> S.Svg Msg
makeText t selected = 
  let 
    attributesList = 
      [ SA.x (toString t.x)
      , SA.y (toString t.y)
      , SA.fontSize (toString t.size)
      , SA.fontFamily t.font
      , SA.fill t.fill
      , onMouseDown t.id
      ]
    selectedList = 
      [ SA.stroke "#ABFF55"
      , SA.strokeWidth "1"
      ]
  in
    if selected then
      S.text_ (attributesList ++ selectedList) [S.text t.text]  
    else
      S.text_ attributesList [S.text t.text]


-- attributes pane generation functions
-- create HTML elements for sliders and dropdowns

attributesForm : Model -> H.Html Msg
attributesForm model =
  case model.objs of
    [] -> H.div [] []
    C c :: _ -> 
      H.div [] 
        [ attributesSlider "r" c.r 10 200
        , attributesColor "fill" c.fill
        ]
    R r :: _ ->
      H.div [] 
        [ attributesSlider "width" r.width 10 400
        , attributesSlider "height" r.height 10 400
        , attributesSlider "corner" r.corner 0 20
        , attributesColor "fill" r.fill
        ]
    T t :: _ ->
      H.div [] 
        [ attributesSlider "size" t.size 10 200
        , attributesText "text" t.text
        , attributesFont "font" t.font
        , attributesColor "fill" t.fill
        ]
    L l :: _ ->
      H.div [] 
        [ attributesSlider "len" l.len 50 600
        , attributesSlider "width" l.width 1 12
        , attributesSliderFloat "angle" l.angle 0 (2 * pi)
        , attributesColor "fill" l.fill
        ]
    E e :: _ -> 
      H.div [] 
        [ attributesSlider "rx" e.rx 10 200
        , attributesSlider "ry" e.ry 10 200
        , attributesColor "fill" e.fill
        ]
    D _ :: _ -> H.div [] []

attributesSlider : String -> number -> Int -> Int -> H.Html Msg
attributesSlider attribute val min max =
  H.div [ HA.class "input-group" ] 
    [ H.span 
        [ HA.class "input-group-addon" ]
        [ H.text attribute ]
    , H.input 
      [ HA.type_ "range"
      , HA.class "form-control slider-control"
      , HA.min (toString min)
      , HA.max (toString max)
      , HA.value (toString val)
      , HE.onInput (SetObjectAttribute attribute)
      ] []
    ]

attributesSliderFloat : String -> number -> number -> number -> H.Html Msg
attributesSliderFloat attribute val min max =
  H.div [ HA.class "input-group" ] 
    [ H.span 
        [ HA.class "input-group-addon" ]
        [ H.text attribute ]
    , H.input 
      [ HA.type_ "range"
      , HA.class "form-control slider-control"
      , HA.min (toString min)
      , HA.max (toString max)
      , HA.step "any"
      , HA.value (toString val)
      , HE.onInput (SetObjectAttribute attribute)
      ] []
    ]

attributesText : String -> String -> H.Html Msg
attributesText attribute val =
  H.div [HA.class "input-group"] 
    [ H.span 
        [ HA.class "input-group-addon" ]
        [ H.text attribute ]
    , H.input 
      [ HA.type_ "text"
      , HA.class "form-control"
      , HA.value val
      , HE.onInput (SetObjectAttribute attribute)
      ] []
    ]
  
attributesColor : String -> String -> H.Html Msg
attributesColor attribute val =
  H.div [HA.class "input-group"] 
    [ H.span 
        [ HA.class "input-group-addon" ]
        [ H.text attribute ]
    , H.select 
      [ HA.class "selectpicker form-control"
      , HE.onInput (SetObjectAttribute attribute)
      ]
      [ H.option [ HA.value "#000000" ][ H.text "black" ]  
      , H.option [ HA.value "#B28375" ][ H.text "brown" ]  
      , H.option [ HA.value "#ABFF55" ][ H.text "green" ]  
      , H.option [ HA.value "#FF683C", HA.selected True ][ H.text "orange" ] 
      , H.option [ HA.value "#FF0000" ][ H.text "red" ]  
      , H.option [ HA.value "#4863CC" ][ H.text "blue" ]   
      , H.option [ HA.value "#818CB2" ][ H.text "purple" ] 
      ]
    ]

attributesFont : String -> String -> H.Html Msg
attributesFont attribute val =
  let fonts = ["Helvetica", "Roboto", "Open Sans", "Source Code Pro", "Merriweather", "Times New Roman", "Comic Sans MS", "Amatic SC", "Barrio", "Kumar One" ] in
  H.div [HA.class "input-group"] 
    [ H.span 
        [ HA.class "input-group-addon" ]
        [ H.text attribute ]
    , H.select 
      [ HA.class "selectpicker form-control"
      , HE.onInput (SetObjectAttribute attribute)
      ]
      (List.map (\a -> H.option [ HA.value a ][ H.text a ]) fonts)
    ]


-- main view function
-- sets up page

view : Model -> H.Html Msg
view model = 
  H.div [HA.id "container"]
    [ 
      H.div [ HA.id "object-selector" ]
        [ 
          H.div 
            [ HA.class "btn-group-vertical" ]
            [ H.button 
              [ HA.class "btn btn-secondary"
              , HE.onClick AddCircle ] 
              [ H.text "Circle" ]
            , H.button 
              [ HA.class "btn btn-secondary"
              , HE.onClick AddEllipse ] 
              [ H.text "Ellipse" ]
            , H.button 
              [ HA.class "btn btn-secondary"
              , HE.onClick AddRect ] 
              [ H.text "Rectangle" ]
            , H.button 
              [ HA.class "btn btn-secondary"
              , HE.onClick AddLine ] 
              [ H.text "Line" ]
            , H.button 
              [ HA.class "btn btn-secondary"
              , HE.onClick AddText ] 
              [ H.text "Text" ]
            ],
          H.div [ HA.class "btn-group-vertical save-buttons" ]
            [ H.button 
                [ HA.class "btn btn-secondary save-button"
                , HE.onClick SaveSVG ] 
                [ H.text "Save SVG" ]
            , H.button 
                [ HA.class "btn btn-secondary save-button"
                , HE.onClick SavePNG ] 
                [ H.text "Save PNG" ]
            ]
        ],
      H.div [HA.id "image"]
        [ S.svg
            [ HA.attribute "xmlns" "http://www.w3.org/2000/svg"
            , SA.version "1.1"
            , SA.id "svg"
            , SA.width (toString globals.width)
            , SA.height (toString globals.height)
            ]
            (S.defs [] 
              [ S.style []
                [ S.text "@import url('https://fonts.googleapis.com/css?family=Amatic+SC|Barrio|Inconsolata|Kumar+One|Merriweather|Open+Sans|Roboto|Source+Code+Pro|Ubuntu');"]]
              :: (makeSvgObjs model.objs model.drag))
        ]
      , H.div [HA.id "attribute-editor"]
        [
          attributesForm model
        ]
      ]

onMouseDown : Int -> H.Attribute Msg
onMouseDown id =
  HE.on "mousedown" (Decode.map (\pos -> DragStart pos id) Mouse.position)