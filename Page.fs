module Page

open System
open Hexel
open Shape

type Beeset = 
    | Beewhich
    | Beeline
    | Beeyond
    | Beedroom

// Random pastel color
let pastel () =
    let rand = Random()
    let mixWithWhite (color: int) = (color + 255) >>> 1
    let red = mixWithWhite (rand.Next(256))
    let green = mixWithWhite (rand.Next(256))
    let blue = mixWithWhite (rand.Next(256))
    // Ensure the values are within the 0-255 range
    let clamp value = min 255 (max 0 value)
    let red = clamp red
    let green = clamp green
    let blue = clamp blue
    $"rgba({red}, {green}, {blue}, 0.75)"

// Function to create an array of random pastel colors
let pastels (size: int) =
    Array.init size (fun _ -> pastel())
///

// Defaults
let stxInstr = 
    "Depending on your selection above, Hywe Syntax (index/size/label) will populate here.\n" +
    "Use the populated examples as reference to script your custom layouts.\n" +
    "Click on hyWEAVE to update any selection or alteration.\n\n"+
    "• BEE-line  : Hywe syntax in its simplest form.\n" +
    "• BEE-yond  : Syntax for slightly complex layouts.\n" +
    "• BEE-droom : Syntax with nested branching, for more detailed layouts.\n"

let beeline = "(1/35/Start),(2/35/End)"

let beeyond = "(1/25/Dock),(1.1/25/Logistics),(1.2/25/Lab),"+
              "(1.3/25/Habitation),(1.4/25/Power)"

let beedroom = "(#/W=25/H=15/S=5),"+
               "(1/7/Foyer),(2/12/Living),(3/18/Dining),(1.1/12/Study),"+
               "(2.1/12/Staircase),(3.1/14/Kitchen),(3.2/14/Bed-1),"+
               "(3.3/18/Bed-2),(3.4/18/Bed-3),(3.1.1/6/Utility),"+
               "(3.2.1/8/Bath-1),(3.3.1/10/Closet-2),(3.4.1/10/Closet-3),"+
               "(3.4.2/10/Bath-3),(3.3.1.1/10/Bath-2)"

let introduction = "Weave spatial layouts at a high level of abstraction using properly " +
                    "formatted syntax in Hywe, an endogenous space planning concept " +
                    "currently undergoing its formative stages of development " + 
                    "as an early stage design interface."

// CSS Styles
let styleHeader = "margin-top: 0px;
                    background: #d3d3d1; 
                    color: #363636; 
                    flex-direction: column;"

let styleLogo = "width: 100%;
                    height: 37px;
                    opacity: 1;
                    background: #363636;
                    padding-left: 5px;
                    padding-top: 5px;"

let styleTitle = "color: white;
                    font-family: 'Optima', Candara, Calibri;
                    font-size: 20px;
                    font-weight: normal;
                    padding-left: 10px;
                    padding-right: 10px;
                    padding-bottom: 7px;"

let styleIntro = "font-family: 'Optima', Candara, Calibri; 
                    font-size: 18px; 
                    color: #363636; 
                    padding-left: 12px;
                    padding-right: 10px;
                    padding-bottom: 5px;"

let styleDrop1 = "width: 75%;
                    display: block;
                    margin-left: 20px;
                    margin-right: 20px;
                    margin-top: 20px;
                    margin-bottom: 20px;
                    height: 36px;
                    font-size: 14px;
                    border: none;
                    padding: 10px 10px;
                    color: #646464;
                    border-radius: 10px;
                    text-align: center;
                    background-color: #ececec;
                    box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2);
                    font-family: 'Optima', Candara, Calibri"

let styleDrop2 = "width: 20%;
                    display: block;
                    height: 26px;
                    font-size: 12px;
                    background:#f9f9f9;
                    margin-left: 5px;
                    margin-right: 5px;
                    color: #808080;
                    padding: 5px 10px;
                    border-radius: 5px;
                    border: none;
                    text-align: center;
                    font-family: 'Optima', Candara, Calibri;"

let styleTable = "font-size: 14px;
                opacity:75%;
                border: none;
                padding: 10px 10px;
                color: #646464;
                text-align: center;
                font-family: 'Optima', Candara, Calibri"

// Border Hexels for Hywe generation
let hxlBnd 
    (sqn : Sqn)
    (bas : Hxl) = 
        let a,b,c = hxlCrd bas
        match sqn with 
        | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE 
            -> Array.append (hxlOrt sqn (hxlVld sqn (AV(a-100,b-2,c))) 200 false false) (adjacent sqn (hxlVld sqn (AV(0,0,0))))
                |> hxlUni 3
        | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW 
            -> Array.append (hxlOrt sqn (hxlVld sqn (AV(a-104,b-2,c))) 200 false false) (adjacent sqn (hxlVld sqn (AV(0,0,0))))
                |> hxlUni 3