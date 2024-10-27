module Page

open System

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
    "Hywe syntax based on above selection populates here.\n" +
    "• BEE-line  : Elementary syntax for simple layouts.\n" +
    "• BEE-yond  : Enhanced syntax for branched layouts.\n" +
    "• BEE-droom : Evolved syntax for complex layouts.\n\n" +
    "Alter the populated script to explore variations.\n"

let beeline = "(1/19/Start),(2/15/End)"

let beeyond = "(1/24/Dock),(1.1/25/Logistics),(1.2/24/Lab),"+
              "(1.3/25/Habitation),(1.4/25/Power)"

let beedroom = "(0/W=15/H=15/I=0/S=1/Q=11),"+
                "(1/7/Foyer),(2/12/Living),(3/18/Dining),(1.1/12/Study),(2.1/12/Staircase),"+
                "(3.1/15/Kitchen),(3.2/14/Bed-1),(3.3/18/Bed-2),(3.4/18/Bed-3),"+
                "(3.1.1/6/Utility),(3.2.1/8/Bath-1),(3.3.1/10/Closet-2),(3.4.1/11/Closet-3),"+
                "(3.4.2/10/Bath-3),(3.3.1.1/10/Bath-2)"

let introduction = "Weave spatial layouts at a high level of abstraction using properly " +
                    "formatted syntax in Hywe, an endogenous space planning concept " +
                    "currently undergoing its formative stages of development " + 
                    "as an early stage design interface."

// CSS Styles
let styleDrop2 = "width: 35%;
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
