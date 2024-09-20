module Content

open Coxel
open Bolero.Html

type Beeset = 
    | Beewhich
    | Beeline
    | Beeyond
    | Beedroom

let introduction1= "Weave spatial layouts at a high level of abstraction using properly " +
                    "formatted syntax in Hywe, an endogenous space planning concept " +
                    "currently undergoing its formative stages of development " + 
                    "as an early stage design interface."

// Sample Syntax
let beeline = "(1/48/Start),(2/52/End)"

let beeyond = "(1/25/Dock),(1.1/25/Logistics),(1.2/25/Lab),"+
              "(1.3/25/Habitation),(1.4/25/Power)"

let beedroom = "(1/7/Foyer),(2/12/Living),(3/18/Dining),(1.1/12/Study),"+
                "(2.1/12/Staircase),(3.1/14/Kitchen),(3.2/14/Bed-1),"+
                "(3.3/18/Bed-2),(3.4/18/Bed-3),(3.1.1/6/Utility),"+
                "(3.2.1/8/Bath-1),(3.3.1/10/Closet-2),(3.4.1/10/Closet-3),"+
                "(3.4.2/10/Bath-3),(3.3.1.1/10/Bath-2)"

// Text box instructions
let stxInstr = 
    "Depending on your selection above, Hywe Syntax (index/size/label) will populate here.\n" +
    "Use the populated examples as reference to script your custom layouts.\n" +
    "Click on hyWEAVE to update any selection or alteration.\n\n"+
    "• BEE-line  : Hywe syntax in its simplest form.\n" +
    "• BEE-yond  : Syntax for slightly complex layouts.\n" +
    "• BEE-droom : Syntax with nested branching, for more detailed layouts.\n"

// CSS
let styleHeader1 = "margin-top: 0px;
                    background: #d3d3d1; 
                    color: #363636; 
                    flex-direction: column;"

let styleDropdown1 = "width: 75%;
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

let styleLabel1 = "width: 20%;
                    display: flex;
                    height: 26px;
                    font-size: 12px;
                    background:#f9f9f9;
                    margin-left: 5px;
                    margin-right: 5px;
                    padding: 5px 10px;
                    border-radius: 5px;
                    border: none;
                    text-align: center;
                    color: #808080;
                    font-family: 'Optima', Candara, Calibri;"

let styleTable1 = "font-size: 14px;
                    opacity:75%;
                    border: none;
                    padding: 10px 10px;
                    color: #646464;
                    text-align: center;
                    font-family: 'Optima', Candara, Calibri"

let styleNameLogo1 = "width: 100%;
                        height: 37px;
                        opacity: 1;
                        background: #363636;
                        padding-left: 5px;
                        padding-top: 5px;"

let styleTitle1 ="color: white;
                    font-family: 'Optima', Candara, Calibri;
                    font-size: 20px;
                    font-weight: normal;
                    padding-left: 10px;
                    padding-right: 10px;
                    padding-bottom: 7px;"

let styleIntroduction1 = "font-family: 'Optima', Candara, Calibri; 
                                font-size: 18px; 
                                color: #363636; 
                                padding-left: 12px;
                                padding-right: 10px;
                                padding-bottom: 5px;"

// Header
let header = 
        div{
            attr.``style`` styleHeader1
            // Name and Logo
            div{
                attr.``class`` "flex-container"
                attr.``style`` styleNameLogo1
                // Logo
                a{
                    attr.href "https://github.com/vykrum/Hywe"
                    attr.target "blank"
                    img{
                        attr.width "30"
                        attr.height "30"
                        attr.src "https://vykrum.github.io/Hywe/favicon-32x32.png"
                    }
                }
                // Title
                div{
                    attr.``style`` styleTitle1
                    " H Y W E"
                    }
                // Acronym
                div{
                    img{
                    attr.width "200"
                    attr.height "45"
                    attr.src "https://vykrum.github.io/Hywe/hyweLogoAcronym.png"
                    }
                }
                }
            // Introduction
            div{
                attr.``style`` styleIntroduction1
                p{
                    introduction1
                }    
            }
        }

// Table
let table cxCxl1 cxClr1 cxlAvl = 
    div{
                attr.``class`` "flex-container"
                attr.``style`` "flex-wrap: wrap; justify-content: center;"
                table{
                    attr.width "95%"
                    attr.``style`` styleTable1
                    thead{
                            tr{
                                th{"Index"}
                                th{"Label"}
                                th{"Required"}
                                th{"Achieved"}
                                th{"Open"}
                            }
                    }
                    tbody{
                        for cxl in (Array.zip3 cxCxl1 cxClr1 cxlAvl) do
                            tr{
                                let (fs,sn,th) = cxl
                                attr.``style`` $"background-color:{sn}"
                                
                                let reqSz = 
                                    match (prpVlu fs.Rfid) with
                                    | "1" -> ((prpVlu fs.Size |> int) + 1).ToString()
                                    | _ -> prpVlu fs.Size

                                let achSz = Array.length fs.Hxls

                                let achCl =
                                    match achSz < (reqSz|>int) with
                                    | true -> "red"
                                    | false -> "#646464"

                                let achVtCl =                                     
                                        match th < 1 with
                                        | true -> "red"
                                        | false -> "#646464"

                                td{
                                    attr.width "15%"
                                    attr.``style`` "
                                        padding: 5px 10px;
                                        text-align: center;"
                                    prpVlu fs.Rfid
                                    }
                                td{
                                    attr.width "35%"
                                    attr.``style`` "
                                        padding: 5px 10px;
                                        text-align: center;"
                                    prpVlu fs.Name
                                    }
                                td{
                                    attr.width "15%"
                                    attr.``style`` "
                                        padding: 5px 10px;
                                        text-align: center;"
                                    reqSz
                                    }
                                td{
                                attr.width "15%"
                                attr.``style`` $"
                                    padding: 5px 10px;
                                    text-align: center;
                                    color:{achCl};"
                                $"{achSz}"
                                }
                                td{
                                attr.width "15%"
                                attr.``style`` $"
                                    padding: 5px 10px;
                                    text-align: center;
                                    color:{achVtCl};"
                                $"{th}"
                                }
                            }
                    }
                }
            }