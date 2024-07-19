module Hywe.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Hexel
open Coxel
open Shape
open Bridge
open Parse

type Model =
    {
        shp1 : int
        sqn1 : int
        scl1 : int
        cxl0001 : int
        cxl0002 : int
        cxl0003 : int
        cxl0011 : int
        cxl0021 : int
        cxl0031 : int
        cxl0032 : int
        cxl0033 : int
        cxl0034 : int
        cxl0311 : int
        cxl0321 : int
        cxl0331 : int
        cxl0341 : int
        cxl0342 : int
        cxl3311 : int
        lbl0001 : string
        lbl0002 : string
        lbl0003 : string
        lbl0011 : string
        lbl0021 : string
        lbl0031 : string
        lbl0032 : string
        lbl0033 : string
        lbl0034 : string
        lbl0311 : string
        lbl0321 : string
        lbl0331 : string
        lbl0341 : string
        lbl0342 : string
        lbl3311 : string
        spcStr1 : string
        spcStr2 : string
    }

// Default Input
let initModel =
    {
        shp1 = 4
        sqn1 = 13
        scl1 = 10
        cxl0001 = 7
        cxl0002 = 12
        cxl0003 = 12
        cxl0011 = 10
        cxl0021 = 10
        cxl0031 = 12
        cxl0032 = 14
        cxl0033 = 18
        cxl0034 = 18
        cxl0311 = 6
        cxl0321 = 8
        cxl0331 = 10
        cxl0341 = 10
        cxl0342 = 10
        cxl3311 = 10
        lbl0001 = "Foyer"
        lbl0002 = "Living"
        lbl0003 = "Dining"
        lbl0011 = "Study"
        lbl0021 = "Staircase"
        lbl0031 = "Kitchen."
        lbl0032 = "Bed."
        lbl0033 = "Bed."
        lbl0034 = "Bed"
        lbl0311 = "Utility"
        lbl0321 = "Bath"
        lbl0331 = "Closet"
        lbl0341 = "Closet"
        lbl0342 = "Bath"
        lbl3311 = "Bath"
        spcStr1 = "(1/7/Foyer),(2/12/Living),(3/12/Dining),(1.1/10/Study),(2.1/10/Staircase),(3.1/12/Kitchen),(3.2/14/Bed-1),(3.3/18/Bed-2),(3.4/18/Bed-3),(3.1.1/6/Utility),(3.2.1/8/Bath-1),(3.3.1/10/Closet-2),(3.4.1/10/Closet-3),(3.4.2/10/Bath-3),(3.3.1.1/10/Bath-2)"
        spcStr2 = "(1/7/Foyer),(2/12/Living),(3/12/Dining),(3.1/12/Kitchen),(3.2/14/Bed-1),(3.3/18/Bed-2),(3.1.1/6/Utility),(3.2.1/10/Closet-1),(3.3.1/10/Closet-2),(3.3.2/10/Bath-2),(3.2.1.1/10/Bath-1)"
    }

type Message =
    | SetShp1 of int
    | SetSqn1 of int
    | SetScl1 of int
    | SetCxl0001 of int
    | SetCxl0002 of int
    | SetCxl0003 of int
    | SetCxl0011 of int
    | SetCxl0021 of int
    | SetCxl0031 of int
    | SetCxl0032 of int
    | SetCxl0033 of int
    | SetCxl0034 of int
    | SetCxl0311 of int
    | SetCxl0321 of int
    | SetCxl0331 of int
    | SetCxl0341 of int
    | SetCxl0342 of int
    | SetCxl3311 of int
    | SetLbl0001 of string
    | SetLbl0002 of string
    | SetLbl0003 of string
    | SetLbl0011 of string
    | SetLbl0021 of string
    | SetLbl0031 of string
    | SetLbl0032 of string
    | SetLbl0033 of string
    | SetLbl0034 of string
    | SetLbl0311 of string
    | SetLbl0321 of string
    | SetLbl0331 of string
    | SetLbl0341 of string
    | SetLbl0342 of string
    | SetLbl3311 of string
    | SetSpcStr1 of string
    | SetSpcStr2

let update message model =
    match message with
    | SetSqn1 value -> { model with sqn1 = value }
    | SetShp1 value -> { model with shp1 = value }
    | SetScl1 value -> { model with scl1 = value }
    | SetCxl0001 value -> { model with cxl0001 = value }
    | SetCxl0002 value -> { model with cxl0002 = value }
    | SetCxl0003 value -> { model with cxl0003 = value }
    | SetCxl0011 value -> { model with cxl0011 = value }
    | SetCxl0021 value -> { model with cxl0021 = value }
    | SetCxl0031 value -> { model with cxl0031 = value }
    | SetCxl0032 value -> { model with cxl0032 = value }
    | SetCxl0033 value -> { model with cxl0033 = value }
    | SetCxl0034 value -> { model with cxl0034 = value }
    | SetCxl0311 value -> { model with cxl0311 = value }
    | SetCxl0321 value -> { model with cxl0321 = value }
    | SetCxl0331 value -> { model with cxl0331 = value }
    | SetCxl0341 value -> { model with cxl0341 = value }
    | SetCxl0342 value -> { model with cxl0342 = value }
    | SetCxl3311 value -> { model with cxl3311 = value }
    | SetLbl0001 value -> { model with lbl0001 = value }
    | SetLbl0002 value -> { model with lbl0002 = value }
    | SetLbl0003 value -> { model with lbl0003 = value }
    | SetLbl0011 value -> { model with lbl0011 = value }
    | SetLbl0021 value -> { model with lbl0021 = value }
    | SetLbl0031 value -> { model with lbl0031 = value }
    | SetLbl0032 value -> { model with lbl0032 = value }
    | SetLbl0033 value -> { model with lbl0033 = value }
    | SetLbl0034 value -> { model with lbl0034 = value }
    | SetLbl0311 value -> { model with lbl0311 = value }
    | SetLbl0321 value -> { model with lbl0321 = value }
    | SetLbl0331 value -> { model with lbl0331 = value }
    | SetLbl0341 value -> { model with lbl0341 = value }
    | SetLbl0342 value -> { model with lbl0342 = value }
    | SetLbl3311 value -> { model with lbl3311 = value }
    | SetSpcStr1 value -> { model with spcStr1 = value }
    | SetSpcStr2 -> { model with spcStr2 = model.spcStr1}

// Interface
let view model dispatch =      
    concat {
        // Header
        div{
            attr.``style`` "margin-top: 0px;
                            background: #d3d3d1; 
                            color: #363636; 
                            flex-direction: column;"
            // Name and Logo
            div{
                attr.``class`` "flex-container"
                attr.``style`` "width: 100%;
                                height: 37px;
                                opacity: 1;
                                background: #363636;
                                padding-left: 5px;
                                padding-top: 5px;"
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
                    attr.``style`` "color: white;
                                    font-family: 'Optima', Candara, Calibri;
                                    font-size: 20px;
                                    font-weight: normal;
                                    padding-left: 10px;
                                    padding-right: 10px;
                                    padding-bottom: 7px;"
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
                attr.``style`` "font-family: 'Optima', Candara, Calibri; 
                                font-size: 18px; 
                                color: #363636; 
                                padding-left: 12px;
                                padding-right: 10px;
                                padding-bottom: 5px;"
                p{
                    "Hywe is an endogenous space planning concept currently undergoing its formative stages of development as an early stage design interface."
                    br
                    "Weave spatial layouts at a high level of abstraction using properly formatted (heirarchy/size/label) instructions."
                }
            }
        }

        // Scale
        let scl = model.scl1
        
        // Shape
        let shp = match model.shp1 with 
                    | 1 -> Sqr
                    | 2 -> Arw
                    | 3 -> Prl
                    | _ -> Hxg

        // Sequence
        let sqn = match model.sqn1 with 
                    | 1 -> VRCWEE 
                    | 2 -> VRCCEE    
                    | 3 -> VRCWSE
                    | 4 -> VRCCSE
                    | 5 -> VRCWSW
                    | 6 -> VRCCSW
                    | 7 -> VRCWWW
                    | 8 -> VRCCWW
                    | 9 -> VRCWNW
                    |10 -> VRCCNW
                    |11 -> VRCWNE
                    |12 -> VRCCNE
                    |13 -> HRCWNN
                    |14 -> HRCCNN
                    |15 -> HRCWNE
                    |16 -> HRCCNE
                    |17 -> HRCWSE
                    |18 -> HRCCSE
                    |19 -> HRCWSS
                    |20 -> HRCCSS
                    |21 -> HRCWSW
                    |22 -> HRCCSW
                    |23 -> HRCWNW
                    |24 -> HRCCNW
                    | _ -> VRCWEE

        // Nested Coxel Colors
        let cxClr = [|   
            "#FDF0C1"
            "#85C1E9"
            "#C6BDB4"
            "#BCCFD3"
            "#FADBD8"
            "#C7A27C"
            "#D4EFDF"
            "#F5CBA7"
            "#EBDEF0"
            "#AED6F1"
            "#D0CEBA"
            "#F0B27A"
            "#8FBC8F"
            "#ADBBB2"
            "#D6EAF8"
                   |]

        // Nested Coxels Basic Framework
        let spaceStr = $"(1/{model.cxl0001}/{model.lbl0001}),(2/{model.cxl0002}/{model.lbl0002}),(3/{model.cxl0003}/{model.lbl0003}),(1.1/{model.cxl0011}/{model.lbl0011}),(2.1/{model.cxl0021}/{model.lbl0021}),(3.1/{model.cxl0031}/{model.lbl0031},(3.2/{model.cxl0032}/{model.lbl0032},(3.3/{model.cxl0033}/{model.lbl0033},(3.4/{model.cxl0034}/{model.lbl0034}),(3.1.1/{model.cxl0311}/{model.lbl0311}),(3.2.1/{model.cxl0321}/{model.lbl0321}),(3.3.1/{model.cxl0331}/{model.lbl0331}),(3.4.1/{model.cxl0341}/{model.lbl0341}),(3.4.2/{model.cxl0342}/{model.lbl0342}),(3.3.1.1/{model.cxl3311}/{model.lbl3311})"
        let bsNs = hxlVld sqn (AV(1,4,0))
        let bsOc = 
            match sqn with 
            | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE 
                -> let a,b,c = hxlCrd bsNs
                   Array.append (hxlOrt sqn (hxlVld sqn (AV(a-100,b-2,c))) 200 false) (adjacent sqn (hxlVld sqn (AV(0,0,0))))
                   |> allAV true
            | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW 
                -> let a,b,c = hxlCrd bsNs
                   Array.append (hxlOrt sqn (hxlVld sqn (AV(a-104,b-2,c))) 200 false) (adjacent sqn (hxlVld sqn (AV(0,0,0))))
                   |> allAV true
        let cxCxl = spaceCxl sqn bsNs bsOc spaceStr

        // Universal Controls
        div{
            attr.``class`` "flex-container"
            attr.``style`` "flex-wrap: wrap; 
                            justify-content: center;
                            display: flex;
                            flex-direction: column;"
            
            // Scale
            div{
                attr.``class`` "flex-container"
                attr.``style`` "flex-wrap: wrap;
                                display: flex;
                                justify-content: center;"
                // Scale Label
                div{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` "background:#d3d3d1;"
                    " Scale "
                }
                // Scale Value
                div{
                    attr.``class`` "label"
                    attr.``style`` "background:#d3d3d1;"
                    attr.``type`` "text"
                    $"{model.scl1}" 
                }
                // Slider Scale
                input{
                    attr.``style`` "background:#d3d3d1;"
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.min "1"
                    attr.max "25"
                    bind.input.int model.scl1 (fun a -> dispatch (SetScl1 a))
                }
            }
            // Shape
            div{
                attr.``class`` "flex-container"
                attr.``style`` "flex-wrap: wrap;
                                display: flex;
                                justify-content: center;"
                // Shape Label
                div{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` "background:#d3d3d1;"
                    "Shape"
                }
                // Shape Value
                div{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` "background:#d3d3d1;"
                    $"{shp}" 
                }
                // Slider Shape
                input{
                    attr.``style`` $"background:#d3d3d1;"
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.min "1"
                    attr.max "4"
                    bind.input.int model.shp1 (fun a -> dispatch (SetShp1 a))
                }
            }
            // Sequence
            div{
                attr.``class`` "flex-container"
                attr.``style`` "flex-wrap: wrap;
                                display: flex;
                                justify-content: center;"
                // Sequence Label
                div{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` "background:#d3d3d1;"
                    " Sequence "
                }
                // Sequence Value
                div{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` "background: #d3d3d1;"
                    $"{sqn}"
                }
                // Slider Sequence
                input{
                    attr.``style`` "background: #d3d3d1;"
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.min "1"
                    attr.max "24"
                    bind.input.int model.sqn1 (fun a -> dispatch (SetSqn1 a))
                }  
            }
        }  

        // Formatting Instructions
        div{
            attr.``class`` "flex-container"
            attr.``style`` "flex-wrap: wrap; 
                            justify-content: right;
                            display: flex;
                            flex-direction: row;
                            width: 90%;
                            margin-left: 20px;
                            margin-right: 20px;"
            a{
                attr.href "https://github.com/vykrum/Hywe/wiki/Hywe-Syntax"
                attr.target "blank"
                img{
                    attr.width "20"
                    attr.height "20"
                    attr.src "https://vykrum.github.io/Hywe/help.png"
                }
            }
        }

        // Input String
        div{
            attr.``class`` "flex-container"
            attr.``style`` "flex-wrap: wrap; 
                            justify-content: center;
                            display: flex;
                            flex-direction: row;"
            
            textarea {
                attr.``type`` "textarea"
                attr.``class`` "textarea"
                attr.``style`` "width: 95%;
                                margin-left: 20px;
                                margin-right: 20px;
                                height:100px;
                                font-size: 14px;"
                bind.change.string model.spcStr1 (fun a -> dispatch (SetSpcStr1 a))
            }
            button {
                attr.``class`` "button1"
                attr.``style`` "
                                width: 95%;
                                margin-left: 20px;
                                margin-right: 20px;
                                margin-top: 5px;"
                on.click (fun _ -> dispatch (SetSpcStr2))
                "h y W E A V E"
            }
            div{
                attr.``class`` "flex-container"
                attr.``style`` "flex-wrap: wrap; justify-content: center;"
                let cxCxl1 = spaceCxl sqn bsNs bsOc model.spcStr2
                let cxClr1 = pastels (Array.length cxCxl1)
                nstdCxls cxCxl1 cxClr1 scl shp 1200 
            }  
        } 
  
      
        // Text Block Description 
        div{
            attr.``style`` "margin-top: 5px;
                            margin-left: 20px;
                            margin-right: 20px;
                            background: #d3d3d1; 
                            color: #363636; 
                            flex-direction: column;
                            border-radius: 5px;"
            p{
                attr.``style`` "font-family: 'Optima', Candara, Calibri; 
                                font-size: 14px; 
                                color: #363636; 
                                padding-left: 20px;
                                padding-right: 20px;
                                padding-top: 10px;
                                padding-bottom: 10px;"

                "Manipulating the color coded sliders controls the scale of corresponding clusters. The period count in hierarchy labels increases corresponding to depth of branch  "
            }
        }

         // Sample Nested Coxels
        div{
            attr.``class`` "flex-container"
            attr.``style`` "flex-wrap: wrap; justify-content: center;"
            
            // Coxel SVG
            span{
                attr.``style`` "flex-wrap: wrap; justify-content: center;"
                nstdCxls cxCxl cxClr scl shp 400
            }
            // Nested Coxel Parameters
            span{
                attr.``class`` "label1"
                attr.``style`` $"background:{cxClr[0]}; 
                                border:none; 
                                width:525px; 
                                margin-left:10px;
                                margin-right:10px;
                                margin-top: 20px; 
                                padding-left:10px;"
                let minSld = "0"
                let maxSld = "25"
                // Label 1
                input{
                        attr.``type`` "text"
                        attr.``class`` "label, input1"
                        attr.``style`` $"background:{cxClr[0]};"
                        bind.input.string model.lbl0001 (fun a -> dispatch (SetLbl0001 a))
                    }
                // Refid 1
                label{
                        attr.``type`` "text"
                        attr.``class`` "label"
                        attr.``style`` "test-align: right;"
                        " [ 1 ] "
                        }
                // Slider 1
                input{
                    attr.``class`` "slider1"
                    attr.``type`` "range"
                    attr.min $"{minSld}"
                    attr.max $"{maxSld}"
                    bind.input.int model.cxl0001 (fun a -> dispatch (SetCxl0001 a))
                }
                // Count 1
                label{
                        attr.``type`` "text"
                        attr.``class`` "label"
                        $"{model.cxl0001}"
                }
            
                div{
                    attr.``class`` "label1"
                    attr.``style`` $"background:{cxClr[2]};"
                    // Label 2
                    input{
                            attr.``type`` "text"
                            attr.``class`` "label, input1"
                            attr.``style`` $"background:{cxClr[2]};"
                            bind.input.string model.lbl0002 (fun a -> dispatch (SetLbl0002 a))
                        }
                    // Refid 2
                    label{
                            attr.``type`` "text"
                            attr.``class`` "label"
                            attr.``style`` "test-align: right;"
                            " [ 2 ] "
                            }
                    // Slider 2
                    input{
                        attr.``class`` "slider1"
                        attr.``type`` "range"
                        attr.min $"{minSld}"
                        attr.max $"{maxSld}"
                        bind.input.int model.cxl0002 (fun a -> dispatch (SetCxl0002 a))
                    }
                    // Count 2
                    label{
                            attr.``type`` "text"
                            attr.``class`` "label"
                            $"{model.cxl0002}"
                    }
                    div{
                        attr.``class`` "label1"
                        attr.``style`` $"background:{cxClr[4]};"
                        // Label 3
                        input{
                                attr.``type`` "text"
                                attr.``class`` "label, input1"
                                attr.``style`` $"background:{cxClr[4]};"
                                bind.input.string model.lbl0003 (fun a -> dispatch (SetLbl0003 a))
                            }
                        // Refid 3
                        label{
                                attr.``type`` "text"
                                attr.``class`` "label"
                                attr.``style`` "test-align: right;"
                                " [ 3 ] "
                                }
                        // Slider 3
                        input{
                            attr.``class`` "slider1"
                            attr.``type`` "range"
                            attr.min $"{minSld}"
                            attr.max $"{maxSld}"
                            bind.input.int model.cxl0003 (fun a -> dispatch (SetCxl0003 a))
                        }
                        // Count 3
                        label{
                                attr.``type`` "text"
                                attr.``class`` "label"
                                $"{model.cxl0003}"
                        }
                        div{
                            attr.``class`` "label1"
                            attr.``style`` $"background:{cxClr[5]};"
                            // Label 3.1
                            input{
                                    attr.``type`` "text"
                                    attr.``class`` "label, input1"
                                    attr.``style`` $"background:{cxClr[5]};"
                                    bind.input.string model.lbl0031 (fun a -> dispatch (SetLbl0031 a))
                                }
                            // Refid 3.1
                            label{
                                    attr.``type`` "text"
                                    attr.``class`` "label"
                                    attr.``style`` "test-align: right;"
                                    " [ 3.1 ] "
                                    }
                            // Slider 3.1
                            input{
                                attr.``class`` "slider1"
                                attr.``type`` "range"
                                attr.min $"{minSld}"
                                attr.max $"{maxSld}"
                                bind.input.int model.cxl0031 (fun a -> dispatch (SetCxl0031 a))
                            }
                            // Count 3.1
                            label{
                                    attr.``type`` "text"
                                    attr.``class`` "label"
                                    $"{model.cxl0031}"
                            }
                            div{
                                attr.``class`` "label1"
                                attr.``style`` $"background:{cxClr[9]};"
                                // Label 3.1.1
                                input{
                                        attr.``type`` "text"
                                        attr.``class`` "label, input1"
                                        attr.``style`` $"background:{cxClr[9]};"
                                        bind.input.string model.lbl0311 (fun a -> dispatch (SetLbl0311 a))
                                    }
                                // Refid 3.1.1
                                label{
                                        attr.``type`` "text"
                                        attr.``class`` "label"
                                        attr.``style`` "test-align: right;"
                                        " [ 3.1.1 ] "
                                        }
                                // Slider 3.1.1
                                input{
                                    attr.``class`` "slider1"
                                    attr.``type`` "range"
                                    attr.min $"{minSld}"
                                    attr.max $"{maxSld}"
                                    bind.input.int model.cxl0311 (fun a -> dispatch (SetCxl0311 a))
                                }
                                // Count 3.1.1
                                label{
                                        attr.``type`` "text"
                                        attr.``class`` "label"
                                        $"{model.cxl0311}"
                                }
                            }
                        }
                        div{
                            attr.``class`` "label1"
                            attr.``style`` $"background:{cxClr[6]};"
                            // Label 3.2
                            input{
                                    attr.``type`` "text"
                                    attr.``class`` "label, input1"
                                    attr.``style`` $"background:{cxClr[6]};"
                                    bind.input.string model.lbl0032 (fun a -> dispatch (SetLbl0032 a))
                                }
                            // Refid 3.2
                            label{
                                    attr.``type`` "text"
                                    attr.``class`` "label"
                                    attr.``style`` "test-align: right;"
                                    " [ 3.2 ] "
                                    }
                            // Slider 3.2
                            input{
                                attr.``class`` "slider1"
                                attr.``type`` "range"
                                attr.min $"{minSld}"
                                attr.max $"{maxSld}"
                                bind.input.int model.cxl0032 (fun a -> dispatch (SetCxl0032 a))
                            }
                            // Count 3.2
                            label{
                                    attr.``type`` "text"
                                    attr.``class`` "label"
                                    $"{model.cxl0032}"
                            }
                            div{
                                attr.``class`` "label1"
                                attr.``style`` $"background:{cxClr[10]};"
                                // Label 3.2.1
                                input{
                                        attr.``type`` "text"
                                        attr.``class`` "label, input1"
                                        attr.``style`` $"background:{cxClr[10]};"
                                        bind.input.string model.lbl0321 (fun a -> dispatch (SetLbl0321 a))
                                    }
                                // Refid 3.2.1
                                label{
                                        attr.``type`` "text"
                                        attr.``class`` "label"
                                        attr.``style`` "test-align: right;"
                                        " [ 3.2.1 ] "
                                        }
                                // Slider 3.2.1
                                input{
                                    attr.``class`` "slider1"
                                    attr.``type`` "range"
                                    attr.min $"{minSld}"
                                    attr.max $"{maxSld}"
                                    bind.input.int model.cxl0321 (fun a -> dispatch (SetCxl0321 a))
                                }
                                // Count 3.2.1
                                label{
                                        attr.``type`` "text"
                                        attr.``class`` "label"
                                        $"{model.cxl0321}"
                                }
                            }
                        }
                        div{
                            attr.``class`` "label1"
                            attr.``style`` $"background:{cxClr[7]};"
                            // Label 3.3
                            input{
                                    attr.``type`` "text"
                                    attr.``class`` "label, input1"
                                    attr.``style`` $"background:{cxClr[7]};"
                                    bind.input.string model.lbl0033 (fun a -> dispatch (SetLbl0033 a))
                                }
                            // Refid 3.3
                            label{
                                    attr.``type`` "text"
                                    attr.``class`` "label"
                                    attr.``style`` "test-align: right;"
                                    " [ 3.3 ] "
                                    }
                            // Slider 3.3
                            input{
                                attr.``class`` "slider1"
                                attr.``type`` "range"
                                attr.min $"{minSld}"
                                attr.max $"{maxSld}"
                                bind.input.int model.cxl0033 (fun a -> dispatch (SetCxl0033 a))
                            }
                            // Count 3.3
                            label{
                                    attr.``type`` "text"
                                    attr.``class`` "label"
                                    $"{model.cxl0033}"
                            }
                            div{
                                attr.``class`` "label1"
                                attr.``style`` $"background:{cxClr[11]};"
                                // Label 3.3.1
                                input{
                                        attr.``type`` "text"
                                        attr.``class`` "label, input1"
                                        attr.``style`` $"background:{cxClr[11]};"
                                        bind.input.string model.lbl0331 (fun a -> dispatch (SetLbl0331 a))
                                    }
                                // Refid 3.3.1
                                label{
                                        attr.``type`` "text"
                                        attr.``class`` "label"
                                        attr.``style`` "test-align: right;"
                                        " [ 3.3.1 ] "
                                        }
                                // Slider 3.3.1
                                input{
                                    attr.``class`` "slider1"
                                    attr.``type`` "range"
                                    attr.min $"{minSld}"
                                    attr.max $"{maxSld}"
                                    bind.input.int model.cxl0331 (fun a -> dispatch (SetCxl0331 a))
                                }
                                // Count 3.3.1
                                label{
                                        attr.``type`` "text"
                                        attr.``class`` "label"
                                        $"{model.cxl0331}"
                                }
                                div{
                                    attr.``class`` "label1"
                                    attr.style $"background:{cxClr[12]};"
                                    // Label 3.3.1.1
                                    input{
                                            attr.``type`` "text"
                                            attr.``class`` "label, input1"
                                            attr.style $"background:{cxClr[12]};"
                                            bind.input.string model.lbl3311 (fun a -> dispatch (SetLbl3311 a))
                                        }
                                    // Refid 3.3.1.1
                                    label{
                                            attr.``type`` "text"
                                            attr.``class`` "label"
                                            attr.``style`` "test-align: right;"
                                            " [ 3.3.1.1 ] "
                                            }
                                    // Slider 3.3.1.1
                                    input{
                                        attr.``class`` "slider1"
                                        attr.``type`` "range"
                                        attr.min $"{minSld}"
                                        attr.max $"{maxSld}"
                                        bind.input.int model.cxl3311 (fun a -> dispatch (SetCxl3311 a))
                                    }
                                    // Count 3.3.1.1
                                    label{
                                            attr.``type`` "text"
                                            attr.``class`` "label"
                                            $"{model.cxl3311}"
                                    }
                                }
                            }
                        }
                        div{
                            attr.``class`` "label1"
                            attr.``style`` $"background:{cxClr[8]};"
                            // Label 3.4
                            input{
                                    attr.``type`` "text"
                                    attr.``class`` "label, input1"
                                    attr.``style`` $"background:{cxClr[8]};"
                                    bind.input.string model.lbl0034 (fun a -> dispatch (SetLbl0034 a))
                                }
                            // Refid 3.4
                            label{
                                    attr.``type`` "text"
                                    attr.``class`` "label"
                                    attr.``style`` "test-align: right;"
                                    " [ 3.4 ] "
                                    }
                            // Slider 3.4
                            input{
                                attr.``class`` "slider1"
                                attr.``type`` "range"
                                attr.min $"{minSld}"
                                attr.max $"{maxSld}"
                                bind.input.int model.cxl0034 (fun a -> dispatch (SetCxl0034 a))
                            }
                            // Count 3.4
                            label{
                                    attr.``type`` "text"
                                    attr.``class`` "label"
                                    $"{model.cxl0034}"
                            }
                            div{
                                attr.``class`` "label1"
                                attr.``style`` $"background:{cxClr[13]};"
                                // Label 3.4.1
                                input{
                                        attr.``type`` "text"
                                        attr.``class`` "label, input1"
                                        attr.``style`` $"background:{cxClr[13]};"
                                        bind.input.string model.lbl0341 (fun a -> dispatch (SetLbl0341 a))
                                    }
                                // Refid 3.4.1
                                label{
                                        attr.``type`` "text"
                                        attr.``class`` "label"
                                        attr.``style`` "test-align: right;"
                                        " [ 3.4.1 ] "
                                        }
                                // Slider 3.4.1
                                input{
                                    attr.``class`` "slider1"
                                    attr.``type`` "range"
                                    attr.min $"{minSld}"
                                    attr.max $"{maxSld}"
                                    bind.input.int model.cxl0341 (fun a -> dispatch (SetCxl0341 a))
                                }
                                // Count 3.4.1
                                label{
                                        attr.``type`` "text"
                                        attr.``class`` "label"
                                        $"{model.cxl0341}"
                                }
                            }
                            div{
                                attr.``class`` "label1"
                                attr.``style`` $"background:{cxClr[14]};"
                                // Label 3.4.2
                                input{
                                        attr.``type`` "text"
                                        attr.``class`` "label, input1"
                                        attr.``style`` $"background:{cxClr[14]};"
                                        bind.input.string model.lbl0342 (fun a -> dispatch (SetLbl0342 a))
                                    }
                                // Refid 3.4.2
                                label{
                                        attr.``type`` "text"
                                        attr.``class`` "label"
                                        attr.``style`` "test-align: right;"
                                        " [ 3.4.2 ] "
                                        }
                                // Slider 3.4.2
                                input{
                                    attr.``class`` "slider1"
                                    attr.``type`` "range"
                                    attr.min $"{minSld}"
                                    attr.max $"{maxSld}"
                                    bind.input.int model.cxl0342 (fun a -> dispatch (SetCxl0342 a))
                                }
                                // Count 3.4.2
                                label{
                                        attr.``type`` "text"
                                        attr.``class`` "label"
                                        $"{model.cxl0342}"
                                }
                            }
                        }
                    }
                    div{
                        attr.``class`` "label1"
                        attr.``style`` $"background:{cxClr[3]};"
                        // Label 2.1
                        input{
                                attr.``type`` "text"
                                attr.``class`` "label, input1"
                                attr.``style`` $"background:{cxClr[3]};"
                                bind.input.string model.lbl0021 (fun a -> dispatch (SetLbl0021 a))
                            }
                        // Refid 2.1
                        label{
                                attr.``type`` "text"
                                attr.``class`` "label"
                                attr.``style`` "test-align: right;"
                                " [ 2.1 ] "
                                }
                        // Slider 2.1
                        input{
                            attr.``class`` "slider1"
                            attr.``type`` "range"
                            attr.min $"{minSld}"
                            attr.max $"{maxSld}"
                            bind.input.int model.cxl0021 (fun a -> dispatch (SetCxl0021 a))
                        }
                        // Count 2.1
                        label{
                                attr.``type`` "text"
                                attr.``class`` "label"
                                $"{model.cxl0021}"
                        }
                    }
                }
                div{
                    attr.``class`` "label1"
                    attr.``style`` $"background:{cxClr[1]};"
                    // Label 1.1
                    input{
                            attr.``type`` "text"
                            attr.``class`` "label, input1"
                            attr.``style`` $"background:{cxClr[1]};"
                            bind.input.string model.lbl0011 (fun a -> dispatch (SetLbl0011 a))
                        }
                    // Refid 1.1
                    label{
                            attr.``type`` "text"
                            attr.``class`` "label"
                            attr.``style`` "test-align: right;"
                            " [ 1.1 ] "
                            }
                    // Slider 1.1
                    input{
                        attr.``class`` "slider1"
                        attr.``type`` "range"
                        attr.min $"{minSld}"
                        attr.max $"{maxSld}"
                        bind.input.int model.cxl0011 (fun a -> dispatch (SetCxl0011 a))
                    }
                    // Count 1.1
                    label{
                            attr.``type`` "text"
                            attr.``class`` "label"
                            $"{model.cxl0011}"
                    }     
                } 
            }
        }

    }

// Bolero component handling state updates and rendering the user interface
type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view