﻿module ICPC
open System

let input= "please sit spot. sit spot, sit. spot here now here."

let splitString (text : string) = // Creates a list out of a string
    text.Split [|' '|] |> Seq.toList

let isValidInput (text:string)= // Checks to see if the string is valid the commaSprinkler
    let charList = Seq.toList(text)
    match charList<>[] && List.length (splitString text) >= 1 && (List.last charList)='.' with
    | true ->   let rec checkCharacters i charList=
                    match charList with
                    | [] -> true
                    | head::tail -> let asciiVal= head |> int
                                    match (asciiVal >=97 && asciiVal <=122),i with // String cannot start with a special character and cannot contain capital letters
                                    | false,0 -> false
                                    | true,_ -> checkCharacters (i+1) tail
                                    | _ ->  let nextChr= match tail with
                                                              | []->'0' //indicates next item is an empty list
                                                              |innerHead::_->innerHead 
                                            match (head = ' '&& nextChr <> ' ' && nextChr <>'.' && nextChr<> ',') || (head = ',' && nextChr=' ') || (head = '.' && (nextChr = ' ' || nextChr= '0')) with // Checks if special that there are no other special characters beside
                                            | true -> checkCharacters (i+1) tail              // whitespace, period, and comma
                                            | _-> false
                checkCharacters 0 charList
    | _ -> false

let  containsCommaOrPeriod (word: char list)= // Keeps track of whether the end of a word contains a comma or period
    match List.last word with                 // Returns a bool tuple: (endsWithComma,endsWithPeriod)
    | ',' -> true,false
    | '.' -> false,true
    | _ -> false,false

let rec addWordOccurance index orginalWord xs out = // Add the index occurance of a word to list of locations the word occures
    match xs with
    | [] -> out
    | head::tail -> let word,occuranceIndices= head
                    match word=orginalWord with //Finds the word of the index to add
                    | true -> addWordOccurance index orginalWord tail (out@[(word,occuranceIndices@[index])])
                    | _ ->  addWordOccurance index orginalWord tail (out@[(word,occuranceIndices)])                      

let rec updateCurrent xs commaPeriodList=
    match xs with
    | [] -> commaPeriodList
    | head::tail -> updateCurrent tail (List.mapi(fun i v-> match head=i with 
                                                            | true ->  let containsComma,containsPeriod = v
                                                                       match containsComma,containsPeriod with
                                                                       | true,_ | _,true -> v
                                                                       | false,_ -> (true,false)
                                                            | _ -> v) commaPeriodList)


let rec updateNext xs commaPeriodList=
    match xs with
    | [] -> commaPeriodList
    | head::tail -> updateNext tail (List.mapi(fun i v-> match head<>0 && i=head-1 with 
                                                                    | true ->  let containsComma,containsPeriod = v
                                                                               match containsComma,containsPeriod with
                                                                               | true,_ | _,true -> v
                                                                               | false,_ -> (true,false)
                                                                    | _ -> v) commaPeriodList)

let updateCommaPeriodList currWordIndices nextWordIndices commaPeriodList =
    (updateCurrent currWordIndices commaPeriodList) |> (updateNext nextWordIndices) 

let rec getItemFromIndex index xs=
    match xs with
    | [] -> None
    | head::tail -> let word,indicesList= head
                    let rec increment xs= 
                        match xs with 
                        | [] -> false
                        | innerHead::innerTail -> match innerHead=index with 
                                                  | true -> true
                                                  | _ -> increment innerTail
                    match increment indicesList with | true -> Some head | false -> getItemFromIndex index tail

let rec getRemaindingList i n xs=
    match xs with
    | [] -> xs
    | head::tail -> match i=n with 
                    | false -> getRemaindingList (i+1) n tail
                    | true-> tail
let rec addCommas index wordListLength wordOccuranceList xs out= // Updates commaPeriodList to add all commas created by commaSprinkler rules
    match xs with
    | [] -> out
    | head::tail -> match head with
                    | (true,_)-> match index+1 >= wordListLength with
                                 | true -> out
                                 | _ -> match (getItemFromIndex index wordOccuranceList),(getItemFromIndex (index+1) wordOccuranceList) with
                                        | None,_ | _,None -> failwith "Tried to access an index that doesn't exist"
                                        | Some(_,currWordIndices),Some(_,nextWordIndices) -> let updatedList= updateCommaPeriodList currWordIndices nextWordIndices out
                                                                                             addCommas (index+1) wordListLength wordOccuranceList (getRemaindingList 0 index updatedList) updatedList
                    | _ -> addCommas (index+1) wordListLength wordOccuranceList tail out

let rec createInitialState index (wordList:string list) commaPeriodList wordOccuranceList= // Goes through the intial list of words to create the list of which words end with commas and periods
    match wordList with
    | [] -> (List.rev commaPeriodList),(wordOccuranceList)
    | head::tail -> createInitialState (index+1) tail (containsCommaOrPeriod (Seq.toList(head))::commaPeriodList) 
                            (let trimmedWord = head.Trim(',','.')
                            match (List.tryFind (fun (w,i)->w=trimmedWord) wordOccuranceList) with // Checks if word already exists
                            | None -> (wordOccuranceList@[(trimmedWord,[index])]) // Adds a new word to the word occurance list
                            | _ -> addWordOccurance index trimmedWord wordOccuranceList []) // If word exists adds a new occuranceIndex and returns the updated list

let rec getWordFromIndex index xs=
    match xs with
    | [] -> None
    | head::tail -> let word,indicesList= head
                    let rec increment xs= 
                        match xs with 
                        | [] -> getWordFromIndex index tail
                        | innerHead::innerTail -> match innerHead=index with 
                                                  | true -> Some word
                                                  | _ -> increment innerTail
                    increment indicesList

let rec createNewString  index newString wordOccuranceList commaPeriodList =
    match commaPeriodList with
    | [] -> Some newString
    | head::tail -> let word= getWordFromIndex index wordOccuranceList
                    match word with
                    | None -> failwith "An error occured in creating the string- indexing error"
                    | Some w -> match newString with
                                | "" -> match head with
                                        | true,_ -> createNewString (index+1)(w+",") wordOccuranceList tail
                                        | _,true -> createNewString (index+1)(w+".") wordOccuranceList tail
                                        | _ -> createNewString (index+1) (w) wordOccuranceList tail
                                | _ ->  match head with
                                        | true,_ -> createNewString (index+1)(newString+" "+w+",") wordOccuranceList tail
                                        | _,true -> createNewString (index+1)(newString+" "+w+".") wordOccuranceList tail
                                        | _ -> createNewString (index+1) (newString+" "+w) wordOccuranceList tail

let commaSprinkler input =
    match isValidInput input with
    | true -> let commaPeriodList,wordOccuranceList= createInitialState 0 (splitString input) [] []
              ((addCommas 0 (List.length commaPeriodList) wordOccuranceList commaPeriodList commaPeriodList)) |> (createNewString 0 "" (wordOccuranceList))
    | _ -> None



let isValidRiver input = //Checks to see if the input given for river is valid
    let charList = Seq.toList(input) // string must have 2 words, cannot be empty string, cannot end and start with a whitespace
    match charList<>[] && List.length (splitString input) >= 2 && (List.last charList)<>' ' && (List.head charList) <>' ' with
    | true ->   let rec checkCharacters i charList=
                    match charList with
                    | [] -> true
                    | head::tail -> let chrVal= head |> int
                                    let validChr asciiVal=(asciiVal >=97 && asciiVal <=122) || (asciiVal>=65 && asciiVal<=90) // string must be lower or upper case character
                                    match validChr chrVal with // String cannot start with a special character 
                                    | false ->  let nextChr= match tail with
                                                             | [] ->'0'
                                                             | innerHead::_ -> innerHead
                                                match head=' ' && nextChr |> int |> validChr with 
                                                | true->  checkCharacters 0 tail // resets character counter after a valid whitespace
                                                | _ -> false
                                    | true -> match i<80 with //single word cannot be larger than 80 characters note: starting index is 0
                                               | true -> checkCharacters (i+1) tail
                                               | _ -> false
                checkCharacters 0 charList
    | _ -> false

let rec getWordLength i wordIndex wordList= //Gets the length of a specifc index given and its index in a list of strings
        match i=wordIndex with
        | true -> List.fold(fun acc item -> acc+1) 0 (Seq.toList((List.item i wordList)))
        | _ -> getWordLength (i+1) wordIndex wordList

let rec longestWord i listLength longestLength input= // Finds the length of the longest word given a list of strings
    match i=listLength with
    | true -> longestLength 
    | _ -> longestWord (i+1) listLength (let curWordLength= getWordLength 0 i input
                                         match longestLength < curWordLength with 
                                         |true -> curWordLength 
                                         | _ -> longestLength) input

let rec newLineTextList index prevLength lineWidth lineText wordList lineTextList=
    match index<>List.length wordList with
    | true ->   let wordLength = getWordLength 0 index wordList
                match prevLength = 0 with
                | true -> match wordLength<lineWidth,wordLength=lineWidth with //first word on a new line
                          | true,false ->newLineTextList (index+1) wordLength lineWidth (List.item index wordList) wordList lineTextList
                          | false,true -> newLineTextList (index+1) wordLength lineWidth "" wordList (lineTextList@[(List.item index wordList)])
                          | _ -> failwith "Error in calculating the longest word"
                | _ -> let newLineLength = 1 + prevLength + wordLength
                       let newLineText = lineText + " " + List.item index wordList
                       match newLineLength < lineWidth, newLineLength = lineWidth with
                       | true,false -> newLineTextList (index+1) newLineLength lineWidth newLineText wordList lineTextList // existing text fits into new line with additional space left
                       | false,true -> newLineTextList (index+1) newLineLength lineWidth "" wordList (lineTextList@[newLineText]) // words existing text fits into new line with no additional space
                       | false,false -> newLineTextList index 0 lineWidth "" wordList (lineTextList@[lineText]) // word doesn't fit has to go onto new line
                       | _ -> failwith "Error has occured with the line width"
    | _ -> lineTextList


let rec whitespaceIndices i xs out= // Returns a list of the indices in which there is a whitespace
    match xs with
    | [] -> out
    | head::tail -> match head=' ' with
                    | true -> whitespaceIndices (i+1) tail (out@[i])
                    | _ -> whitespaceIndices (i+1) tail out

//getLongestRiver still needs to be fully implemented to work
let rec getLongestRiver index count xs out=
    match xs with
    | [] -> count
    | head::next::tail -> let curLine= Seq.toList head
                          let nextLine= Seq.toList next
                          let indexes=0
                          match (List.tryItem (indexes-1) nextLine)=Some ' ',
                                (List.tryItem indexes nextLine)=Some ' ',
                                (List.tryItem (indexes+1) nextLine)=Some ' ' with
                          | true,_,_ -> indexes-1
                          | _,true,_ -> indexes
                          | _,_,true -> indexes+1
                          | _ -> -1
    | head::tail -> count

let rivers input =
    match isValidRiver input with
    | true -> Some ""
    | false -> None

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
