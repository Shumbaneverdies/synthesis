module Synthesis

open System.Diagnostics

let abelar belar =
    belar>12 && belar<3097 && (belar%2)=0

let area b h =
    match (b<0.0),(h<0.0) with 
    |_,true | true,_ -> failwith "base and height"
    |_,_ -> ( 0.5 * b ) * h
   
let zollo p = 
   match (p<0) with 
   |true -> -1*p
   |false -> p + p
 
let min sam jack =
    match (sam > jack) with
    |true -> jack
    |false -> sam 

let max alex jones =
    match (alex > jones) with 
    |true -> alex
    |false -> jones 

let ofTime h m s =
    (h * 3600) + (m * 60) + (s)

let toTime secs = match secs>0 with 
                  |false -> 0,0,0
                  |true -> let hours = secs/3600
                           let minutes = (secs - (hours * 3600))/60
                           let seconds = secs - (hours * 3600) - (minutes *60)
                           hours,minutes,seconds
        

let digits d = 
 let rec N digit acc = 
         match (digit/10)=0 with 
         |true -> acc
         |false -> N (digit /10) (acc+1)
 N d 1
 

let minmax (a,b,c,d) =
    let(_num1,_) = (a,b) 

let isLeap year = 
 match (year>=1582), (year%4)=0 , (year%100)=0, (year%400)=0 with 
 |false,_,_,_ -> failwith "fail"
 |_,true,true,false -> false 
 |_,true,true,true | _,true,false,false -> true
 |_ -> false


let month monthnum =
   match (monthnum>=1 && monthnum <=12), (monthnum = 1), (monthnum=2), (monthnum=3), (monthnum=4), (monthnum=5), (monthnum=6), (monthnum=7), (monthnum=8), (monthnum=9), (monthnum =10), (monthnum=11),(monthnum=12) with 
   |false,_,_,_,_,_,_,_,_,_,_,_,_ -> failwith "shouldFail"
   |_,true,false,false,false,false,false,false,false,false,false,false,false -> ("January", 31)
   |_,false,true,false,false,false,false,false,false,false,false,false,false -> ("February",28)
   |_,false,false,true,false,false,false,false,false,false,false,false,false -> ("March", 31)
   |_,false,false,false,true,false,false,false,false,false,false,false,false -> ("April", 30)
   |_,false,false,false,false,true,false,false,false,false,false,false,false -> ("May",31)
   |_,false,false,false,false,false,true,false,false,false,false,false,false -> ("June", 30)
   |_,false,false,false,false,false,false,true,false,false,false,false,false -> ("July", 31)
   |_,false,false,false,false,false,false,false,true,false,false,false,false -> ("August", 31)
   |_,false,false,false,false,false,false,false,false,true,false,false,false -> ("September", 30)
   |_,false,false,false,false,false,false,false,false,false,true,false,false -> ("October", 31)
   |_,false,false,false,false,false,false,false,false,false,false,true,false -> ("November",30)
   |_,false,false,false,false,false,false,false,false,false,false,false,true -> ("December", 31)
   |_ -> failwith "shouldfail"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"