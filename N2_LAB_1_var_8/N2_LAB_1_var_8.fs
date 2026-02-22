(*
Задание 2 Вариант 8
Сформировать список из цифр числа.
*)

open System

let rec addDigits positiveNum  list=
    if positiveNum = 0 then
        list
    else
        let temp = positiveNum % 10
        let newList = [temp] @ list 
        addDigits (positiveNum / 10) newList

[<EntryPoint>]
let main args =
    printf "Введите целое число: "
    let num = int(Console.ReadLine())    
    let positiveNum = abs num
    
    let result = addDigits positiveNum []
    printf "%A " result
    0