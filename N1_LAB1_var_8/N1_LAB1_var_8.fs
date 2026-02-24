(*
Задание 1 Вариант 8
Сформировать список из значений true и false по принципу: true — если очередное введенное
число нечетное, false — в противном случае.
*)

open System

let rec buildList digits list =
    if digits <= 0 then
        list
    else
        printf "Введите число: "
        let num = int(Console.ReadLine())
        let newListTail =
            if num % 2 = 0 then
                list @ [false]
            else
                list @ [true]
        buildList (digits - 1) newListTail

[<EntryPoint>]
let main args =
    printf "Введите кол-во цифр, которое хотие ввести: "
    let digits = int(Console.ReadLine())
    
    let result = buildList digits []
    printfn "Список: %A" result    
    0