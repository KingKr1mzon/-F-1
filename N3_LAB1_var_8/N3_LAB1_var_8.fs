(*
Создайте собственные функции для выполнения основных операций над списками (добавление/
удаление/поиск элемента, сцепка двух списков, получение элемента по номеру).
*)

open System

// добавление числа в начало
let addStart digit list=
    digit :: list
    
// добавление числа в конец
let rec addEnd digit list =
    match list with
    | [] -> [digit]
    | head :: tail -> head :: (addEnd digit tail)
    
// добавление числа по индексу
let rec addIndex index digit list =
    match index, list with
    | 0, _ -> [digit] @ list
    | index, head :: tail -> head :: (addIndex (index - 1) digit tail)
    | _, [] -> [digit]

// Удаление числа в начале
let removeStart list = 
    match list with
    | [] -> []
    | _ :: tail -> tail
    
// Удаление числа с конца
let rec removeEnd list =
    match list with
    | [] | [_] -> []
    | head :: tail -> head :: (removeEnd tail)

// Удаление числа по индексу
let rec removeIndex index list =
    match index, list with
    | _, [] -> []
    | 0, _ :: tail -> tail
    | index, head :: tail -> head :: (removeIndex (index - 1) tail)

// поиск числа по номеру
let rec getIndex index list =
    match index, list with
    | 0, head :: _ -> Some(head)
    | index, _ :: tail when index > 0 -> getIndex (index - 1) tail
    | _ -> None

// объединение списков
let rec merger list1 list2 =
    match list1 with
    | [] -> list2
    | head :: tail -> head :: (merger tail list2)

// Главное меню
let rec mainMenu (currentList: int list) =
    printfn "\n--- Текущий список: %A ---" currentList
    printfn "1 - Добавить число в начало"
    printfn "2 - Добавить число в конец"
    printfn "3 - Добавить число по индексу"
    printfn "4 - Удалить  число в начале"
    printfn "5 - Удалить  число в конце"
    printfn "6 - Удалить  число по индексу"
    printfn "7 - Поиск    числа по номеру"
    printfn "8 - Объединение списков"
    printfn "exit - выход"
    printf "Выбор: "
    
    let input = Console.ReadLine()
    
    match input with
    | "1" ->
        printf "Число: "
        let temp = int(Console.ReadLine())
        mainMenu (addStart temp currentList)
    | "2" ->
        printf "Число: "
        let temp = int(Console.ReadLine())
        mainMenu (addEnd temp currentList)
    | "3" ->
        printf "Число: "
        let temp = int(Console.ReadLine())
        printf "Позиция: "
        let index = int(Console.ReadLine())
        if index < 0 then
            printfn "Ошибка: Некорректно введен индекс!"
            mainMenu currentList
        else
            mainMenu (addIndex index temp currentList)
    | "4" ->
        if List.isEmpty currentList then
            printfn "Ошибка: Список пуст, удаление невозможно."
            mainMenu currentList
        else
            mainMenu (removeStart currentList)
    | "5" -> 
        mainMenu (removeEnd currentList)
    | "6" ->
        printf "Позиция: "
        let index = int(Console.ReadLine())
        if index < 0 || index >= List.length currentList then
            printfn "Ошибка: Индекс вне диапазона! (Допустимо: 0..%d)" (currentList.Length - 1)
            mainMenu currentList
        else
            mainMenu (removeIndex index currentList)
    | "7" ->
        printf "Индекс: "
        let i = int (Console.ReadLine())
        if i < 0 || i >= List.length currentList then
            printfn "Ошибка: Индекс %d вне диапазона (0..%d)" i (currentList.Length - 1)
            mainMenu currentList
        else
            match getIndex i currentList with
            | Some x -> printfn "Число под номером %d: %d" i x
            | None -> printfn "Ошибка: Элемент не найден"
            mainMenu currentList
    | "8" ->
        mainMenu (merger currentList [10; 20; 30])
    | "exit" | "Exit" | "EXIT"  ->
        printfn "Программа завершена"
    | _ ->
        printfn "Ошибка: неверный пункт меню"
        mainMenu currentList
    

[<EntryPoint>]
let main args =
    mainMenu [1; 2; 3]
        
    0