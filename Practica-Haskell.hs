-- Main.hs
-- Importamos las librerías necesarias
import System.IO
import System.Directory (doesFileExist)
import Data.List (findIndex, delete)

-- Definimos un tipo de dato para representar una tarea
data Task = Task {
    description :: String,
    isCompleted :: Bool
} deriving (Show, Read) -- Show para convertir a String, Read para convertir desde String

-- Constante para el nombre del archivo donde se guardarán las tareas
tasksFile :: String
tasksFile = "tasks.txt"

-- Función principal que se ejecuta al iniciar el programa
main :: IO ()
main = do
    putStrLn "================================="
    putStrLn "   Aplicación de Lista de Tareas   "
    putStrLn "================================="
    -- Cargamos las tareas desde el archivo
    tasks <- loadTasks
    -- Iniciamos el bucle principal de la aplicación
    appLoop tasks

-- Bucle principal de la aplicación
appLoop :: [Task] -> IO ()
appLoop tasks = do
    -- Mostramos el menú de opciones
    putStrLn "\nOpciones:"
    putStrLn "1. Ver tareas"
    putStrLn "2. Agregar tarea"
    putStrLn "3. Marcar tarea como completada"
    putStrLn "4. Eliminar tarea"
    putStrLn "5. Guardar y Salir"
    putStr "> "
    hFlush stdout -- Aseguramos que el prompt se muestre antes de leer la entrada

    -- Leemos la opción del usuario
    option <- getLine
    case option of
        "1" -> viewTasks tasks >> appLoop tasks
        "2" -> addTask tasks >>= appLoop
        "3" -> completeTask tasks >>= appLoop
        "4" -> removeTask tasks >>= appLoop
        "5" -> saveTasks tasks >> putStrLn "¡Tareas guardadas! Adiós."
        _   -> putStrLn "Opción no válida, intenta de nuevo." >> appLoop tasks

-- 1. Función para mostrar las tareas
viewTasks :: [Task] -> IO ()
viewTasks tasks
    | null tasks = putStrLn "\nNo hay tareas pendientes."
    | otherwise  = do
        putStrLn "\n--- Tus Tareas ---"
        mapM_ printTask (zip [1..] tasks)
    where
        printTask :: (Int, Task) -> IO ()
        printTask (index, task) =
            putStrLn $ show index ++ ". " ++ formatTask task
        
        formatTask :: Task -> String
        formatTask task =
            let status = if isCompleted task then "[x]" else "[ ]"
            in status ++ " " ++ description task

-- 2. Función para agregar una nueva tarea
addTask :: [Task] -> IO [Task]
addTask tasks = do
    putStr "Describe la nueva tarea: "
    hFlush stdout
    desc <- getLine
    let newTask = Task { description = desc, isCompleted = False }
    putStrLn "¡Tarea agregada!"
    return (tasks ++ [newTask])

-- 3. Función para marcar una tarea como completada
completeTask :: [Task] -> IO [Task]
completeTask tasks = do
    putStr "Número de la tarea a completar: "
    hFlush stdout
    indexStr <- getLine
    let maybeIndex = readMaybe indexStr :: Maybe Int
    case maybeIndex of
        Just index | index > 0 && index <= length tasks ->
            let updatedTasks = updateTaskAt (index - 1) (\t -> t { isCompleted = True }) tasks
            in putStrLn "¡Tarea marcada como completada!" >> return updatedTasks
        _ -> putStrLn "Índice inválido." >> return tasks

-- 4. Función para eliminar una tarea
removeTask :: [Task] -> IO [Task]
removeTask tasks = do
    putStr "Número de la tarea a eliminar: "
    hFlush stdout
    indexStr <- getLine
    let maybeIndex = readMaybe indexStr :: Maybe Int
    case maybeIndex of
        Just index | index > 0 && index <= length tasks ->
            let updatedTasks = deleteTaskAt (index - 1) tasks
            in putStrLn "¡Tarea eliminada!" >> return updatedTasks
        _ -> putStrLn "Índice inválido." >> return tasks

-- Función auxiliar para actualizar una tarea en una posición específica
updateTaskAt :: Int -> (Task -> Task) -> [Task] -> [Task]
updateTaskAt index f tasks = take index tasks ++ [f (tasks !! index)] ++ drop (index + 1) tasks

-- Función auxiliar para eliminar una tarea en una posición específica
deleteTaskAt :: Int -> [Task] -> [Task]
deleteTaskAt index tasks = take index tasks ++ drop (index + 1) tasks

-- Función para leer de forma segura un String
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(val, "")] -> Just val
    _           -> Nothing

-- 5. Funciones para guardar y cargar tareas
saveTasks :: [Task] -> IO ()
saveTasks tasks = writeFile tasksFile (show tasks)

loadTasks :: IO [Task]
loadTasks = do
    fileExists <- doesFileExist tasksFile
    if fileExists
        then do
            content <- readFile tasksFile
            -- Usamos readMaybe para manejar el caso de archivo vacío o corrupto
            case readMaybe content of
                Just tasks -> return tasks
                Nothing    -> return [] -- Si el archivo está malformado, empezamos con una lista vacía
        else return [] -- Si el archivo no existe, empezamos con una lista vacía