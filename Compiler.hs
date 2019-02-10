module Compiler
    (compileBf) where

import Brainfuck
import System.IO

compileBf :: BfSource -> FilePath -> IO ()
compileBf src outname = do
    handle <- openFile outname WriteMode
    -- Beginning of nasm program
    mapM_ (hPutStrLn handle)
        [ "section .bss"
        , "    memory resb 30000"
        , "section .text"
        , "    global _start"
        , "_printChar:"
        , "    mov rdx, 1"
        , "    mov rbx, 1"
        , "    mov rax, 4"
        , "    int 80h"
        , "    ret"
        , "_readChar:"
        , "    mov rax, 3"
        , "    xor rbx, rbx"
        , "    mov rdx, 1"
        , "    int 80h"
        , "    ret"
        , "_start:"
        , "    mov rcx, memory"
        ]
    -- Generate code
    mapM_ (bf2asm handle) (fromBfSource src)
    -- Exit
    hPutStrLn handle "    mov rax, 1"
    hPutStrLn handle "    xor rbx, rbx"
    hPutStr handle   "    int 80h"
    hClose handle


bf2asm :: Handle -> BfCommand -> IO ()
bf2asm handle (GoLeft x) = hPutStrLn handle $
    "    " ++
        if x == 1
            then "dec rcx"
            else "sub rcx, " ++ show x
bf2asm handle (GoRight x) = hPutStrLn handle $
    "    " ++
        if x == 1
            then "inc rcx"
            else "add rcx, " ++ show x
bf2asm handle (Add x) =
    mapM_ (hPutStrLn handle)
        [ "    mov al, [rcx]"
        , "    " ++
            if x == 1
                then "inc al"
                else "add al, " ++ show x
        , "    mov [rcx], al"
        ]
bf2asm handle (Sub x) =
    mapM_ (hPutStrLn handle)
        [ "    mov al, [rcx]"
        , "    " ++
            if x == 1
                then "dec al"
                else "sub al, " ++ show x
        , "    mov [rcx], al"
        ]
bf2asm handle (LoopL x) =
    hPutStrLn handle $ "_L" ++ show x ++ ":"
bf2asm handle (LoopR x) =
    mapM_ (hPutStrLn handle)
        [ "    mov al, [rcx]"
        , "    test al, al"
        , "    jnz _L" ++ show x
        ]
bf2asm handle WriteChar = hPutStrLn handle "    call _printChar"
bf2asm handle ReadChar  = hPutStrLn handle "    call _readChar"
bf2asm handle (BfConst x) =
    mapM_ (hPutStrLn handle)
        [ "    " ++
            if x == 0
                then "xor al, al"
                else "mov al, " ++ show x
        , "    mov [rcx], al"
        ]