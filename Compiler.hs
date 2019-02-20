{-# LANGUAGE LambdaCase #-}
module Compiler
    (compileBf) where

import Brainfuck
import System.IO

compileBf :: BfSource -> FilePath -> IO ()
compileBf src outname = do
    handle <- openFile outname WriteMode
    -- Beginning of nasm program
    hPutStrLn handle $ unlines
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
    mapM_ (bf2asm handle) src
    -- Exit
    hPutStrLn handle "    mov rax, 1"
    hPutStrLn handle "    xor rbx, rbx"
    hPutStr handle   "    int 80h"
    hClose handle


bf2asm :: Handle -> BfCommand -> IO ()
bf2asm handle = hPutStrLn handle . \case
    GoRight x -> "    " ++ case () of
        _ | x == 1    -> "inc rcx"
          | x == (-1) -> "dec rcx"
          | x > 0     -> "add rcx," ++ show x
          | otherwise -> "sub rcx," ++ show (-x)
    Add x -> unlines
        [ "    mov al, [rcx]"
        , "    " ++ case () of
            _ | x == 1    -> "inc al"
              | x == (-1) -> "dec al"
              | x > 0     -> "add al, " ++ show x
              | otherwise -> "sub al, " ++ show (-x)
        , "    mov [rcx], al"
        ]
    LoopL x -> unlines
        [ "_LS" ++ show x ++ ":"
        , "    mov al, [rcx]"
        , "    test al, al"
        , "    jz _LE" ++ show x
        ]
    LoopR x -> unlines
        [ "    jmp _LS" ++ show x
        , "_LE" ++ show x ++ ":"
        ]
    WriteChar -> "    call _printChar"
    ReadChar  -> "    call _readChar"
    BfConst x -> unlines
        [ "    " ++ case x of
            0 -> "xor al, al"
            _ -> "mov al, " ++ show x
        , "    mov [rcx], al"
        ]