module Rune.Backend.X86_64.Registers
  ( x86_64Registers,
    x86_64ArgsRegisters,
  )
where

--
-- public
--

x86_64Registers :: [String]
x86_64Registers =
  [ "rax",
    "rbx",
    "rcx",
    "rdx",
    "rsi",
    "rdi",
    "rbp",
    "rsp",
    "r8",
    "r9",
    "r10",
    "r11",
    "r12",
    "r13",
    "r14",
    "r15"
  ]

x86_64ArgsRegisters :: [String]
x86_64ArgsRegisters =
  [ "rdi",
    "rsi",
    "rdx",
    "rcx",
    "r8",
    "r9"
  ]
