type Memoria = [(Int, Int)]
type Acumulador = Int
type FlagEQZ = Int


-- executa do endereço 0 ok
executar :: Memoria -> Memoria
executar mem = loop mem 0 0 0 
    where 
        loop memoria pc acc eqz
         | opcode == 20 = memoria  -- 20 is execHLT
         | otherwise = loop memoria' pc' acc' eqz'
         where
            opcode = readMem memoria pc
            opprox = readMem memoria (pc + 1)
            (memoria', acc', eqz', pc') = executarInstrucao opcode opprox (memoria, acc, eqz, pc + 2)
       



-- aqui e pra executar uma função especifica (arrumar a compilação)
executarInstrucao :: Int -> Int -> (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
executarInstrucao 2 end = execLOD end
executarInstrucao 4 end = execSTO end
executarInstrucao 6 end = execJMP end
executarInstrucao 8 end = execJMZ end
executarInstrucao 10 end = execCPE end
executarInstrucao 14 end = execADD end
executarInstrucao 16 end = execSUB end
executarInstrucao 18 _ = execNOP
executarInstrucao 20 _ = execHLT
executarInstrucao _ _ = execNOP


-- PRECISA FAZER ESSAS FUNÇÔES execLOD execSTO ETC
-- codigo de dicas
execLOD :: Int -> (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execLOD end (mem, acc, eqz, pc) = (mem, readMem mem end, eqz, pc)

execSTO :: Int -> (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execSTO end (mem, acc, eqz, pc) = (writeMem mem end acc, acc, eqz, pc)

execJMP :: Int -> (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execJMP end (mem, acc, eqz, _) = (mem, acc, eqz, end)

execJMZ :: Int -> (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execJMZ end (mem, acc, eqz, pc)
    | eqz == 1  = (mem, acc, eqz, end)  -- Jump if eqz == 1
    | otherwise = (mem, acc, eqz, pc)

execCPE :: Int -> (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execCPE end (mem, acc, _, pc)
    | acc == readMem mem end = (mem, acc, 1, pc)  -- Set eqz to 1 if equal
    | otherwise = (mem, acc, 0, pc)

execADD :: Int -> (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execADD end (mem, acc, eqz, pc) = (mem, acc + readMem mem end, eqz, pc)

execSUB :: Int -> (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execSUB end (mem, acc, eqz, pc) = (mem, acc - readMem mem end, eqz, pc)

execHLT :: (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execHLT (mem, acc, eqz, pc) = (mem, acc, eqz, pc)  -- Halt execution

execNOP :: (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execNOP (mem, acc, eqz, pc) = (mem, acc, eqz, pc)  -- No operation


-- altera a entrada para Memoria que é a lista de tuplas que tinha antes na função da prof.
readMem :: Memoria -> Int -> Int
readMem (m:ms) e
    | e == fst m = snd m
    | e /= fst m = readMem ms e

writeMem :: Memoria -> Int -> Int -> Memoria
writeMem _ end _ | end >= 256 = error "Erro: endereço precisa estar entre 0 e 255"
writeMem _ _ val | val > 255 = error "Erro: valor precisa estar entre 0 e 255 (8 bits) parceiro"
writeMem [] end val = [(end, val)] 
writeMem ((end', val'):ms) end val
    | end == end' = (end, val) : ms  
    | otherwise = (end', val') : writeMem ms end val