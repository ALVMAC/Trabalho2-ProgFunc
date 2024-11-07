type Memoria = [(Int, Int)]
type Acumulador = Int
type FlagEQZ = Int


-- executa do endereço 0 ok
executar :: Memoria -> Memoria
executar mem = loop mem 0 0 0
    where 
        loop memoria pc acc eqz
         | opcode == 20 = memoria  -- 20 é o exechlt e f
         | otherwise = loop memoria' pc' acc' eqz'
         where
            opcode = readMem memoria pc
            opprox = readMem memoria (pc+1)
            (memoria', acc', eqz') = executarInstrucao opcode opprox (memoria, acc, eqz)
            pc' = pc + 2            



-- aqui e pra executar uma função especifica (arrumar a compilação)
executarInstrucao :: Int -> Int -> (Memoria, Acumulador, FlagEQZ) -> (Memoria, Acumulador, FlagEQZ)
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
execLOD ::Int -> (Memoria, Acumulador, FlagEQZ) -> (Memoria, Acumulador, FlagEQZ) 
execLOD end (mem, acc, eqz) = (mem, readMem mem end, eqz)

execSTO :: Int -> (Memoria, Acumulador, FlagEQZ) -> (Memoria, Acumulador, FlagEQZ)
execSTO end (mem, acc, eqz) = (writeMem mem end acc, acc, eqz)

execJMP :: Int -> (Memoria, Acumulador, FlagEQZ) -> (Memoria, Acumulador, FlagEQZ)
execJMP end (mem, acc, eqz) = (mem, acc, eqz)


execJMZ :: Int -> (Memoria, Acumulador, FlagEQZ) -> (Memoria, Acumulador, FlagEQZ)
execJMZ end (mem, acc, eqz)
    | eqz == 1  = (mem, acc, eqz)  -- Salta se eqz for 1 (acumulador == 0)
    | otherwise = (mem, acc, eqz)  -- Caso contrário, não faz nada


execCPE :: Int -> (Memoria, Acumulador, FlagEQZ) -> (Memoria, Acumulador, FlagEQZ)
execCPE end (mem, acc, _)
    | acc == readMem mem end = (mem, acc, 1)  -- Se forem iguais, seta eqz para 1
    | otherwise              = (mem, acc, 0)  -- Caso contrário, seta eqz para 0


execADD :: Int -> (Memoria, Acumulador, FlagEQZ) -> (Memoria, Acumulador, FlagEQZ)
execADD end (mem, acc, eqz) = (mem, acc + readMem mem end, eqz)


execSUB :: Int -> (Memoria, Acumulador, FlagEQZ) -> (Memoria, Acumulador, FlagEQZ)
execSUB end (mem, acc, eqz) = (mem, acc - readMem mem end, eqz)


execHLT :: (Memoria, Acumulador, FlagEQZ) -> (Memoria, Acumulador, FlagEQZ)
execHLT (mem, acc, eqz) = (mem, acc, eqz)

-- do nosso codigo de dicas
execNOP :: (Memoria, Acumulador, FlagEQZ) -> (Memoria, Acumulador, FlagEQZ) 
execNOP (mem, acc, eqz) = (mem, acc, eqz)

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

