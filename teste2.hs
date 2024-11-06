type Memoria = [(Int, Int)]


-- executa do endereço 0
executar :: Memoria -> Memoria
executar mem = loop mem 0 0 0
    where 
        loop memoria pc acc eqz
         | opcode == 20 = memoria  -- 20 é o hlt e f
         | otherwise = loop memoria' pc' acc' eqz'
         where
            opcode = readMem memoria pc
            opprox = readMem memoria (pc+1)
            (memoria', acc', eqz') = executarInstrucao opcode opprox (memoria, acc, eqz)
            pc' = pc + 2            

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

-- altera a entrada para Memoria que é a lista de tuplas que tinha antes na função da prof.
readMem :: Memoria -> Int -> Int
readMem (m:ms) e
    | e == fst m = snd m
    | e /= fst m = readMem ms e

