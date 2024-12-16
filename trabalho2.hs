type Memoria = [(Int, Int)]
type Acumulador = Int
type FlagEQZ = Int

--0 LOD 240   
--2 ADD 241   
--4 STO 251   
--6 HLT       
--7 NOP  
-- 8+4 = 12 
prog1 :: Memoria
prog1 = [(0,2),(1,240),(2,14),(3,241),(4,4),(5,251),(6,20),(7,18),(240,8),(241,4),(251,0)]

--prog2
--0 LOD 240   
--2 ADD 241   
--4 SUB 242   
--6 STO 251   
--8 HLT       
--9 NOP   
--8+4-2 = 10 
prog2 :: Memoria
prog2 = [(0,2),(1,240),(2,14),(3,241),(4,16),(5,242),(6,4),(7,251),(8,20),(9,18),(240,8),(241,4),(242,2),(251,0)]

--prog3
--0 LOD 240   
--2 STO 244   
--4 LOD 241   
--6 STO 245   
--8 LOD 245   
--10 CPE 246  
--12 JMZ 28   
--14 LOD 251  
--16 ADD 244  
--18 STO 251  
--20 LOD 245  
--22 SUB 247  
--24 STO 245  
--26 JMP 8    
--28 HLT      
--29 NOP  
-- 14 * 4 = 56    

prog3 :: Memoria
prog3 = [(0,2),(1,240),(2,4), (3,244),(4,2),(5,241),(6,4),(7,245),(8,2),(9,245),(10,10),(11,246),(12,8),(13,28),(14,2),(15,251),(16,14),(17,244),(18,4),(19,251),(20,2),(21,245),(22,16),(23,247),(24,4),(25,245),(26,6),(27,8),(28,20),(29,18),(240,14),(241,4),(244,0),(245,0),(246,0),(247,1),(251,0)]

--prog4
--0 LOD 240   
--2 STO 251   
--4 LOD 241   
--6 STO 245   
--8 LOD 245   
--10 CPE 246  
--12 JMZ 28   
--14 LOD 251  
--16 ADD 248  
--18 STO 251  
--20 LOD 245  
--22 ADD 247  
--24 STO 245  
--26 JMP 8    
--28 HLT      
--29 NOP      
--A = 0; Resp = 1; while(A < 5) { A = A + 1; Resp = Resp + 2;}
prog4 :: Memoria
prog4 = [(0,2),(1,240),(2,4), (3,251),(4,2),(5,241),(6,4),(7,245),(8,2),(9,245),(10,10),(11,246),(12,8),(13,28),(14,2),(15,251),(16,14),(17,248),(18,4),(19,251),(20,2),(21,245),(22,14),(23,247),(24,4),(25,245),(26,6),(27,8),(28,20),(29,18),(240,1),(241,0),(244,0),(245,0),(246,5),(247,1),(248,2),(251,0)]

-- executa do endereço 0 ok
executar :: Memoria -> Memoria
executar mem = loop mem 0 0 0 
    where 
        loop memoria pc acc eqz
         | opcode == 20 = memoria  -- finaliza a brincadeira
         | otherwise = loop memoria' pc' acc' eqz'
         where
            opcode = readMem memoria pc
            opprox = readMem memoria (pc + 1)
            (memoria', acc', eqz', pc') = executarInstrucao opcode opprox (memoria, acc, eqz, pc + 2)
       



-- aqui e pra executar uma função especifica eu adicionei um inteiro pra levar o pc junto pq sem ele não tava conseguindo fazer o jmp nem njmz
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



-- codigo de dicas, mas adicionando o pc que vai junto
execLOD :: Int -> (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execLOD end (mem, acc, eqz, pc) = (mem, readMem mem end, eqz, pc)

execSTO :: Int -> (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execSTO end (mem, acc, eqz, pc) = (writeMem mem end acc, acc, eqz, pc)

execJMP :: Int -> (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execJMP end (mem, acc, eqz, _) = (mem, acc, eqz, end)

execJMZ :: Int -> (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execJMZ end (mem, acc, eqz, pc)
    | eqz == 1  = (mem, acc, eqz, end)  -- oula se é 1 
    | otherwise = (mem, acc, eqz, pc)

execCPE :: Int -> (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execCPE end (mem, acc, _, pc)
    | acc == readMem mem end = (mem, acc, 1, pc)  -- coloca a flag em 1 
    | otherwise = (mem, acc, 0, pc)

execADD :: Int -> (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execADD end (mem, acc, eqz, pc) = (mem, acc + readMem mem end, eqz, pc)

execSUB :: Int -> (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execSUB end (mem, acc, eqz, pc) = (mem, acc - readMem mem end, eqz, pc)

execHLT :: (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execHLT (mem, acc, eqz, pc) = (mem, acc, eqz, pc)  

execNOP :: (Memoria, Acumulador, FlagEQZ, Int) -> (Memoria, Acumulador, FlagEQZ, Int)
execNOP (mem, acc, eqz, pc) = (mem, acc, eqz, pc)  


-- altera a entrada para Memoria que é a lista de tuplas que tinha antes na função da prof.
readMem :: Memoria -> Int -> Int
readMem (m:ms) e
    | e == fst m = snd m
    | e /= fst m = readMem ms e

writeMem :: Memoria -> Int -> Int -> Memoria
writeMem _ end _ | end >= 256 = error "Erro: endereço precisa estar entre 0 e 255"
writeMem _ _ val | val > 255 = error "Erro: valor precisa estar entre 0 e 255 so tem 8bitss"
writeMem [] end val = [(end, val)] 
writeMem ((end', val'):ms) end val
    | end == end' = (end, val) : ms  
    | otherwise = (end', val') : writeMem ms end val