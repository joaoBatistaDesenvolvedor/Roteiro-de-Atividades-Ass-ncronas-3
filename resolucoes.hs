--1) Operador lógico OU (pré-fixo):
--a) Apresente 3 definições para o operador lógico OU, utilizando casamento de padrões.--
--b) Apresente 2 definições para o operador lógico OU, utilizando expressões condicionais--
--(no lugar de casamento de padrões)--

{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
ou::Bool->Bool->Bool
ou True True =True
ou False  True =True
ou True False =True
ou False False =False

ou2:: Bool->Bool->Bool
ou2 False False =False
ou2 _ _=True

ou3:: Bool->Bool->Bool
ou3 True a=True
ou3 False False=False

ou_com_if1:: Bool->Bool->Bool
ou_com_if1 x y=not (x == False && y==False)

ou_com_if2:: Bool->Bool->Bool
ou_com_if2 x y=not (x == False && y==False) || x


--2) Defina uma função que recebe dois pontos no espaço e retorna a distância entre eles.--
--Considere que um ponto no espaço é representado por uma dupla de números (float) que--
--correspondem às coordenadas do ponto.--

hypotenusa::(Float,Float)->Float

hypotenusa (co,ca)=(co**2+ca**2)**(1/2)


--3) Dado um valor inteiro, escreva a função recursiva fatorial. Obs: Fazer uma--
--definição usando guardas e outra com casamento de padrões.--
fatorial::Int->Int
fatorial 0=1
fatorial n=n*fatorial(n-1)

fatorial_guardas:: Int->Int
fatorial_guardas n | n==0=1
                   |otherwise =n*fatorial_guardas(n-1)


--4) Dado um número inteiro n, escreva a função recursiva fibo que retorna o n-ésimo--
--termo da sequência de Fibonacci a seguir, sendo os casos base F0 = 0 e F1 = 1. Utilize a--
--definição recursiva vista em sala: fibo(n) = fibo(n-2) + fibo(n-1).--

fibonacci::Int->Int
fibonacci 1=1
fibonacci 2=1
fibonacci n=fibonacci (n-2)+fibonacci (n-1)

--5) Dado um número inteiro n, escreva a função recursiva n_tri, que retorna o n-ésimo--
--termo da sequência de números triangulares, dada a seguir.--
--0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, ...--
triangulares::Int->Int
triangulares 1 = 1
triangulares n =n+ triangulares(n-1)
--6) Escreva a função potencia2, que calcula a potência de 2 elevada a um expoente n--
--de forma recursiva: 2---
--n = 2n-1 * 2.--

potencia2::(Int ,Int )->Int
potencia2 (b,0) =1
potencia2 (b,1) =b
potencia2 (b,e) =b*potencia2(b,e-1)*2

--7) a) Escreva a função recursiva prodIntervalo: dados dois inteiros m e n, onde m<n,--
--retorna o produto: m*(m+1)*...(n-1)*n.--

prodIntervalo:: (Int,Int )->Int
prodIntervalo (m,n)= if(m<=n) then m*prodIntervalo((m+1,n)) else 1

-- b) Reescreva a função fatorial usando a função prodIntervalo.--
fatorial2_prodIntervalo::Int->Int
fatorial2_prodIntervalo x =prodIntervalo (1,x)

--8) Defina de forma recursiva as funções resto_div e div_inteira, que retornam o--
--resto e o quociente da divisão inteira de um inteiro m por inteiro n, realizando subtrações--
--sucessivas de n a partir de m.--
--Ex: m=20 e n=3: 20-3=17, 17-3=14, 14-3=11, 11-3=8, 8-3=5, 5-3=2.---
--Como 2<3: resto=2 e quociente=6.--


div_inteira::(Int,Int)->Int
div_inteira (dividendo,divisor) | dividendo<divisor=0
                                | otherwise = 1 + div_inteira((dividendo - divisor, divisor))




resto_inteira::(Int,Int)->Int
resto_inteira (dividendo,divisor) | dividendo==0|| divisor==1=0
                                  | dividendo<divisor =dividendo
                                  | otherwise = resto_inteira((dividendo - divisor, divisor))




--9) Implemente a função mdc, usando a definição recursiva vista em sala:--
--mdc(m,n) = m, se n = 0--
--mdc(m,n) = mdc(n, k), se n > 0, sendo k = m mod n--
--Obs: Fazer uma definição usando guardas e outra com casamento de padrões.--


mdc:: (Int,Int )->Int 
mdc (m,0)=m
mdc(m,n)=mdc(n,mod m n )

mdc_guarda:: (Int,Int )->Int
mdc_guarda (m,n)| n==0 =m
                |otherwise = mdc(n,mod m n )

-- 10) Implemente a função binomial usando a definição recursiva vista em sala:--
--binomial (n,k) = 1, se k = 0--
--binomial (n,k) = 1, se k = n--
--binomial (n,k) = binomial (n-1,k) + binomial (n-1,k-1), se 0 < k < n--
--Observe que binomial (n,k) não é definido se k>n.--
--Obs: Fazer uma definição usando guardas e outra com casamento de padrões.--
           
binomial_casamento::(Int,Int)->Int 
binomial_casamento(n,0)=1
binomial_casamento(n,k)=if(k==n)
  then 1
  else binomial_casamento(n-1,k)+binomial_casamento(n-1,k-1)


binomial_guardas::(Int,Int)->Int 

binomial_guardas(n,k)|k==0=1
                     |k==n=1
                     |otherwise =binomial_guardas(n-1,k)+binomial_guardas(n-1,k-1)

                     --11) ** Exercício de maior complexidade da lista**
--Faça uma segunda definição da função recursiva fibo2 que retorna o n-ésimo termo da--
--sequência de Fibonacci utilizando recursividade e os conceitos a seguir (dica: defina a--
--função passo(x,y)).--
--a) Defina um par na sequência de Fibonacci como (n,n+1).--
--Exemplos: (1,1), (3,5), (55,89), (233,377)--
--b) Dois pares consecutivos na sequência podem ser considerados como um passo:--
--(x,y) => (y, x+y). Exemplos: (1,1) => (1,2); (3,5) => (5,8); (55,89) => (89, 144)--
--c) A partir do par inicial (1,1), podemos definir o enésimo par, como a aplicação--
--consecutiva de n passos:--
--(1,1) => (1,2) => (2,3) => (3,5) => (5,8) => (8,13) => (13,21) => (21,34) => (34,55) =>...--
--d) O n-ésimo termo (para n>0) é o primeiro elemento do enésimo par.--
--Ex: quarto par: (3,5) e quarto termo: 3 e décimo par: (55,89) e décimo termo: 55--
passo::(Integer ,Integer )->(Integer ,Integer )
passo(x,y)=(x,x+y)


fibbo2::Integer ->(Integer,Integer)
fibbo2(1)= passo(1,1)
fibbo2(n)= passo(x,y)
           where (x,y)= fibbo2(n-1)




lista :: (Eq a, Num a) => a -> [(Integer, Integer)]
lista(1)= [(1,1)]
lista(n)=  (passo(x,y)):(x,y) :xs
           where  ((x,y):xs)= lista(n-1)

  
  
  
inverte_lista :: (Eq a, Num a) => a -> [(Integer, Integer)]
inverte_lista n=reverse (lista n)