algoritmo "PolinomiosRaizesEProdutos"

var
   pol: vetor[1..7,0..6] de real
   gr: vetor[1..7] de inteiro
   nome: vetor[1..7] de caractere

   w: vetor[0..6] de real
   nv: vetor[0..6] de real

   gw: inteiro
   achou: logico

   lim: inteiro
   indice: inteiro
   cc: inteiro

// ------------------------------------------------------------
// Multiplica o polinomio a pelo polinomio b e guarda em r.
// ------------------------------------------------------------
procedimento multiplica(a, b, r: inteiro)
var
   i, j: inteiro
inicio
   para i de 0 ate 6 faca
      pol[r,i] <- 0
   fimpara

   para i de 0 ate gr[a] faca
      para j de 0 ate gr[b] faca
         pol[r,i+j] <- pol[r,i+j] + pol[a,i] * pol[b,j]
      fimpara
   fimpara

   gr[r] <- gr[a] + gr[b]
fimprocedimento

// ------------------------------------------------------------
// Escreve o polinomio na tela.
// ------------------------------------------------------------
procedimento mostraPol(numPol: inteiro)
var
   i: inteiro
   c: real
   primeiro: logico
inicio
   primeiro <- verdadeiro

   para i de gr[numPol] ate 0 passo -1 faca
      c <- pol[numPol,i]

      se c <> 0 entao

         se primeiro entao
            se c < 0 entao
               escreva("-")
            fimse
         senao
            se c < 0 entao
               escreva(" - ")
            senao
               escreva(" + ")
            fimse
         fimse

         se (abs(c) <> 1) ou (i = 0) entao
            escreva(abs(c))
            se i > 0 entao
               escreva("*")
            fimse
         fimse

         se i >= 1 entao
            escreva("x")
            se i > 1 entao
               escreva("^", i)
            fimse
         fimse

         primeiro <- falso
      fimse
   fimpara

   escreval("")
fimprocedimento

// ------------------------------------------------------------
// Tenta dividir o polinomio w por (x - c).
// ------------------------------------------------------------
procedimento tentaDividir(c: inteiro)
var
   i: inteiro
   resto: real
inicio
   nv[gw-1] <- w[gw]

   para i de gw-1 ate 1 passo -1 faca
      nv[i-1] <- w[i] + c * nv[i]
   fimpara

   resto <- w[0] + c * nv[0]

   se abs(resto) < 0.000001 entao
      para i de 0 ate gw-1 faca
         w[i] <- nv[i]
      fimpara

      w[gw] <- 0
      gw <- gw - 1
      achou <- verdadeiro
   senao
      achou <- falso
   fimse
fimprocedimento

// ------------------------------------------------------------
// Encontra e escreve as raizes do polinomio.
// ------------------------------------------------------------
procedimento calculaRaizes(numPol: inteiro)
var
   cand, i, cont, cr: inteiro
   a, b, c2, delta, sd, x1, x2, tmp: real
inicio
   gw <- gr[numPol]

   para i de 0 ate 6 faca
      w[i] <- pol[numPol,i]
   fimpara

   cont <- 0

   // PASSO 1: procura raizes inteiras
   cand <- -lim

   enquanto (cand <= lim) e (gw > 2) faca
      cr <- cand
      achou <- verdadeiro

      enquanto achou e (gw > 2) faca
         tentaDividir(cr)

         se achou entao
            cont <- cont + 1
            escreval("   x", cont, " = ", cr)
         fimse
      fimenquanto

      cand <- cand + 1
   fimenquanto

   // PASSO 2: resolve o polinomio restante
   se gw > 2 entao
      escreval("   Sobrou um polinomio de grau ", gw, " sem raizes inteiras.")

   senao
      se gw = 2 entao

         a <- w[2]
         b <- w[1]
         c2 <- w[0]

         delta <- b*b - 4*a*c2

         se abs(delta) < 0.000000001 entao

            x1 <- (-b) / (2*a)

            cont <- cont + 1
            escreval("   x", cont, " = ", x1, "  (raiz dupla)")

            cont <- cont + 1
            escreval("   x", cont, " = ", x1, "  (raiz dupla)")

         senao
            se delta > 0 entao

               sd <- raizq(delta)

               x1 <- (-b - sd) / (2*a)
               x2 <- (-b + sd) / (2*a)

               se x1 > x2 entao
                  tmp <- x1
                  x1 <- x2
                  x2 <- tmp
               fimse

               cont <- cont + 1
               escreval("   x", cont, " = ", x1)

               cont <- cont + 1
               escreval("   x", cont, " = ", x2)

            senao

               x1 <- (-b) / (2*a)
               x2 <- raizq(-delta) / (2*a)

               escreval("   Sem raizes reais restantes. Raizes complexas: ", x1, " +- ", abs(x2), "i")

            fimse
         fimse

      senao
         se gw = 1 entao
            cont <- cont + 1
            escreval("   x", cont, " = ", (-w[0]) / w[1])
         fimse
      fimse
   fimse
fimprocedimento

// ============================================================
// PROGRAMA PRINCIPAL
// ============================================================
inicio

   lim <- 100

   nome[1] <- "p"
   nome[2] <- "q"
   nome[3] <- "r"
   nome[4] <- "s"
   nome[5] <- "f"
   nome[6] <- "g"
   nome[7] <- "h"

   // Zera todos os coeficientes
   para indice de 1 ate 7 faca
      para cc de 0 ate 6 faca
         pol[indice,cc] <- 0
      fimpara
   fimpara

   // p(x) = x^2 + 3x - 10
   gr[1] <- 2
   pol[1,2] <- 1
   pol[1,1] <- 3
   pol[1,0] <- -10

   // q(x) = x^3 - 3x^2 - 6x + 8
   gr[2] <- 3
   pol[2,3] <- 1
   pol[2,2] <- -3
   pol[2,1] <- -6
   pol[2,0] <- 8

   // r(x) = 2x^3 - 10x^2 + 6x + 18
   gr[3] <- 3
   pol[3,3] <- 2
   pol[3,2] <- -10
   pol[3,1] <- 6
   pol[3,0] <- 18

   // s(x) = x^4 - 10x^2 + 9
   gr[4] <- 4
   pol[4,4] <- 1
   pol[4,2] <- -10
   pol[4,0] <- 9

   // Produtos pedidos
   multiplica(1, 2, 5)
   multiplica(1, 3, 6)
   multiplica(1, 4, 7)

   // ---------- a) raizes de p, q, r, s ----------
   escreval("=== a) RAIZES DE CADA POLINOMIO ===")

   para indice de 1 ate 4 faca
      escreval("")
      escreva(nome[indice], "(x) = ")
      mostraPol(indice)
      calculaRaizes(indice)
   fimpara

   // ---------- b) f = p . q ----------
   escreval("")
   escreval("=== b) f = p . q ===")
   escreva("f(x) = ")
   mostraPol(5)
   calculaRaizes(5)

   // ---------- c) g = p . r ----------
   escreval("")
   escreval("=== c) g = p . r ===")
   escreva("g(x) = ")
   mostraPol(6)
   calculaRaizes(6)

   // ---------- d) h = p . s ----------
   escreval("")
   escreval("=== d) h = p . s ===")
   escreva("h(x) = ")
   mostraPol(7)
   calculaRaizes(7)

fimalgoritmo